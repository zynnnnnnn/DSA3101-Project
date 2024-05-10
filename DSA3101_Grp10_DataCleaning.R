library(tidyverse)
library(zoo)

## PART 1: Joining separate csv files 

# Create seq of dates that we need from "2020-06-01" to "2020-07-05" (58 weeks)

weeks <- seq(as.Date("2020-06-01"), as.Date("2021-07-05"), by = "week")


# Reading in data and filtering the dates that we need

test <- read.csv("covid-19-testing-policy.csv") %>%
  mutate(Date = as.Date(Day, format = "%Y-%m-%d"), .before = Day) %>%
  mutate(Day = NULL) %>%
  filter(Date %in% weeks)

contact <- read.csv("covid-contact-tracing.csv") %>%
  mutate(Date = as.Date(Day, format = "%Y-%m-%d"), .before = Day) %>%
  mutate(Day = NULL) %>%
  filter(Date %in% weeks)

vacc <- read.csv("covid-vaccination-policy.csv") %>%
  mutate(Date = as.Date(Day, format = "%Y-%m-%d"), .before = Day) %>%
  mutate(Day = NULL) %>%
  filter(Date %in% weeks)

face <- read.csv("face-covering-policies-covid.csv") %>%
  mutate(Date = as.Date(Day, format = "%Y-%m-%d"), .before = Day) %>%
  mutate(Day = NULL) %>%
  filter(Date %in% weeks)

support <- read.csv("income-support-covid.csv") %>%
  mutate(Date = as.Date(Day, format = "%Y-%m-%d"), .before = Day) %>%
  mutate(Day = NULL) %>%
  filter(Date %in% weeks)

internal <- read.csv("internal-movement-covid.csv") %>%
  mutate(Date = as.Date(Day, format = "%Y-%m-%d"), .before = Day) %>%
  mutate(Day = NULL) %>%
  filter(Date %in% weeks)

travel <- read.csv("international-travel-covid.csv") %>%
  mutate(Date = as.Date(Day, format = "%Y-%m-%d"), .before = Day) %>%
  mutate(Day = NULL) %>%
  filter(Date %in% weeks)

campaign <- read.csv("public-campaigns-covid.csv") %>%
  mutate(Date = as.Date(Day, format = "%Y-%m-%d"), .before = Day) %>%
  mutate(Day = NULL) %>%
  filter(Date %in% weeks)

events <- read.csv("public-events-covid.csv") %>%
  mutate(Date = as.Date(Day, format = "%Y-%m-%d"), .before = Day) %>%
  mutate(Day = NULL) %>%
  filter(Date %in% weeks)

gathering <- read.csv("public-gathering-rules-covid.csv") %>%
  mutate(Date = as.Date(Day, format = "%Y-%m-%d"), .before = Day) %>%
  mutate(Day = NULL) %>%
  filter(Date %in% weeks)

school <- read.csv("school-closures-covid.csv") %>%
  mutate(Date = as.Date(Day, format = "%Y-%m-%d"), .before = Day) %>%
  mutate(Day = NULL) %>%
  filter(Date %in% weeks)

all <- read.csv("owid-covid-data.csv") %>%
  mutate(Date = as.Date(date, format = "%Y-%m-%d"), .before = date) %>%
  mutate(date = NULL) %>%
  filter(Date %in% weeks) %>%
  rename(Code = iso_code, Entity = location) #renaming to match those of the other datasets


# Joining the datasets together

covid_0 <- school %>%
  full_join(travel, by = c("Date", "Code", "Entity")) %>%
  full_join(gathering, by = c("Date", "Code", "Entity")) %>%
  full_join(events, by = c("Date", "Code", "Entity")) %>%
  full_join(campaign, by = c("Date", "Code", "Entity")) %>%
  full_join(internal, by = c("Date", "Code", "Entity")) %>%
  full_join(support, by = c("Date", "Code", "Entity")) %>%
  full_join(face, by = c("Date", "Code", "Entity")) %>%
  full_join(vacc, by = c("Date", "Code", "Entity")) %>%
  full_join(test, by = c("Date", "Code", "Entity")) %>%
  full_join(contact, by = c("Date", "Code", "Entity")) %>%
  inner_join(all, by = c("Date", "Code", "Entity"))


## PART 2: Data cleaning

# Filtering out entities with insufficient weeks of data

new <- covid_0 %>%
  group_by(Code) %>%
  mutate(count = n(), continent = NULL) %>% # Counting the number of weeks the entities have data for (max is 58 weeks)
  filter(count >= 50) %>% # Filter out entities with less than 50 weeks of data as they would have insufficient data for us to work with
  ungroup() %>%
  arrange(Entity)


# Columns 45-55 (Vaccination related): Setting NA values to 0 (before "2020-12-07")
# The earliest vaccination date we have in our dataset is "2020-12-07"

date <- as.Date("2020-12-07", format = "%Y-%m-%d")
temp1 <- filter(new, Date < date)
temp2 <- filter(new, Date >= date) 

for (i in c(45:55)) {
  temp1[,i] <- 0
}

temp3 <- rbind(temp1, temp2)
new_2 <- arrange(temp3, Entity, Date)


# Remove columns with >25% of NA values

i = 73
while (i > 0) {
  len <- length(new_2[[i]])
  num_blanks <- length(which(is.na(new_2[i])))
  frac <- num_blanks/len
  if (frac > 0.25) {
    new_2[i] <- NULL
  }
  i = i -1
}

new_2$tests_units <- NULL # Not a useful variable as it does not contain much information
new_2$count <- NULL # We can drop the count column since we no longer need it


# Creating function to check how many NA values each column has

na_columns <- function(x) {
  for (j in c(1:length(x))) {
    num_blanks <- length(which(is.na(x[j])))
    print(paste0(j, ":", num_blanks))
  }
}

na_columns(new_2) 


# Columns 4-14 (categorical variables) : for NA values, take previous value
# These columns contain policy responses data and we assume that these values will not change greatly within a week.

for (i in c(3:14)) {
  new_2[i] <- na.locf(new_2[i])
}

na_columns(new_2) 


# Columns 18, 19, 24, 25 (death related): Set NA to 0
# We manually checked the NA values against information online to make sure they are indeed meant to be 0

which(is.na(new_2$total_deaths))
which(is.na(new_2$new_deaths))
which(is.na(new_2$total_deaths_per_million))
which(is.na(new_2$new_deaths_per_million))

blanks <- which(is.na(new_2[18]))
for (i in c(18, 19, 24, 25)) {
  for (j in blanks) {
    new_2[[i]][j] <- 0
  }
}

na_columns(new_2) 


# Columns 28-29 (New vaccinations related) : Set NA to previous values (mostly 0)
# This can be done since these entities have yet to start their vaccinations

for (i in c(28,29)) {
  new_2[i] <- na.locf(new_2[i])
}

na_columns(new_2) 


# check columns still including NA values

nac <- new_2 %>% group_by(Entity) %>% summarize_all(.funs = list('NA' = ~sum(is.na(.))))
colSums(nac!=0)

# Delete columns with too countries having missing values
# We drop these columns as they do not contain much useful information

new_2<-new_2[,!(names(nac) %in% c("female_smokers_NA","male_smokers_NA","reproduction_rate_NA","stringency_index_NA"))]


# Dropping entities which have NA values for certain variables

t<-new_2 %>% group_by(Entity) %>%filter(all(!is.na(human_development_index)))
t<-t %>% group_by(Entity) %>%filter(all(!is.na(diabetes_prevalence)))
t<-t %>% group_by(Entity) %>%filter(all(!is.na(cardiovasc_death_rate)))
t<-t %>% group_by(Entity) %>%filter(all(!is.na(gdp_per_capita)))
t<-t %>% group_by(Entity) %>%filter(all(!is.na(aged_70_older)))
t<-t %>% group_by(Entity) %>%filter(all(!is.na(aged_65_older)))
t<-t %>% group_by(Entity) %>%filter(all(!is.na(median_age)))
t<-t %>% group_by(Entity) %>%filter(all(!is.na(population_density)))
t<-t %>% group_by(Entity) %>%filter(all(!is.na(hospital_beds_per_thousand)))
new_3<-t
                                   
 
# Dropping highly correlated columns

t<-new_3[,!(names(new_3) %in% c("total_cases","new_cases","new_cases_smoothed","total_deaths","new_deaths","new_deaths_smoothed","total_cases_per_million","new_cases_per_million","total_deaths_per_million","new_deaths_per_million", "new_vaccinations_smoothed","aged_70_older"))]


## PART 3: Creating 2 new variables

# Creating "international_travel_controls_binary", based on "international_travel_controls" where 0,1 indicates open and 2,3,4 indicates closed
# Open is represented by 1 while closed is represented by 0

new_3 <- new_3 %>% mutate(international_travel_controls_binary=ifelse(international_travel_controls<3,1,0))


# Creating "probability_open", by calculating moving average of 13 weeks based on "international_travel_control_binary"

new_3 <- new_3 %>% group_by(Entity) %>% mutate(probability_open=rollmean(international_travel_controls_binary,13,align="left",fill=NA))


