library(tidyverse)
library(lubridate)
### This script is for data cleaning

crime_2010 <- read_csv("Data/Crimes_2010.csv") %>%
  mutate(
    across(`Primary Type`:`Location Description`, as.factor))
crime_2019 <- read_csv("Data/Crimes_2019.csv") %>%
  mutate(across(`Primary Type`:`Location Description`, as.factor))
# income <- read_csv("Data/Ward_income.csv")
# demo <- read_csv("Data/WardPop_2021.csv")

# Violent + Prop Crime Descriptions
violent = c("ASSAULT", "BATTERY", "CRIM SEXUAL ASSAULT", "CRIMINAL SEXUAL ASSAULT", "HOMICIDE", "HUMAN TRAFFICKING", "KIDNAPPING", "SEX OFFENSE")
property = c("ARSON", "THEFT", "MOTOR VEHICLE THEFT", "BURGLARY", "ROBBERY", "CRIMINAL DAMAGE")

### -----------------
### research question
###

# When does violent crime happen? 
# When does property crime happen?
# Do certain types of locations have associations with different types of crimes?

### -------------------
### Crime data cleaning
###
# Final dataset should be one object, crime. It should combine 2010 and 2019 data 
# Columns should be: 
# month, primary type, description, location, arrest, domestic, violent, property

clean_crime_2010 <- crime_2010 %>%
  mutate(Date = mdy_hm(Date), # Convert character type date to date type
         month = month(Date), # Pull month
         Arrest = as.logical(Arrest), # Ensure logical value is logical for combining data
         Domestic = as.logical(Domestic),
         type = as.factor(case_when(`Primary Type` %in% violent ~ "violent", # Classify crime as violent/property/else
                          `Primary Type` %in% property ~ "property",
                          TRUE ~ "other"))) %>%
  # These are the only values we want to look at
  # renames values for ease of use later
  select(Date, 
         month, 
         primary_type = `Primary Type`, 
         desc = Description, 
         location = `Location Description`, 
         arrest = Arrest, 
         domestic = Domestic, 
         type)

# In a previous version of this script the data was filtered to only include
# dates in 2019, because the 2019 data included 2020 data
# However, on closer inspection, there was an issue in data collection that 
# meant that crime_2019 was data from April 2019 until April 2020. 
# To capture seasonality we want to include Jan - April in our data
# So while it's not perfect (since the pandemic effects are included)
# I think it is important to ensure a full year of data is captured for 2010-2019 comparisons - EJ
clean_crime_2019 <- crime_2019 %>% 
  mutate(Date = mdy_hm(Date),
         month = month(Date), 
         Arrest = as.logical(ifelse(Arrest == "Y", TRUE, FALSE)),
         Domestic = as.logical(ifelse(Domestic == "Y", TRUE, FALSE)),
         type = as.factor(case_when(`Primary Type` %in% violent ~ "violent",
                          `Primary Type` %in% property ~ "property",
                          TRUE ~ "other"))) %>%
  select(Date, 
         month, 
         primary_type = `Primary Type`, 
         desc = Description, 
         location = `Location Description`, 
         arrest = Arrest, 
         domestic = Domestic, 
         type)

crime <- bind_rows(clean_crime_2010, clean_crime_2019) # Combine data into one big set
rm(crime_2010, crime_2019, clean_crime_2010, clean_crime_2019) # we only really want the final cleaned dataset to work with, so I'm removing the transient data objects