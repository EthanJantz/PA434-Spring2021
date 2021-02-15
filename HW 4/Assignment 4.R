### ------------
### setup
###
library(dplyr)
library(readr)

immigration <- read_csv(here::here("HW 4", "ImmigrationData.csv"))
region <- read_csv(here::here("HW 4", "RegionData.csv"))

### ------------
### merge
### keep all obs in immigration

# This should be straightforward
# Total expected obs in full_data is 1392
full_data <- immigration %>%
  left_join(region, by = c("CountryCode" = "countrycode")) %>%
  relocate(region, .before = year)

dim(full_data) # 1392 obs

### -----------
### analysis
###

# Question 1 
# filter all obs from countries in Afr in 1990 and count obs

full_data %>%
  filter(region == "Africa", year == 1990) # 57 rows

# Question 2 
# Number of countries with 1 - 2 million female migrants

full_data %>%
  filter(FemaleMigrants %in% 1000000:2000000 & year == 2015) %>%
  distinct(Country) # 15

# Question 3 
# Countries in Africa + Oceania

full_data %>%
  filter(region %in% c("Africa", "Oceania")) %>%
  distinct(Country) # 57 countries

# Question 4 
# Which developed country in 2010 had the most migrants

full_data %>%
  filter(region == "Developed countries", year == 2010) %>%
  select(Country, Migrants) %>%
  arrange(desc(Migrants)) # The United States of America

# Question 5 
# Reorganize dataset so all immigrant population data comes after the year

full_data %>%
  relocate(Migrants:Refugees, .after = year)

# Question 6
# Filter for the specified countries

full_data %>%
  filter(Country %in% c("Italy", "Germany", "Spain", "France", "Portugal", "Greece"), 
         year == 2015)
rm(immigration, region)
