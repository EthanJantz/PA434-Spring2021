### ------------
### Assignment 3
###
library(tidyverse)

migration <- read_csv("MigrationFlows.csv")
origin <- read_csv("Origin.csv")
pop <- read_csv("Population.csv")
refugees <- read_csv("Refugees.csv")

names(migration)
glimpse(migration)
# "tot_*" refers to total migrant stock in this year
# and "gender_*" is a breakdown of that total
# tidy colnames would be country, code, year, mig_total, mig_male, mig_female

names(origin)
glimpse(origin)
# origin needs year variables joined into one number
# "tot_*" refers to total migrant stock from *
# tidy colnames would be country, code, year, mig_africa, 
# mig_asia, mig_oceania, mig_latam, mig_developed

names(pop)
glimpse(pop)
# i think "Poptype" refers to the total population
# tidy colnames would be country, code, year, pop_total, pop_male, pop_female

names(refugees)
glimpse(refugees)
# "refugee_*" refers to refugee stock from year *
# tidy colnames would be country, code, year, ref_total

# https://www.un.org/en/development/desa/population/migration/data/estimates2/data/UN_MigrantStockTotal_2019.xlsx

### ---------
### Task 1
### tidy the data
# tidy common cols: country, code, year
common_cols <- c("country", "code")
# and the rest of the datasets include pop_total, pop_male, pop_female, mig_total, mig_male,
# mig_female, mig_africa, mig_asia, mig_oceania, mig_latam, mig_developed, ref_total

origin_tidy <- origin %>%
  mutate( 
    country = country_name,
    year = as.numeric(paste0(century,decade,year)),
    mig_africa = tot_africa,
    mig_asia = tot_asia,
    mig_oceania = tot_oceania,
    mig_latam = tot_latam,
    mig_developed = tot_developed,
    .keep =  "unused", # retains only the variables I didn't use from .data
  )

pop_tidy <- pop %>%
  pivot_wider(
    names_from = Poptype,
    values_from = population
  ) %>%
  mutate(
    country = Country,
    code = `Country code`,
    pop_total = Pop,
    pop_male = MalePop,
    pop_female = FemalePop,
    .keep = "unused"
  )

refugees_tidy <- refugees %>%
  pivot_longer(
    cols = REFUGEES_1990:REFUGEES_2015,
    names_to = "year",
    # $ anchors to the end of the string
    names_pattern = "([[:digit:]]*)$",
    values_to = "ref_total"
  ) %>%
  mutate(
    country = Country,
    code = `Country code`,
    year = as.numeric(year),
    .keep = "unused"
  )

migration_tidy <- migration %>%
  pivot_longer(
    cols = Tot_1990:Female_2015,
    # naming the new columns being created 
    names_to = c(".value", "year"),
    # regex to pull the data from the column names
    # "^" means to read from the first character
    # "([[:alpha:]]*)" the parenthenses are in order of column names
    # [[:alpha:]] matches all letters, * makes it "greedy" so that it selects all of the following letters
    # "_" is outside of parenthenses and not selected for variable names
    # "([[:digit:]]*)" does the same as the alpha group but for numbers
    names_pattern = "^([[:alpha:]]*)_([[:digit:]]*)"
  ) %>%
  # fix the names and make sure year is a number
  mutate(
    country = Country,
    code = `Country code`,
    year = as.numeric(year),
    mig_total = Tot,
    mig_male = Male,
    mig_female = Female,
    .keep = "unused"
  )

# Take a look before joining
names(migration_tidy) # country, code, year, mig_total, mig_male, mig_female
names(origin_tidy) # country, code, year, mig_africa, mig_asia, mig_oceania, mig_latam, mig_developed
names(pop_tidy) # country, code, year, pop_total, pop_male, pop_female
names(refugees_tidy) # country, code, year, ref_total

# Using list() %>% reduce(left_join) to combine all data
full_data <- list(migration_tidy, origin_tidy, pop_tidy, refugees_tidy) %>%
  reduce(left_join, by = c(common_cols, "year"))

countries <- full_data  %>%
  select(all_of(common_cols)) %>%
  # remove duplicate entries
  distinct()


### questions
# 1. The unit of analysis here is the country-year

# 2. Primary issues were inconsistent naming schemes
# I tidied them by lengthening/widening the data to ensure the following common columns: country, code, year, gender
# This ensured that joining all datasets was possible and observations were consistent between datasets

# 3. There are 6 years and 232 countries, multiply for 1392 rows for full_data.

# 4. 
dim(full_data) # 1392 obs of 15 vars
dim(countries) # 232 obs of 2 vars


### -----------
### Task 2
### questions

# 5. avg migrants/total population in 2015
