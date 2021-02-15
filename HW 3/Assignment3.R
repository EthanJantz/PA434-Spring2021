### ------------
### Assignment 3
###
library(tidyverse)

migration <- read_csv("HW 3/MigrationFlows.csv")
origin <- read_csv("HW 3/Origin.csv")
pop <- read_csv("HW 3/Population.csv")
refugees <- read_csv("HW 3/Refugees.csv")

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

write_csv(full_data, "full_data.csv")
write_csv(countries, "common_obs.csv")

### -----------
### Task 2
### questions

# 5. avg migrants/total population in 2015
# Appending this ratio to the full data 
full_data$mig_pop_ratio <- full_data$mig_total / full_data$pop_total
# Creating a new object
data_2015 <- full_data[which(full_data$year == 2015),]
# Finding the average of mig_pop_ratio for the year 2015
mean(data_2015$mig_pop_ratio) # 133.3353... This doesn't seem right
# I went back through and the only error I can find is that I probably misunderstood
# what these variables represent. Let me know if I made a silly mistake in feedback 
# please!

# 6. Refugee / Total Migrants trends from 1990
full_data$ref_mig_ratio <- full_data$ref_total / full_data$mig_total
mean(full_data[which(full_data$year == 1990),]$ref_mig_ratio, na.rm = TRUE) # 15.44% in 1990
mean(full_data[which(full_data$year == 2015),]$ref_mig_ratio, na.rm = TRUE) # 15.15% in 2015
# Very slight decrease in refugee percentages since 1990

# 7. Highest/Lowest % of immigrants in a country in 2010
max(full_data[which(full_data$year == 2010),]$mig_pop_ratio) # 878... same issue as question 5
min(full_data[which(full_data$year == 2010),]$mig_pop_ratio) # 63% which is a more reasonable number

# 8. Median percentage of immigrants in a country in 2015
median(full_data[which(full_data$year == 2015),]$mig_pop_ratio) # 54.4...

# I'm sure I made some error in this homework but I don't know what I did that led to such strange numbers.
migration %>%
  filter(Country == "Burundi") %>%
  select(Tot_2015) # 286810 migrants in Burundi in 2015

pop %>%
  filter(Country == "Burundi", year == 2015) # Total population of 11179, which is impossible if I understand the data right

### ----------
### Update
###

# I understand now that the population values were in the 100k unit. This means the values were off by a factor of 100,000.
