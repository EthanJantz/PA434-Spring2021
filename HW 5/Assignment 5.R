### ----------
### setup
### 

# packages
library(tidyverse)

# sourcing data by running the code from assignment 4
# creates full_data in environment
source(here::here("HW 4", "Assignment 4.R"))

data <- full_data
rm(full_data)

summary(data)

### ----------
### questions
###

# Q 1
# Number of migrants by region in 2015
data %>%
  filter(year == 2015) %>%
  group_by(region) %>%
  summarize(Migrants = sum(Migrants)) %>%
  arrange(desc(Migrants))

# Q 2
# Countries in 2015 where Migrants > (Pop * .5)
data %>%
  filter(Migrants > (Pop * .5), year == 2015) # 16 countries

# Q 3
# Regions with countries from Q 2 data
data %>%
  filter(Migrants > (Pop * .5), year == 2015) %>%
  count(region) %>%
  arrange(desc(n)) # Territories/Others and Asia

# Q 4
# find average of population from 1990 to 2015
# countries with immigrant pop > (that avg * .5)
data %>%
  group_by(Country) %>%
  mutate(Pop_avg = mean(Pop)) %>%
  ungroup() %>%
  filter(Migrants > (Pop_avg * .5), year == 2015)

# Q 5
# regional change in refugees form 1990 to 2015
data %>%
  filter(year %in% c(1990, 2015)) %>%
  group_by(region, year) %>%
  summarize(refugees = sum(Refugees, na.rm = T)) %>%
  mutate(refugees_change = refugees - lag(refugees)) %>%
  drop_na() %>%
  select(region, refugees_change) %>%
  arrange(desc(refugees_change)) # Asia had the largest increase and Africa the largest decrease

# Q 6
# Change in immigration by gender
data %>% 
  drop_na() %>%
  group_by(year) %>%
  summarize(male = sum(MaleMigrants) / sum(Migrants),
            female = sum(FemaleMigrants) / sum(Migrants)) # slight proportional shift toward male migrants since 1990

# Country with lowest female immigration
data %>%
  drop_na() %>%
  group_by(Country) %>%
  summarize(female = mean(FemaleMigrants)) %>%
  filter(female == min(female)) # Vanuatu has the fewest average female immigrants between 1990 and 2015

# Region with highest female immigration
data %>%
  drop_na() %>%
  group_by(region) %>%
  summarize(female = mean(FemaleMigrants)) %>%
  filter(female == max(female)) # Developed countries had the most female migrants on average between 1990 and 2015

### --------------
### missing values
###

# Q 1
# calculate missing values in refugees, pop, and migrant columns
