library(tidyverse)

data <- tidytuesdayR::tt_load('2021-01-05')$transit_cost

summary(data)

data_clean <- data %>%
  mutate(
    across(c(country, city, line, currency, source1, source2), ~ as.factor(.x)), # convert these to factors for ease of analysis
    across(c(start_year, end_year), ~ lubridate::as_date(paste(.x,"1","1", sep = "-"))), # convert year values to date class for ease of analysis
    across(c(tunnel_per, real_cost), ~ as.numeric(gsub("%", "", .x))), # convert character class numbers into numeric class
    rr = as.logical(rr)
  ) %>%
  select(-reference) # we don't really need the URL source in our viz

skimr::skim(data_clean)

# Which cities have the most projects
data_clean %>%
  count(city) %>%
  slice_max(n, n = 10) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(city, n), y = n)) +
  geom_col() +
  coord_flip() # Shanghai, Beijing, Wuhan... China takes up most of the top 10 in our data

# A timeline of projects would be cool, lets do one with a focus on an individual city
# but also one that can be applied to different cities
# to do that i need to build a base plotting object and then apply city-level data to it
