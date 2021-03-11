library(tidyverse)
library(patchwork) # For creating the final submission

data <- tidytuesdayR::tt_load('2021-01-05')$transit_cost

summary(data)

data_clean <- data %>%
  select(-reference, "midpoint_year" = year) %>% # we don't really need the URL source in our viz
  mutate(
    across(c(country, city, line, currency, source1, source2), ~ as.factor(.x)), # convert these to factors for ease of analysis
    across(c(start_year, end_year), ~ lubridate::as_date(paste(.x,"1","1", sep = "-"))), # convert year values to date class for ease of analysis
    across(c(tunnel_per, real_cost), ~ as.numeric(gsub("%", "", .x))), # convert character class numbers into numeric class
    rr = as.logical(rr)
  )%>%
  pivot_longer(cols = start_year:end_year,
               names_to = "state",
               values_to = "year") 

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

# After some research I found a type of timeline that I think will work for this application
# called a Gantt chart. I'll be sort-of following this tutorial:
# https://www.molecularecologist.com/2019/01/03/simple-gantt-charts-in-r-with-ggplot2-and-the-tidyverse/

# First I'll filter to just one country, in this case Japan, and gather the 
# dates so that the data has a date column and a state column

data_jp <- data_clean %>%
  filter(country == "JP")

timeline_jp <- data_jp %>%
  ggplot() +
  geom_line(aes(x = year, y = line, color = length, group = e), size = 10) + # The plot looks like a bar chart, but they're actually lines!
  # It's probably possible to change size to a dynamic element based on the number of lines, but i'm not doing that today
  geom_vline(xintercept = as.numeric(Sys.Date()), linetype = "dotted") + # I want the viewer to be able to tell which projects are anticipated/active
  labs(title = "Rail-Based Transit Projects Over Time",
       x = "Year",
       y = "Transit Project",
       color = "Length of Project (km)") +
  theme(legend.position = "bottom") +
  scale_color_binned() # I think binned makes the plot less confusing, though it does obscure nuance

timeline_jp

# Now I can functionalize this and create the same plot for any country or city
timeline_plot <- function(selected) {
  data_p <- data_clean %>%
    filter(country %in% selected | city %in% selected) # User can subimt a city or country code or combination thereof
  
  timeline_p <- data_p %>%
    ggplot() +
    geom_line(aes(x = year, y = line, color = length, group = e), size = 10) + 
    geom_vline(xintercept = as.numeric(Sys.Date()), linetype = "dotted") +
    labs(title = "Rail-Based Transit Projects Over Time",
         subtitle = selected,
         x = "Year",
         y = "Transit Project",
         color = "Length of Project (km)") +
    theme(legend.position = "bottom") +
    scale_color_binned()
  
  timeline_p
}

# Test it out on France
timeline_plot("FR")

# This works, but if I tried it with CN it would be a mess
timeline_plot("CN") # yes, see?
# So I altered the function to select either city or country
timeline_plot("Shanghai")

# Mumbai, India
timeline_plot("Mumbai")
timeline_plot("IN") # It's still messy for countries with lots of transit projects

# Final submission will be a comparison of cities in countries with a lot of transit projects
data_clean %>%
  group_by(country, city) %>%
  count() %>%
  arrange(desc(n)) # Shanghai, Istanbul, Mumbai because i don't want to compare three cities from China

timeline_plot("Shanghai") + timeline_plot("Istanbul") + timeline_plot("Mumbai")
