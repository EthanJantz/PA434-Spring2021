library(tidyverse)
library(patchwork) # lets me do both plots on the same exported file
# Exploratory Data Analysis

# # Crime seasonality by type
# crime %>%
#   group_by(month, type) %>%
#   count() %>%
#   ggplot(aes(x = month, y = n, color = type)) +
#     geom_line()
# 
# 
# crime %>% 
#   group_by(month(Date), location, type) %>%
#   count() %>%
#   ggplot(aes(x = `month(Date)`, y = n, fill = location)) +
#   geom_col() # too many locations
# 
# crime %>%
#   count(location) %>%
#   arrange(desc(n)) %>%
#   view()
# 
# # maybe i can explore vehicle-related crimes
# crime %>%
#   count(location) %>%
#   filter(grepl("vehicle", location, ignore.case = TRUE)) %>%# grepl checks strings for the presence of a pattern, "vehicle" in this case, in a variable
#   view()
# 
# # okay, let's see what kind of crime happens in rideshares
# crime %>%
#   filter(grepl("ride share", location, ignore.case = T)) %>%
#   group_by(primary_type) %>%
#   count() %>%
#   view() # top 3 are theft, deceptive practice, and battery
# 
# # I think it may be possibile to link these ride share crimes with the actual trips in the Chicago TNP dataset... Maybe another day...
# # For now let's start exploring ride share crime more deeply
# rideshare_crime <- crime %>%
#   filter(grepl("ride share", location, ignore.case = T)) # 161 crimes b/w 2010 and 2019 data occured in ride shares
# 
# rideshare_crime %>%
#   summary()
# 
# # Of note: no ride share crimes in 2010, only 9 arrests, 5 domestic crimes (all battery), only 8 criminal damage (7 to vehicle)
# 
# # plotting thefts
# rideshare_crime %>%
#   filter(primary_type == "THEFT") %>%
#   group_by(month(Date), desc) %>%
#   count() %>%
#   filter(desc != "POCKET-PICKING") %>%
#   ggplot(aes(x = `month(Date)`, y = n)) +
#   geom_col(aes(fill = desc), position = "dodge") # is there seasonality in rideshare crimes? hard to tell, since data prior to April 2019 isn't included in the data. Data for Q1 is for 2020 and not 2019

# What about comparing ride share crimes with commercial vehicle crimes?
vehicle_comparison_crime <- crime %>%
  filter(grepl("ride share|vehicle-commercial|vehicle - commercial|non-commercial", location, ignore.case = T)) %>%
  mutate(location = case_when(
    grepl("vehicle-commercial|vehicle - commercial|delivery", location, ignore.case = T) ~ "COMMERCIAL", # there are whitespace differences that i'm trying to account for here
    grepl("ride share", location, ignore.case = T) ~ "RIDESHARE",
    grepl("non-commercial", location, ignore.case = T) ~ "PERSONAL"
  ))

vehicle_comparison_crime %>%
  count(primary_type, location) %>%
  arrange(desc(n)) # Theft is the most prevalent crime committed in a vehicle

# Exploring arrests
vehicle_comparison_crime %>%
  group_by(month(Date), location, arrest) %>%
  count() %>%
  ggplot(aes(x = `month(Date)`, y = n)) +
  geom_col(aes(fill = arrest), position = "dodge") # interesting, the proportion of arrests goes down as the number of crimes increases

# Back to the research question: Does the location of a crime have associations with different types of crimes?
# Is there a difference between the crimes that happen iin commercial/rideshare vehicles compared with non-commercial/personal vehicles?

vehicle_type_plot <- vehicle_comparison_crime %>%
  mutate(personal_vehicle = ifelse(location == "PERSONAL", T, F)) %>%
  group_by(personal_vehicle, type) %>%
  count() %>%
  group_by(personal_vehicle) %>%
  mutate(freq = n / sum(n)) %>% # creates a frequency table
  ungroup() %>%
  mutate(personal_vehicle = ifelse(personal_vehicle, "Personal", "Commercial/Rideshare")) %>%
  ggplot(aes(x = personal_vehicle, y = freq)) +
  geom_col(aes(fill = str_to_title(type)), position = "dodge") +
  geom_text(aes(label = n, group = type), colour = "white", size = 3,
            vjust = 1.25, hjust = "center", position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
  labs(title = "Proportion of Types of Crime in Vehicles",
       subtitle = "by Use of Vehicle",
       caption = "Data from Chicago Open Data Portal, 2010 and 2019",
       x = "Vehicle Use",
       y = "Percent",
       fill = "Type of Crime") # It looks like the answer is yes. Let's try and take this a step further

vehicle_type_plot

# Plotting the breakdown of other crimes by personal/commercial use
other_plot <- vehicle_comparison_crime %>%
  mutate(personal_vehicle = ifelse(location == "PERSONAL", T, F)) %>%
  filter(type == "other") %>%
  count(personal_vehicle, primary_type) %>%
  group_by(personal_vehicle) %>%
  mutate(freq = n / sum(n)) %>% # creates a frequency table
  ungroup() %>%
  mutate(personal_vehicle = ifelse(personal_vehicle, "Personal", "Commercial/Rideshare")) %>%
  group_by(personal_vehicle) %>%
  slice_max(order_by = freq, n = 3) %>%
  ggplot(aes(x = reorder(str_to_title(primary_type), freq), y = freq, fill = personal_vehicle)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
  coord_flip() +
  geom_text(aes(label = n), colour = "white", size = 3,
            hjust = 1.25, position = position_dodge(.9)) +
  labs(title = "Description of Crime Occuring in Vehicle by Use of Vehicle",
       subtitle = "top 3 non-violent and non-property related crimes",
       caption = "Data from Chicago Open Data Portal, 2010 and 2019",
       x = "Crime Description",
       y = "Percent",
       fill = "Vehicle Use")

# It looks like when we break the data down by other crime type we see a distinct difference in the mode between personal and non-personal vehicles
# The only type of crime in the top 3 crimes for both vehicle usage is criminal damage

vehicle_type_plot / other_plot

# One thing that comes to mind after these plots and some other exploration: The police are here to protect property and are used as such more often than not