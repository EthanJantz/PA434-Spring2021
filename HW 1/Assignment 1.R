### ----------------
### Setup
###

# Packages
library(tidyverse)
library(patchwork)
library(RColorBrewer)

# Data
exp_activities <- c("Write the album",
                    "Yoga",
                    "Road trip",
                    "Spend time outside",
                    "Learn a new hobby",
                    "Rest")

reality_activities <- c( "Legal weed",
                         "Explore different sitting positions at my desk",
                         "Cook",
                         "Zoom calls",
                         "Exercise")

exp_timespent <- c(20, 
                   15,
                   15,
                   25,
                   45,
                   5)

reality_timespent <- c(10,
                       25,
                       15,
                       40,
                       10)

# Coerce to dataframe
lockdown_exp <- as.data.frame(cbind(exp_activities, exp_timespent)) %>%
  # Create expectation/reality variable
  mutate(exp_real = "expectation") %>%
  # Reorder and rename variables to match b/w data
  select(
    "activities" = exp_activities,
    "time" = exp_timespent,
    exp_real
  )

lockdown_reality <- as.data.frame(cbind(reality_activities, reality_timespent)) %>%
  # Create expectation/reality variable
  mutate(exp_real = "reality") %>%
  # Reorder and rename variables to match b/w data
  select(
    "activities" = reality_activities,
    "time" = reality_timespent,
    exp_real
  )

# Put it all together
lockdown_time <- bind_rows(lockdown_exp, lockdown_reality)

### ----------
### Plotting
###

expectations <- lockdown_time %>%
  filter(exp_real == "expectation") %>%
  # Create plotting object using data
  ggplot(aes(x = "", y = time, fill = activities)) +
  # Create dividers between chunks in pie chart
  geom_bar(stat = "identity", color = "white", size = 4) +
  # Assign polar coordinates to plot
  coord_polar(theta = "y", start = 0) +
  # Nothing fancy around the chart
  theme_void() +
  # Legend underneath the chart
  theme(legend.position = "bottom", legend.title = element_blank(), legend.direction = "vertical",
        plot.title = element_text(hjust = .5, size = 18)) +
  # Colors
  scale_fill_brewer(palette = "Set1") +
  # Title
  ggtitle("Expectations")

reality <- lockdown_time %>%
  filter(exp_real == "reality") %>%
  # Create plotting object using data
  ggplot(aes(x = "", y = time, fill = activities)) +
  # Create dividers between chunks in pie chart
  geom_bar(stat = "identity", color = "white", size = 4) +
  # Assign polar coordinates to plot
  coord_polar(theta = "y", start = 0) +
  # Nothing fancy around the chart
  theme_void() +
  # Legend underneath the chart
  theme(legend.position = "bottom", legend.title = element_blank(), legend.direction = "vertical",
        plot.title = element_text(hjust = .5, size = 18)) +
  # There is no Dark1
  scale_fill_brewer(palette = "Dark2") +
  # Title
  labs(title = "Reality")

# Side by side plot using patchwork library
side_by_side <- expectations + reality

full_plot <- side_by_side + 
  # Adding title using patchwork library
  plot_annotation(
    title = "My Lockdown in Review",
    theme = theme(plot.title = element_text(size = 32, hjust = .5, face = "bold")))

### -------
### Save
###
ggsave("Assignment 1.jpeg", full_plot)










