# Driveline Baseball Hitting Project

# The goal of this project is to quantify how well energy transfers up the kinematic chain.

library(tidyverse)  # for cleaning and piping
library(ggplot2)    # for plotting

# Load data
data <- read_csv("C:/Users/Katrina Peralta/Downloads/Driveline Baseball/Hitting Data/poi_metrics.csv")

# Define key metrics
energy_data <- data %>%
  select(session_swing, session, exit_velo_mph_x, pelvis_angular_velocity_maxhss_x,
         torso_angular_velocity_maxhss_x, upper_arm_speed_mag_max_x,hand_speed_mag_max_x, 
         bat_speed_mph_max_x)
         
# Efficiency defined as next segment peak / previous segment peak
# Find efficiency
energy_data <- energy_data %>%
  mutate(
    pelvis_torso = torso_angular_velocity_maxhss_x / pelvis_angular_velocity_maxhss_x,
    torso_arm = upper_arm_speed_mag_max_x / torso_angular_velocity_maxhss_x,
    arm_hand = hand_speed_mag_max_x / upper_arm_speed_mag_max_x,
    hand_bat = bat_speed_mph_max_x / hand_speed_mag_max_x,
    total_efficiency = (pelvis_torso * torso_arm * arm_hand * hand_bat)^(1/4)
  )

# Fix NAs and replace with 0
energy_data <- energy_data %>%
  mutate(
    pelvis_torso = replace_na(pelvis_torso, 0),
    torso_arm = replace_na(torso_arm, 0),
    arm_hand = replace_na(arm_hand, 0),
    hand_bat = replace_na(hand_bat, 0),
    total_efficiency = replace_na(total_efficiency, 0)
  )

# Define thresholds for top & bottom 10%
top_thresh = quantile(energy_data$total_efficiency, 0.9, na.rm = TRUE)
bottom_thresh = quantile(energy_data$total_efficiency, 0.1, na.rm = TRUE)

# Summary stats
summary(energy_data)

# Plotting energy transfer efficiency vs exit velo
ggplot(energy_data, aes(x=total_efficiency, y=exit_velo_mph_x)) +
  geom_point(alpha=0.7) + 
  geom_vline(xintercept = c(bottom_thresh, top_thresh), linetype = 'dashed', color='red') + 
  geom_smooth(method='lm', se = TRUE, color='blue', fill='lightblue') +  # trendline & CI
  labs(x = 'Total Energy Transfer Efficiency',
       y = 'Exit Velocity (mph)',
       title = 'Energy Transfer Efficiency vs Exit Velocity',
       color = 'Hitter Session'
       ) +
  theme(legend.position = 'right')


### Analysis:
# Each dot is a swing representing how efficiently energy transfers up the kinematic
# chain and its relative exit velocity. The overall trend has a positive slope meaning
# more efficient energy transfer generally correlates with higher exit velocity. However,
# this relationship looks weak due to the small slope. Other factors may include weak timing,
# point of contact, or poor sequencing with upper body compensation. The red dashed lines
# represent the bottom and top 10% thresholds for energy transfer where inefficient swings 
# are left of the first red line and elite energy transfer swings are right of the second 
# red line. Overall, a smoother energy transfer corresponds to higher exit velocities.
# This analysis emphasizes the importance of implementing this sequencing in training 
# sessions such as hip-shoulder separation. 


