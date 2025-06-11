# Load necessary libraries
library(dplyr)
library(ggplot2)
library(bit64)
library(pracma)  # For rad2deg function
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

setwd("C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Processed Data")

filePath <- "C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/SICK DATA FRAMES/Vel_and_Accel_df.csv"
df <- read.csv(filePath, colClasses=c("startTime" = "integer64", "endTime" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)


# Load your data

# Reshape the data from wide to long format using pivot_longer
movement_long_df <- df %>%
  pivot_longer(
    cols = c(hand_time2Initiation, 
             handRot_time2Initiation, 
             headRot_time2Initiation, 
             gaze_time2Initiation),
    names_to = "Movement_Type",
    values_to = "initiation_time"
  ) %>%
  mutate(
    Movement_Type = recode(Movement_Type,
                           "hand_time2Initiation" = "Hand",
                           "handRot_time2Initiation" = "Hand Rotation",
                           "headRot_time2Initiation" = "Head Rotation",
                           "gaze_time2Initiation" = "Gaze")
  )

lme_model <- lmer(
  initiation_time ~ Group * Movement_Type + (1 + Movement_Type | Participant),
  data = movement_long_df,
  REML = FALSE  # Use Maximum Likelihood (ML) for better model comparison
)

# Display model summary
summary(lme_model)

plotDF <- movement_long_df %>% filter(Group != "")
plotDF <- plotDF %>% filter(!is.na(initiation_time))
plotDF <- plotDF %>% filter(initiation_time > 0)

library(lme4)
library(lmerTest)

# Fit the LME model
lme_model <- lmer(
  initiation_time ~ Group * Movement_Type + (1 + Movement_Type | Participant),
  data = movement_long_df,
  REML = FALSE
)

# View model summary
summary(lme_model)

# 
# sdInitTime <- sd(plotDF$initiation_time) *3
# plotDF <- plotDF %>% filter(initiation_time < sdInitTime)


ggplot(plotDF, aes(x = Movement_Type, y = initiation_time, fill = Group)) +
  geom_boxplot(outlier.shape = NA) + # Remove outliers for clarity
  theme_minimal(base_size = 15) +
  labs(
    title = "Initiation Time by Movement Type and Group",
    x = "Movement Type",
    y = "Initiation Time (s)"
  ) +
  ylim(0, .7)+
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.major = element_line(linetype = "dashed", color = "gray", size = 0.5)
    
  )

ggplot(plotDF, aes(x = initiation_time)) +
  geom_histogram(bins = 30) +
  facet_grid(Group ~ Movement_Type) +
  theme_minimal()

# QQ plot for normality
ggplot(plotDF, aes(sample = initiation_time)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(Group ~ Movement_Type)




# Calculate pairwise time differences for each trial
plotDF2 <- plotDF %>%
  pivot_wider(names_from = Movement_Type, values_from = initiation_time) %>%
  mutate(
    Hand_Head_Diff = Hand - `Head Rotation`,
    Hand_Gaze_Diff = Hand - Gaze,
    Head_Gaze_Diff = `Head Rotation` - Gaze
  )

ggplot(plotDF2, aes(x = Hand_Head_Diff, fill = Group)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Distribution of Hand-Head Synchrony by Group",
    x = "Hand - Head Initiation Difference (s)",
    y = "Density"
  ) +
  theme_minimal()





plotDF3 <- plotDF %>%
  pivot_wider(names_from = Movement_Type, values_from = initiation_time) %>%
  mutate(
    Hand_Head_Diff = Hand - `Head Rotation`,
    Hand_Gaze_Diff = Hand - Gaze,
    Head_Gaze_Diff = `Head Rotation` - Gaze
  )

# Convert back to long format for analysis
plotDF3 <- plotDF3 %>%
  select(Participant, Group, Hand_Head_Diff, Hand_Gaze_Diff, Head_Gaze_Diff) %>%
  pivot_longer(cols = starts_with("Hand") | starts_with("Head"),
               names_to = "Pair",
               values_to = "Time_Difference")


# Plot the distribution of synchronization differences by group
ggplot(plotDF3, aes(x = Time_Difference, fill = Group)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Pair) +
  labs(
    title = "Distribution of Movement Synchrony Differences by Group",
    x = "Time Difference (s)",
    y = "Density"
  ) +
  theme_minimal()



# Calculate pairwise time differences
plotDF4 <- plotDF %>%
  pivot_wider(names_from = Movement_Type, values_from = initiation_time) %>%
  mutate(
    Hand_Head_Diff = Hand - `Head Rotation`,
    Hand_Gaze_Diff = Hand - Gaze,
    Head_Gaze_Diff = `Head Rotation` - Gaze
  ) %>%
  select(Participant, Group, Hand_Head_Diff, Hand_Gaze_Diff, Head_Gaze_Diff) %>%
  pivot_longer(cols = starts_with("Hand") | starts_with("Head"),
               names_to = "Pair",
               values_to = "Time_Difference")

# Create a histogram with counts (not density)
ggplot(plotDF4, aes(x = Time_Difference, fill = Group)) +
  geom_histogram(position = "dodge", binwidth = 0.01, color = "black") +
  facet_wrap(~ Pair, scales = "free_y") +
  labs(
    title = "Count of Time Differences by Group",
    x = "Time Difference (s)",
    y = "Count"
  ) +
  theme_minimal()




