library(tidyverse)
library(lme4)
library(emmeans)

# Load and prep
df <- read_csv("C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/SICK DATA FRAMES/GrabmoveDF_Final.csv")
df <- df %>% filter(Participant != "nuP34")

df <- df %>%
  mutate(
    cubeColor = ifelse(cubeColor == "Blue", "Red", cubeColor),
    cubeColor = recode(cubeColor,
                       "Gold"  = "Still",
                       "Red"   = "Slow",
                       "White" = "Fast"),
    cubeColor = factor(cubeColor, levels = c("Still", "Slow", "Fast")),
    Condition = factor(Condition, levels = c("solo", "co", "comp")),
    Group = factor(Group)
  )

# Function to process any metric
process_metric <- function(df, metric, label) {
  df_clean <- df %>%
    filter(!is.na({{ metric }})) %>%
    group_by(Group, Condition, cubeColor) %>%
    summarise(
      mean_metric = mean({{ metric }}, na.rm = TRUE),
      sd = sd({{ metric }}, na.rm = TRUE),
      n = n(),
      se = sd / sqrt(n),
      ci95 = se * qt(0.975, df = n - 1),
      .groups = "drop"
    ) %>%
    mutate(Measure = label)
  
  return(df_clean)
}


# Process metrics
df_dur   <- process_metric(df, Duration_Time,   "Movement Time\n(ms)")
df_hand  <- process_metric(df, hand_avgVel,     "Avg Hand Velocity\n(m/s)")
df_gaze  <- process_metric(df, gaze_avgVel,     "Avg Gaze Velocity\n(degrees/s)")

plot_df <- bind_rows(df_dur, df_hand, df_gaze)

# Final plot
ggplot(plot_df, aes(x = Condition, y = mean_metric, group = Group, color = Group)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, shape = 16) +
  geom_line(position = position_dodge(width = 0.3), size = 1.2) +
  geom_errorbar(
    aes(ymin = mean_metric - ci95, ymax = mean_metric + ci95),
    width = 0.1,
    position = position_dodge(width = 0.3),
    size = 0.8
  ) +
  facet_grid(Measure ~ cubeColor, scales = "free_y") +
  labs(x = "Condition", y = NULL, color = "Group") +
  theme_classic(base_size = 14) +
  scale_color_manual(values = c("Aut" = "#D55E00", "Non-Aut" = "#0072B2")) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.spacing = unit(1.2, "lines"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    panel.background = element_rect(fill = "gray98", color = NA),
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  )


#---------------------------------------------------------------------------------------------------------------------


df <- read_csv("C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/SICK DATA FRAMES/GrabmoveDF_Final.csv")
df <- df %>% filter(Participant != "nuP34")
df <- df %>% filter(startTime != 638477423459004288)
df <- df %>% filter(startTime != 638761841318563456)
df <- df %>% filter(hand_peak_accel < 50)

df <- df %>%
  mutate(
    cubeColor = ifelse(cubeColor == "Blue", "Red", cubeColor),
    cubeColor = recode(cubeColor,
                       "Gold"  = "Still",
                       "Red"   = "Slow",
                       "White" = "Fast"),
    cubeColor = factor(cubeColor, levels = c("Still", "Slow", "Fast")),
    Condition = factor(Condition, levels = c("solo", "co", "comp")),
    Group = factor(Group)
  )

# Function to process any metric
process_metric <- function(df, metric, label) {
  df_clean <- df %>%
    filter(!is.na({{ metric }})) %>%
    group_by(Group, Condition, cubeColor) %>%
    summarise(
      mean_metric = mean({{ metric }}, na.rm = TRUE),
      sd = sd({{ metric }}, na.rm = TRUE),
      n = n(),
      se = sd / sqrt(n),
      ci95 = se * qt(0.975, df = n - 1),
      .groups = "drop"
    ) %>%
    mutate(Measure = label)
  
  return(df_clean)
}


# Process metrics
df_dur   <- process_metric(df, hand_peak_vel,   "Peak Hand Velocity\n(m/s)")
df_hand  <- process_metric(df, hand_peak_accel,     "Peak Hand Acceleration\n(m/s²)")
df_gaze  <- process_metric(df, hand_endVel,     "End Hand Velocity\n(m/s)")
df_last  <- process_metric(df, hand_endAccel,     "End Hand Acceleration\n(m/s²)")


plot_df <- bind_rows(df_last, df_gaze, df_hand, df_dur)

# Final plot
ggplot(plot_df, aes(x = Condition, y = mean_metric, group = Group, color = Group)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, shape = 16) +
  geom_line(position = position_dodge(width = 0.3), size = 1.2) +
  geom_errorbar(
    aes(ymin = mean_metric - ci95, ymax = mean_metric + ci95),
    width = 0.1,
    position = position_dodge(width = 0.3),
    size = 0.8
  ) +
  facet_grid(Measure ~ cubeColor, scales = "free_y") +
  labs(x = "Condition", y = NULL, color = "Group") +
  theme_classic(base_size = 14) +
  scale_color_manual(values = c("Aut" = "#D55E00", "Non-Aut" = "#0072B2")) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.spacing = unit(1.2, "lines"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    panel.background = element_rect(fill = "gray98", color = NA),
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  )



