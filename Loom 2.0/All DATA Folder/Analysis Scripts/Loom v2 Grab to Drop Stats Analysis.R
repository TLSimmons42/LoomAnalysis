library(dplyr)
library(ggplot2)
library(bit64)
library(emmeans)
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)

# Read in the CSV file
df <- read_csv("C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/SICK DATA FRAMES/Grab_2_Drop_FINAL.csv")
#df <- df %>% filter(cubeColor == "Gold")
#sdDurationTime <- sd(df$Duration_Time)

#df <- df %>% filter(Duration_Time <= sdDurationTime*3)


df <- df %>% filter(Participant != "nuP34")

metric   <- "hand_time2Initiation"
nice_lab <- "Movement duration (ms)"
#df<- df %>% mutate(Group = ifelse(Participant == "nuP22", "Aut", Group))

# metric   <- "hand_1secondVel"
# nice_lab <- "Peak hand velocity (m/s)"
# 
# 
# metric   <- "hand_1secondAccel"
# nice_lab <- "Peak hand Accel (m/s^2)"
# 
# metric   <- "gaze_1secondVel"
# nice_lab <- "Peak hand Accel (m/s^2)"
# 
# metric   <- "gaze_1secondAccel"
# nice_lab <- "Avg hand velocity (m/s)"
# 
# metric   <- "hand_time2Initiation"
# nice_lab <- "Avg hand velocity (m/s)"

df_clean <- df %>%
  # filter(Participant != "nuP30")%>%
  filter(!!metric_sym !=0) %>%
  filter(!is.na(!!metric_sym)) %>%
  mutate(
    Group     = factor(Group),
    Condition = factor(Condition, levels = c("solo", "co", "comp"))
  ) %>%
  select(Participant, Group, Condition, !!sym(metric)) %>%
  rename(Value = !!sym(metric)) %>%
  drop_na()

plot_summary <- df_clean %>%
  group_by(Group, Condition) %>%
  summarise(
    mean = mean(Value),
    se   = sd(Value) / sqrt(n()),
    ci   = se * qt(.975, df = n() - 1),
    .groups = "drop"
  )
# # OPTIONAL: Trial-level outlier trimming BEFORE aggregation
# df_clean <- df_clean %>%
#   filter(!!metric_sym < mean(!!metric_sym) + 3 * sd(!!metric_sym) & !!metric_sym > mean(!!metric_sym) - 3 * sd(!!metric_sym))


mod <- lmer(
  Value ~ Group * Condition + (1 | Participant),
  data = df_clean
)

summary(mod)
emmeans(mod, pairwise ~ Group | Condition, adjust = "bonferroni")




#-----------------------------------------------------------------------------------------------------------
library(tidyverse)
library(readr)

# Load and prepare data
df <- read_csv("C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/SICK DATA FRAMES/Grab_2_Drop_FINAL.csv")

df <- df %>%
  filter(Participant != "nuP34") %>%
  mutate(
    Condition = factor(Condition, levels = c("solo", "co", "comp")),
    Group = factor(Group)
  )

# Function to process participant means for any metric
# process_metric <- function(df, metric, label) {
#   metric <- "hand_time2PeakVel"
#   mow <- df %>%
#     filter(!is.na(.data[[metric]])) %>%
#     group_by(Participant, Group, Condition) %>%
#     summarise(
#       participant_mean = mean(.data[[metric]], na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     group_by(Group, Condition) %>%
#     summarise(
#       mean_metric = mean(participant_mean),
#       sd = sd(participant_mean),
#       n = n(),
#       se = sd / sqrt(n),
#       ci95 = se * qt(0.975, df = n - 1),
#       .groups = "drop"
#     ) %>%
#     mutate(Measure = label)
# }

process_metric <- function(df, metric, label) {
  df %>%
    filter(!is.na(.data[[metric]])) %>%
    group_by(Group, Condition) %>%
    summarise(
      mean_metric = mean(.data[[metric]], na.rm = TRUE),
      sd = sd(.data[[metric]], na.rm = TRUE),
      n = n(),
      se = sd / sqrt(n),
      ci95 = se * qt(0.975, df = n - 1),
      .groups = "drop"
    ) %>%
    mutate(Measure = label)
}



# Process all 10 metrics
plot_df <- bind_rows(
  process_metric(df, "Duration_Time",         "Movement Time (ms)"),
  process_metric(df, "hand_time2Initiation",  "Hand Initiation (s)"),
  process_metric(df, "headRot_time2Initiation","Head Rot. Initiation (s)"),
  process_metric(df, "gaze_time2Initiation",   "Gaze Initiation (s)"),
  process_metric(df, "hand_peak_vel",          "Peak Hand Velocity (m/s)"),
  process_metric(df, "headRot_peak_vel",       "Peak Head Rot. Vel. (°/s)"),
  process_metric(df, "gaze_peak_vel",          "Peak Gaze Velocity (°/s)"),
  process_metric(df, "hand_time2PeakVel",      "Hand TTP Velocity (s)"),
  process_metric(df, "headRot_time2PeakVel",   "Head Rot. TTP Velocity (s)"),
  process_metric(df, "gaze_time2PeakVel",      "Gaze TTP Velocity (s)")
)

# Final plot
ggplot(plot_df, aes(x = Condition, y = mean_metric, group = Group, color = Group)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_line(position = position_dodge(width = 0.3), size = 1.2) +
  geom_errorbar(
    aes(ymin = mean_metric - ci95, ymax = mean_metric + ci95),
    width = 0.1,
    position = position_dodge(width = 0.3),
    size = 0.8
  ) +
  facet_wrap(~ Measure, scales = "free_y", ncol = 3) +
  labs(x = "Condition", y = NULL, color = "Group") +
  theme_classic(base_size = 14) +
  scale_color_manual(values = c("Aut" = "#D55E00", "Non-Aut" = "#0072B2")) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )
