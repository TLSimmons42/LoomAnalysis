library(dplyr)
library(ggplot2)
library(bit64)
library(emmeans)
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)

# Read in the CSV file
df <- read_csv("C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/SICK DATA FRAMES/GrabmoveDF_Final.csv")


df <- df %>% filter(startTime != 638477423459004288)
df <- df %>% filter(startTime != 638761841318563456)
df <- df %>% filter(hand_peak_accel < 100)

df <- df %>% filter(Participant != "nuP30")

df <- df %>% filter(Participant != "nuP34")

metric   <- "Duration_Time"
nice_lab <- "Movement duration (ms)"
df<- df %>% mutate(Group = ifelse(Participant == "nuP34", "Aut", Group))
# 
metric   <- "hand_peak_vel"
nice_lab <- "Peak hand velocity (m/s)"


metric   <- "gaze_avg_accel"
nice_lab <- "Peak hand velocity (m/s)"

# 
# 
metric   <- "hand_endAccel"
nice_lab <- "Peak hand Accel (m/s^2)"

metric   <- "hand_endVel"
nice_lab <- "Peak hand Accel (m/s^2)"

metric   <- "hand_avgVel"
nice_lab <- "Avg hand velocity (m/s)"

metric   <- "hand_peak_accel"
nice_lab <- "Avg hand velocity (m/s)"


df <- df %>%
  mutate(
    TargetSpeed = factor(cubeColor, levels = c("Gold","Red","White")),
    Group       = factor(Group),          # Aut / Non-Aut
    Condition   = factor(Condition, levels = c("solo","co","comp"))
  ) %>%
  select(Participant, Group, Condition, TargetSpeed, !!metric) %>% 
  rename(Value = !!metric) %>%      # generic column for the model
  drop_na()

subj_df <- df %>%
  group_by(Participant, Group, Condition) %>%     # keep all three factors
  summarise(
    Mean = mean(Value, na.rm = TRUE),            # participant-level mean
    .groups = "drop"
  )


mod <- lmer(Value ~ Group * Condition * TargetSpeed + 
              (1 + Condition + TargetSpeed | Participant),
            data = df)

mod <- lmer(Value ~ Group * Condition * TargetSpeed + 
                    (1 + Condition + TargetSpeed || Participant), 
                  data = df)

mod <- lmer(
  Value ~ Group * Condition * TargetSpeed + (1 | Participant),
  data = df
)

summary(mod) 
 

emm <- emmeans(mod, ~ Group | Condition * TargetSpeed)

contr <- contrast(emm, method = "revpairwise", adjust = "fdr")
summary(contr)



# Example summary data (replace with your actual data)
summary_df <- df %>%
  group_by(Condition, Group) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    sd_value = sd(Value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_value / sqrt(n),
    ci95 = se * qt(0.975, df = n - 1)
  )


# Plot
ggplot(summary_df, aes(x = Condition, y = mean_value, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - ci95, ymax = mean_value + ci95),
                position = position_dodge(0.8), width = 0.2) +
  labs(x = "Condition", y = "Mean Value", fill = "Group") +
  theme_minimal() +
  theme(text = element_text(size = 14))





dfCon <- df %>% filter(Condition == "co")
t.test(Value ~ Group, data = dfCon)
#-------------------------------------------------------------------------------------------------------------------------------------------


#Table Cration

library(lme4)
library(lmerTest)
library(emmeans)
library(dplyr)
library(tidyr)
library(readr) 

run_metric_analysis <- function(df, metric, nice_lab = NULL) {
  df_metric <- df %>%
    mutate(
      TargetSpeed = factor(cubeColor, levels = c("Gold", "Red", "White")),
      Group       = factor(Group),
      Condition   = factor(Condition, levels = c("solo", "co", "comp"))
    ) %>%
    select(Participant, Group, Condition, TargetSpeed, !!sym(metric)) %>%
    rename(Value = !!sym(metric)) %>%
    drop_na()
  
  # Fit the model
  # mod <- lmer(Value ~ Group * Condition * TargetSpeed + 
  #               (1 + Condition + TargetSpeed | Participant), data = df_metric)
  # 
  mod <- lmer(
    Value ~ Group * Condition * TargetSpeed + (1 | Participant),
    data = df_metric
  )
  
  # Fixed effects summary
  fixed_effects <- summary(mod)$coefficients %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Term") %>%
    mutate(Metric = metric,
           Label  = nice_lab)
  
  # Group comparisons within each condition-speed combo
  emm <- emmeans(mod, ~ Group | Condition * TargetSpeed)
  group_diff <- contrast(emm, method = "pairwise", adjust = "bonferroni") %>%
    as.data.frame() %>%
    mutate(Metric = metric, Label = nice_lab)
  
  list(fixed = fixed_effects, group_comparisons = group_diff, model = mod)
}

df <- read_csv("C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/SICK DATA FRAMES/GrabmoveDF_Final.csv") %>%
  filter(Participant != "nuP30")

result <- run_metric_analysis(df, "hand_endVel", "Peak hand velocity (m/s)")

# View tables
result$fixed
result$group_comparisons
