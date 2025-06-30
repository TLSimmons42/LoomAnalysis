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

#sdDurationTime <- sd(df$Duration_Time)

#df <- df %>% filter(Duration_Time <= sdDurationTime*3)


df <- df %>% filter(Participant != "nuP34")
df <- df %>% mutate(Group = ifelse(Participant == "nuP30", "Aut",Group))

metric   <- "Duration_Time"
nice_lab <- "Movement duration (ms)"
#df<- df %>% mutate(Group = ifelse(Participant == "nuP22", "Aut", Group))

metric   <- "hand_1secondVel"
nice_lab <- "Peak hand velocity (m/s)"


metric   <- "hand_1secondAccel"
nice_lab <- "Peak hand Accel (m/s^2)"

metric   <- "gaze_1secondVel"
nice_lab <- "Peak hand Accel (m/s^2)"

metric   <- "gaze_1secondAccel"
nice_lab <- "Avg hand velocity (m/s)"

metric   <- "hand_time2Initiation"
nice_lab <- "Avg hand velocity (m/s)"





# --- 1.  Prep data (no TargetSpeed) ----
df <- df %>%
  #mutate(Group = ifelse(Participant == "nuP33", "Aut", Group))%>%
# or your full path
  mutate(
    Group     = factor(Group),                      # Aut / Non-Aut
    Condition = factor(Condition, levels = c("solo", "co", "comp"))
  ) %>% 
  select(Participant, Group, Condition, !!metric) %>%   # keep only the columns you need
  rename(Value = !!metric) %>% 
  drop_na()

# --- 2.  Fit model without TargetSpeed ----
mod <- lmer(
  Value ~ Group * Condition + (1 | Participant),    # no TargetSpeed term
  data = df
)

summary(mod)

emm <- emmeans(mod, ~ Group | Condition)

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






dfCon <- df %>% filter(Condition == "solo")
t.test(Value ~ Group, data = dfCon)














ggplot(subj_df, aes(x = Condition, y = Mean, fill = Group)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.6) +
  labs(x = "Category", y = "Value", 
       title = "Box Plot of Values by Category and Group") +
  theme_minimal() +
  ylim(0, .3)+
  scale_fill_manual(values = c("lightblue", "lightcoral"), name = "Group") +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))











subj_df <- df %>%
  group_by(Participant, Group, Condition) %>%     # keep all three factors
  summarise(
    Mean = mean(Value, na.rm = TRUE),            # participant-level mean
    .groups = "drop"
  )


anova_results <- aov_ez(
  id = "Participant",       # Subject identifier
  dv = "avgStratDuration",        # Dependent variable
  between = "Group",    # Between-subjects factor
  within = "Trial",      # Within-subjects factor
  data = analysisDurationDF
)
print(anova_results)

emmeans_results_trial <- emmeans(anova_results, pairwise ~ Trial, adjust = "bonferroni")
print(emmeans_results_trial$contrasts)


anova_afex <- aov_ez(
  id      = "Participant",
  dv      = "Mean",
  within  = "Condition",
  between = "Group",
  data    = subj_df,
)
print(anova_afex)

emmeans_results_trial <- emmeans(anova_afex, pairwise ~ Group | Condition, adjust = "bonferroni")
print(emmeans_results_trial$contrasts)










summary_df <- df %>%
  group_by(Condition, Group) %>%
  summarise(
    mean_value = mean(hand_1secondVel, na.rm = TRUE),
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
