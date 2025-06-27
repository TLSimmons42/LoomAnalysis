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

metric   <- "Duration_Time"
nice_lab <- "Movement duration (ms)"
#df<- df %>% mutate(Group = ifelse(Participant == "nuP22", "Aut", Group))
# 
# metric   <- "hand_peak_vel"
# nice_lab <- "Peak hand velocity (m/s)"
# 
# 
# metric   <- "hand_time2PeakAccel"
# nice_lab <- "Peak hand Accel (m/s^2)"

metric   <- "hand_endVel"
nice_lab <- "Peak hand Accel (m/s^2)"

metric   <- "gaze_time2PeakAccel"
nice_lab <- "Avg hand velocity (m/s)"

metric   <- "handRot_peak_vel"
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


mod <- lmer(
  Value ~ Group * Condition * TargetSpeed + (1 | Participant),
  data = df
)

summary(mod) 


emm <- emmeans(mod, ~ Group | Condition * TargetSpeed)

contr <- contrast(emm, method = "revpairwise", adjust = "fdr")
summary(contr)

# 4b. If you need effect sizes
eff <- eff_size(contr, sigma = sigma(mod), edf = df.residual(mod))
eff