# ── libraries ────────────────────────────────────────
library(tidyverse)    # dplyr + ggplot2
library(patchwork)    # layout & shared legend

# ── load & preprocess ───────────────────────────────
arousal_df <- read.csv(
  "C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/SICK DATA FRAMES/Grab_Arousal_DF.csv"
)

# rename groups for clarity
arousal_df <- arousal_df %>%
  mutate(Group = recode(Group, c = "Non-Aut", e = "Aut"))

trim_combined_df <- arousal_df %>%
  filter(
    Condition != "", Group != "",
    TimeEpoch <= 1,  MeanPercentChange > -100
  )

subject_means <- trim_combined_df %>%
  group_by(TimeEpoch, Participant, Condition, Group) %>%
  filter(MeanPercentChange <= sd(MeanPercentChange) * 3) %>%      # 3-SD trim
  summarise(participant_mean = mean(MeanPercentChange), .groups = "drop")

summary_df <- subject_means %>%
  group_by(TimeEpoch, Condition, Group) %>%
  summarise(
    mean_percent = mean(participant_mean),
    sd           = sd(participant_mean),
    n            = n(),
    sem          = sd / sqrt(n),
    ci95         = sem * qt(.975, df = n - 1),
    .groups = "drop"
  )

# ── colour palette ──────────────────────────────────
pal <- c("Non-Aut" = "#0072B2", "Aut" = "#D55E00")

# ── helper to build each panel ──────────────────────
make_panel <- function(cond, title_text,
                       left_y = FALSE, x_label = FALSE) {
  summary_df %>%
    filter(Condition == cond) %>%
    ggplot(aes(TimeEpoch, mean_percent,
               colour = Group, fill = Group, group = Group)) +
    geom_ribbon(aes(ymin = mean_percent - ci95,
                    ymax = mean_percent + ci95),
                alpha = .20, colour = NA) +
    geom_line(size = 1.2) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = pal,
                        breaks = c("Non-Aut", "Aut"),
                        labels = c("Non-Autistic", "Autistic")) +
    scale_fill_manual(values = pal,
                      breaks = c("Non-Aut", "Aut"),
                      labels = c("Non-Autistic", "Autistic")) +
    coord_cartesian(ylim = c(-1, 10)) +
    labs(
      title = title_text,
      y = if (left_y) expression("%" * Delta * "pupil size") else NULL,
      x = if (x_label) "Time (s)" else NULL
    ) +
    theme_minimal(base_size = 14, base_family = "Helvetica") +
    theme(
      plot.title      = element_text(face = "bold", size = 15, hjust = .5),
      # y-axis
      axis.title.y    = if (left_y) element_text(margin = margin(r = 8))
      else element_blank(),
      axis.text.y     = if (left_y) element_text() else element_blank(),
      axis.ticks.y    = if (left_y) element_line() else element_blank(),
      # x-axis ticks ALWAYS kept; only title toggled
      axis.title.x    = if (x_label) element_text(margin = margin(t = 6))
      else element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

# ── build three panels ─────────────────────────────
p1 <- make_panel("solo", "Solo",        left_y = TRUE,  x_label = FALSE)
p2 <- make_panel("co",   "Co-operative",left_y = FALSE, x_label = TRUE)
p3 <- make_panel("comp", "Competitive", left_y = FALSE, x_label = FALSE)

# ── combine & collect legend with patchwork ────────
combined <- (p1 | p2 | p3) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "top",
        legend.title    = element_blank(),
        legend.text     = element_text(size = 12))

# ── add figure-level title ─────────────────────────
final_fig <- combined +
  plot_annotation(
    title = "Pupil-linked Arousal Around Grasp (Grab Task)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 18, hjust = .5,
                                margin = margin(b = 6))
    )
  )

# ── display / save ────────────────────────────────
print(final_fig)
# ggsave("Arousal_Figure_Final.png", final_fig, width = 10, height = 4.5, dpi = 300)



#---------------------------------------------------------------------------------------------------
#Stats Analysis
library(lmerTest)
library(emmeans)
library(dplyr)

trim_combined_df <- arousal_df
trim_combined_df$TimeEpoch <- as.factor(trim_combined_df$TimeEpoch)

mod <- lmer(MeanPercentChange ~ Group * Condition *TimeEpoch + (1 | Participant), data = trim_combined_df)
summary(mod)
anova(mod)

# Estimated marginal means at specific timepoints
emm <- emmeans(mod, ~ Group | Condition * TimeEpoch,
               at = list(TimeEpoch = c(0, 0.5, 1)))

# Pairwise group comparisons within each condition and timepoint
pairs(emm, by = c("Condition", "TimeEpoch"))




# Filter to one condition
solo_df <- trim_combined_df %>% filter(Condition == "solo")

mod_solo <- lmer(MeanPercentChange ~ Group * TimeEpoch + (1 | Participant), data = solo_df)
summary(mod_solo)


solo_df <- trim_combined_df %>% filter(Condition == "co")

mod_co <- lmer(MeanPercentChange ~ Group * TimeEpoch + (1 | Participant), data = solo_df)
summary(mod_co)

solo_df <- trim_combined_df %>% filter(Condition == "comp")

mod_comp <- lmer(MeanPercentChange ~ Group * TimeEpoch + (1 | Participant), data = solo_df)
summary(mod_comp)

anova(mod_solo)
anova(mod_co)
anova(mod_comp)




emm_comp <- emmeans(mod_solo, ~ Group | TimeEpoch,
                    at = list(TimeEpoch = c(0, 0.5, 1.0)))
pairs(emm_comp)

emm_comp <- emmeans(mod_co, ~ Group | TimeEpoch,
                    at = list(TimeEpoch = c(0, 0.5, 1.0)))
pairs(emm_comp)

emm_comp <- emmeans(mod_comp, ~ Group | TimeEpoch,
                    at = list(TimeEpoch = c(0, 0.5, 1.0)))
pairs(emm_comp)

