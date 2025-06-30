# This scrip is designed to calculate the arousal for timeing around grabing and droping
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(bit64)
library(cowplot)
library(scales)
library(patchwork)

arousal_df <- read.csv("C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/SICK DATA FRAMES/Grab_Arousal_DF.csv")
# arousal_df <- read_csv("C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/SICK DATA FRAMES/Drop_Arousal_DF.csv")
arousal_df <- arousal_df %>% mutate(Group = ifelse(Group == "c","Non-Aut",Group))
arousal_df <- arousal_df %>% mutate(Group = ifelse(Group == "e","Aut",Group))


library(tidyverse)
library(cowplot)
library(patchwork)

# Load and filter data
arousal_df <- read.csv("C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/SICK DATA FRAMES/Grab_Arousal_DF.csv")

trim_combined_df <- arousal_df %>%
  filter(Condition != "", Group != "", TimeEpoch <= 1, MeanPercentChange > -100)

subject_means <- trim_combined_df %>%
  group_by(TimeEpoch, Participant, Condition, Group) %>%
  filter(MeanPercentChange <= sd(MeanPercentChange)*3)%>%
  summarise(participant_mean = mean(MeanPercentChange, na.rm = TRUE), .groups = "drop")

summary_df <- subject_means %>%
  group_by(TimeEpoch, Condition, Group) %>%
  summarise(
    mean_percent = mean(participant_mean, na.rm = TRUE),
    sd = sd(participant_mean, na.rm = TRUE),
    n = n(),
    sem = sd / sqrt(n),
    ci95 = sem * qt(0.975, df = n - 1),
    .groups = "drop"
  )

# Define color palette
color_palette <- c("c" = "#0072B2", "e" = "#D55E00")

# Plot function
make_plot <- function(condition_label, title_text, show_y_axis = TRUE) {
  p <- summary_df %>%
    filter(Condition == condition_label) %>%
    ggplot(aes(x = TimeEpoch, y = mean_percent, color = Group, group = Group)) +
    geom_ribbon(aes(
      ymin = mean_percent - ci95,
      ymax = mean_percent + ci95,
      fill = Group
    ), alpha = 0.2, color = NA) +
    geom_line(size = 1.2) +
    geom_point(size = 2.5) +
    scale_color_manual(values = color_palette, labels = c("Non-Autistic", "Autistic")) +
    scale_fill_manual(values = color_palette, labels = c("Non-Autistic", "Autistic")) +
    coord_cartesian(ylim = c(-1, 10)) +
    labs(
      title = title_text,
      x = "Time (s)",
      y = if (show_y_axis) expression("%"~Delta~"pupil size") else NULL
    ) +
    theme_minimal(base_size = 14, base_family = "Helvetica") +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title.y = if (show_y_axis) element_text(margin = margin(r = 10)) else element_blank(),
      axis.text.y = if (show_y_axis) element_text() else element_blank(),
      axis.ticks.y = if (show_y_axis) element_line() else element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none"
    )
  return(p)
}

# Create individual plots
p1 <- make_plot("solo", "Solo", show_y_axis = TRUE)
p2 <- make_plot("co", "Cooperative", show_y_axis = FALSE)
p3 <- make_plot("comp", "Competitive", show_y_axis = FALSE)

# Shared legend from one of the plots
legend <- cowplot::get_legend(
  p1 +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 12)
    )
)

# Combine the plots and legend
combined_plots <- p1 + p2 + p3

final_plot <- cowplot::plot_grid(
  legend,
  combined_plots,
  ncol = 1,
  rel_heights = c(0.15, 1)
)

# Show or save
print(final_plot)
# ggsave("Arousal_Figure_Final.png", final_plot, width = 12, height = 5, dpi = 300)
