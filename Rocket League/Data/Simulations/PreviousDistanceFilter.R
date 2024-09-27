library(plot3D)
library(rgl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(ggpubr)
library(ez)
library(cowplot)
library(ggsci)
library(gridExtra)
library(ggsignif)
library(bit64)


data_files <- list.files(pattern = "P15")

participantDataFile <- data_files[1]
print(participantDataFile)


df <- read.csv(participantDataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)
#df <- df[!duplicated(df$Time), ]

dfNew <- df %>% filter(GazePoint_X >= 0)
dfNew <- dfNew %>% filter(GazePoint_X <= 1920)

dfNew <- dfNew %>% filter(GazePoint_Y >= 0)
dfNew <- dfNew %>% filter(GazePoint_Y <= 1080)
dfNew <- dfNew %>% distinct(Frame, GazePoint_X, .keep_all = TRUE)

dfFilt <- dfNew %>%
  mutate(distance_from_prev_x = GazePoint_X - lag(GazePoint_X)) %>%
  mutate(distance_from_prev_x = abs(distance_from_prev_x))%>%
  mutate(distance_from_prev_y = GazePoint_Y - lag(GazePoint_Y)) %>%
  mutate(distance_from_prev_y = abs(distance_from_prev_y)) %>%
  mutate(disance_from_prev = sqrt((lag(GazePoint_X) - GazePoint_X)^2 + (lag(GazePoint_Y) - GazePoint_Y)^2)) %>%
  filter(disance_from_prev < 450)



plot(dfFilt$Timestamp, dfFilt$disance_from_prev)
plot(dfFilt$GazePoint_X, dfFilt$GazePoint_Y)