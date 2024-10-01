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



data_files <- list.files(pattern = "nuP15.csv")
data_files[]

participantDataFile <- data_files[1]
print(participantDataFile)



df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
df <- df[!duplicated(df$Time), ]

df <- df %>% filter(EyePos_X != "N/A")
df$EyePos_X <- as.numeric(df$EyePos_X)
df$EyePos_Y <- as.numeric(df$EyePos_Y)
df$EyePos_Z <- as.numeric(df$EyePos_Z)


dfFilt <- df %>%
  filter(EyePos_X > -5) %>%
  filter(EyePos_X != 0) %>%
  mutate(distance_from_prev_x = EyePos_X - lag(EyePos_X)) %>%
  mutate(distance_from_prev_x = abs(distance_from_prev_x))%>%
  mutate(distance_from_prev_y = EyePos_X - lag(EyePos_X)) %>%
  mutate(distance_from_prev_y = abs(distance_from_prev_y)) %>%
  mutate(disance_from_prev = sqrt((lag(EyePos_X) - EyePos_X)^2 + (lag(EyePos_Y) - EyePos_Y)^2 + (lag(EyePos_Z) - EyePos_Z)^2))


plot(dfFilt$Time, dfFilt$disance_from_prev)

plot3d(dfFilt$EyePos_X, dfFilt$EyePos_Y, dfFilt$EyePos_Z)
plot3d(df$EyePos_X, df$eyeAngleY, df$EyePos_Z)
