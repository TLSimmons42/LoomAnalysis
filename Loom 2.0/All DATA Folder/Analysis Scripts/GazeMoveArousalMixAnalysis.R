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
library(signal)
library(plotly)


data_files <- list.files(pattern = "sdP11.csv")
participantDataFile <- data_files[1]
print(participantDataFile)


df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
df <- df[!duplicated(df$Time), ]

gazeData <- data.frame(
  x = as.numeric(df$EyePos_X),
  y = as.numeric(df$EyePos_Y),
  z = as.numeric(df$EyePos_Z),
  area = df$CurrentGazeArea,
  group = "gaze"
)
gazeData <- gazeData %>% filter(x != "N/A")
gazeData <- gazeData %>% filter(x > -20)

handData <- data.frame(
  x = as.numeric(df$RayCast_EndPos_X),
  y = as.numeric(df$RayCast_EndPos_Y),
  z = as.numeric(df$RayCast_EndPos_Z),
  area = df$CurrentGazeArea,
  group = "hand"
)
handData <- handData %>% filter(x != "N/A")

combindedMovementDF <- rbind(gazeData, handData)

plot_ly(combindedMovementDF, x = ~x, y = ~y, z = ~z, color = ~area, type = "scatter3d", mode = "markers")

# p <- ggplot(combindedMovementDF, aes(x = x, y = y, z = z, color = group)) +
#   geom_point(size = 3) +
#   labs(title = "Movement Morging Plot", x = "X", y = "Y", z = "Z") +
#   theme_minimal()
# p
# ggplotly(p)

yAxis <- as.numeric(df$HeadRot_Z)

plot(df$Time, yAxis)

