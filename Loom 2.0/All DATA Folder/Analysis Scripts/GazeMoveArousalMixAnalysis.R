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


trimDF <- df %>%
  mutate(Time = (Time - Time[1])/10000000) 

# trimDF <- trimDF %>%
#   dplyr :: filter(Time <= 25 & Time >=24)


trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(grepl("picked", Event), Event, "none"))

trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(grepl("P2", ActionEvent), "none", ActionEvent))

trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(!grepl("P1", ActionEvent), ActionEvent, "none"))

trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(ActionEvent != "none", "Grab", ActionEvent))

trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(grepl("dropped", Event), Event, ActionEvent))

trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(grepl("P1", ActionEvent), "Dropped", ActionEvent))

trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(ActionEvent == "Grab" | ActionEvent == "Dropped", ActionEvent, "none"))

trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(ActionEvent == "Grab" | ActionEvent == "Dropped", ActionEvent, CurrentGazeArea))

trimDF <- trimDF %>% 
  mutate(Size = ifelse(ActionEvent == "Grab" | ActionEvent == "Dropped", 3, 1))

trimDF <- trimDF %>% dplyr :: filter(EyePos_X != "N/A")


trimDF <- trimDF %>%
  filter(as.numeric(EyePos_X) > -1)




# # 3D figure plot
# gazeData <- data.frame(
#   x = as.numeric(df$EyePos_X),
#   y = as.numeric(df$EyePos_Y),
#   z = as.numeric(df$EyePos_Z),
#   area = df$CurrentGazeArea,
#   group = "gaze"
# )
# gazeData <- gazeData %>% filter(x != "N/A")
# gazeData <- gazeData %>% filter(x > -20)
# 
# handData <- data.frame(
#   x = as.numeric(df$RayCast_EndPos_X),
#   y = as.numeric(df$RayCast_EndPos_Y),
#   z = as.numeric(df$RayCast_EndPos_Z),
#   area = df$CurrentGazeArea,
#   group = "hand"
# )
# 
# handData <- handData %>% filter(x != "N/A")
# 
# combindedMovementDF <- rbind(gazeData, handData)
# 
# plot_ly(combindedMovementDF, x = ~x, y = ~y, z = ~z, color = ~area, type = "scatter3d", mode = "markers")
# 


# This will plot the individual movements for hand, head and gaze
subTrimDF <- trimDF 
subTrimDF <- trimDF %>%
  dplyr :: filter(Time <= 50 & Time >=20)

xHand <- subTrimDF$HandPos_X
yHand <- subTrimDF$HandPos_Y
zHand <- subTrimDF$HandPos_Z

xHead <- subTrimDF$HeadPos_X
yHead <- subTrimDF$HeadPos_Y
zHead <- subTrimDF$HeadPos_Z

xHeadRot <- subTrimDF$HeadRot_X
yHeadRot <- subTrimDF$HeadRot_Y
zHeadRot <- subTrimDF$HeadRot_Z

xRay <- subTrimDF$RayCast_EndPos_X
yRay <- subTrimDF$RayCast_EndPos_Y
zRay <- subTrimDF$RayCast_EndPos_Z

xEye <- as.numeric(subTrimDF$EyePos_X)
yEye <- as.numeric(subTrimDF$EyePos_Y)
zEye <- as.numeric(subTrimDF$EyePos_Z)




p <- subTrimDF %>%
  ggplot(aes(x = Time, y = zEye, size = Size, color = ActionEvent)) +
  #geom_line(size = 2)+
  geom_point()+
  # geom_line(aes(y = yHand), color = "red", linetype = "solid") +
  # geom_line(aes(y = zHand), color = "green", linetype = "solid") +
  labs(title = "", x = "Time (S)", y = "x axis hand pos") +
  theme_minimal() 
#p + geom_point(aes(color = factor(ActionEvent)), size = 3)
# p + geom_point(aes (y = yHand),(color = factor(ActionEvent)), size = 3)
# p + geom_point(aes (y = zHand),(color = factor(ActionEvent)), size = 3)

p
# p + scale_y_continuous(limits = c(-1.5,1.5))
 
