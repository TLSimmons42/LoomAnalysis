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


PACdataFile <- "C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Data csv Files/PACdf tester 5_1552.csv"
data_files <- list.files(pattern = "sdP11.csv")
participantDataFile <- data_files[1]
print(participantDataFile)

PACdf <- read.csv(PACdataFile, colClasses=c("PACstartTime" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
df <- df[!duplicated(df$Time), ]

df$Time[is.na(df$Time)] <- 0
df$Time <- as.integer64(df$Time)


participantID <- df$Participant[1]
PACdf <- PACdf %>% filter(Participant == participantID)




trimDF <- df %>%
  filter(as.numeric(HandPos_X) > 0)

trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(grepl("picked", Event), Event, "none"))

trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(grepl("P2", ActionEvent), CurrentGazeArea, ActionEvent))

trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(grepl("P1", ActionEvent), CurrentGazeArea, ActionEvent))

trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(ActionEvent != "none", "Grab", ActionEvent))

trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(grepl("dropped", Event), Event, ActionEvent))

trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(grepl("player", ActionEvent), "none", ActionEvent))

trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(grepl("P2", ActionEvent), "none", ActionEvent))

trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(grepl("P1", ActionEvent), "none", ActionEvent))

trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(grepl("dropped", ActionEvent), "Dropped", ActionEvent))

trimDF <- trimDF %>%
  mutate(ActionEvent = ifelse(ActionEvent == "Grab" | ActionEvent == "Dropped", ActionEvent, CurrentGazeArea))




for (g in 1:nrow(PACdf)) {
  currentPACstartTime <- PACdf$PACstartTime[g]
  currentEvent <- PACdf$PACtype[g]
  trimDF <- trimDF %>% mutate(ActionEvent = ifelse(currentPACstartTime == Time & currentEvent == "grab", "grabLook",ActionEvent))
  trimDF <- trimDF %>% mutate(ActionEvent = ifelse(currentPACstartTime == Time & currentEvent == "place", "placeLook",ActionEvent))
  
}

trimDF <- trimDF %>% 
  mutate(Size = ifelse(ActionEvent == "Grab" | ActionEvent == "Dropped"| ActionEvent == "grabLook"| ActionEvent == "placeLook", 3, 1))


trimDF <- trimDF %>%
  mutate(Time = (Time - Time[1])/10000000) 

trimDF <- trimDF %>% dplyr :: filter(EyePos_X != "N/A")


trimDF <- trimDF %>%
  filter(as.numeric(EyePos_X) > -1)






# 3D figure plot
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
# plot_ly(combindedMovementDF, x = ~x, y = ~y, z = ~z, color = ~ area, type = "scatter3d", mode = "markers")



# This will plot the individual movements for hand, head and gaze
subTrimDF <- trimDF 
subTrimDF <- trimDF %>%
  dplyr :: filter(Time >= 100 & Time <= 130)

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



subTrimDF <- subTrimDF %>% mutate(EularAngle = asin(2 * (sqrt(1 - (xHeadRot^2 + yHeadRot^2 + zHeadRot^2))*yHeadRot - (xHeadRot * zHeadRot))))
subTrimDF <- subTrimDF %>% mutate(EularAngle = EularAngle * (180 / pi))


# quaternion_to_euler <- function(xHeadRot, yHeadRot, zHeadRot) {
#   # Calculate w assuming the quaternion is normalized
#   w <- sqrt(1 - (xHeadRot^2 + yHeadRot^2 + zHeadRot^2))
#   
#   # Convert quaternion to Euler angles
#   euler <- QuatToEuler(c(xHeadRot, yHeadRot, zHeadRot, w))
#   
#   # Convert radians to degrees
#   euler_degrees <- euler * (180 / pi)
#   
#   return(euler_degrees)
# }
# 
# euler_angles <- apply(data, 1, function(row) {
#   quaternion_to_euler(xHeadRot, row['y'], row['z'])
# })


p <- subTrimDF %>%
  ggplot(aes(x = Time, y = xHand, size = Size, color = ActionEvent)) +
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
 


plot_ly(subTrimDF, x = ~xHand, y = ~yHand, z = ~zHand, color = ~ ActionEvent, type = "scatter3d", mode = "markers")

