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
library(plotly)


MovementTimesDF <- data.frame(Participant = factor(),
                              Age = numeric(),
                              Sex = factor(),
                              Condition = factor(),
                              Trial = numeric(),
                              Group = factor(),
                              avgGrab = numeric(),
                              avgDrop = numeric(),
                              avgDropStart = numeric(),
                              grabCount = numeric(),
                              dropCount = numeric(),
                              dropStartCount = numeric(),
                              avgFullSequence = numeric(),
                              stillGrab = numeric(),
                              slowGrab = numeric(),
                              fastGrab = numeric(),
                              stringsAsFactors = FALSE)

individualMovementTimes <- data.frame(Participant = factor(),
                                      Age = numeric(),
                                      Sex = factor(),
                                      Condition = factor(),
                                      Trial = numeric(),
                                      Group = factor(),
                                      sequecnceType = factor(),
                                      startTime = numeric(),
                                      endTime = numeric(),
                                      moveTime = numeric())


individualColorGrabTimes <- data.frame(Participant = factor(),
                                       Age = numeric(),
                                       Sex = factor(),
                                       Condition = factor(),
                                       Trial = numeric(),
                                       Group = factor(),
                                       cubeColor = factor(),
                                       startTime = numeric(),
                                       endTime = numeric(),
                                       moveTime = numeric())



PACdataFile <- "C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Data csv Files/PACdf tester 9_1.csv"
rotationConversionFile <- "C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Data csv Files/headRotTest.csv"
data_files <- list.files(pattern = "nuP15.csv")


strings_to_filter <- c("nuP2_old1","nuP2_old2","nuP2_old3","nuP2_old4",
                       "nuP4_old1","nuP4_old2","nuP4_old3","nuP4_old4", "nuP4",
                       "newnuP3_old5",  "newnuP3_old4","newnuP3_old3","newnuP3_old2","newnuP3_old1", "newnuP3",
                       "nuP3_old5",  "nuP3_old4","nuP3_old3","nuP3_old2","nuP3_old1")
data_files <- data_files[!(grepl(paste(strings_to_filter, collapse="|"), data_files))]


for(f in 1:length(data_files))
{
  
  #print(df$Group[1])
  dropStartCounter <- 0
  participantDataFile <- data_files[1]
  print(participantDataFile)
  
  
  
  rotationConversionDF <- read.csv(rotationConversionFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  PACdf <- read.csv(PACdataFile, colClasses=c("PACstartTime" = "integer64", "PACendTime" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]
  
  if(df$Participant[1] == "sd10"){
    df <- df %>% mutate(Group = ifelse(Group == 1, "e", Group))
    print("meh")
  }
  if(df$Participant[1] == "sdP7"){
    df <- df %>% mutate(Sex = "m")
    
  }
  
  
  df$Time[is.na(df$Time)] <- 0
  df$Time <- as.integer64(df$Time)
  
  participantID <- df$Participant[1]
  PACdf <- PACdf %>% filter(Participant == participantID)
  
  trimDF <- df %>% dplyr :: filter(EyePos_X != "N/A")
  
  trimDF <- trimDF %>%
    mutate(ModTime = (Time - Time[1])/10000000) 
  
  trimDF <- trimDF %>%
    filter(as.numeric(EyePos_X) > -1)
  
  
  trimDF <- trimDF %>%
    filter(as.numeric(HandPos_X) > 0)
  
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(grepl("picked", Event), Event, "temp"))
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(grepl("Player 1 hits", Event), Event, ActionEvent))
  
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(grepl("P2", ActionEvent), "temp", ActionEvent))
  
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(grepl("P1", ActionEvent), "temp", ActionEvent))
  
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(ActionEvent != "temp", "Grab", ActionEvent))
  
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(grepl("dropped", Event), Event, ActionEvent))
  
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(grepl("player", ActionEvent), "temp", ActionEvent))
  
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(grepl("P2", ActionEvent), "temp", ActionEvent))
  
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(grepl("P1", ActionEvent), "temp", ActionEvent))
  
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(grepl("dropped", ActionEvent), "Dropped", ActionEvent))
  
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(ActionEvent == "Grab" | ActionEvent == "Dropped", ActionEvent, CurrentGazeArea))
  
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(grepl("Drop Hit", Event), "DropZone Hit", ActionEvent))
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(grepl("P2", Event) & grepl("dropped", Event), "P2 Drop", ActionEvent))
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(grepl("P2", Event) & grepl("picked", Event), "P2 Grab", ActionEvent))
  
  filterDF <- trimDF %>% filter(ActionEvent == "Dropped")
  for (c in 1:nrow(filterDF)) {
    
    input_string <- filterDF$Event[c]
    #print(input_string)
    
    
    pattern <- ".*(?= was)"
    extracted_phrase <- str_extract(input_string, pattern)
    if(is.na(extracted_phrase)){
      # pattern <- ".*(?= was)"
      # extracted_phrase <- str_extract(input_string, pattern)
      
      extracted_phrase <- sub("(.*?)was.*", "\\1", input_string)
      
      #print("HELOOOOO PATTERN")
    }
    secondHalf <- "was placed in dropzone"
    
    # if(trimDF$Condition[1] == "comp"){
    #   pattern <- ".*(?=was)"
    # }else{ 
    #   pattern <- ".*(?= was)"
    # }
    # 
    # extracted_phrase <- str_extract(input_string, pattern)
    full_phrase <- paste0(extracted_phrase, secondHalf)
    #print(full_phrase)
    
    trimDF <- trimDF %>%
      mutate(row_num = row_number()) %>%
      mutate(Event = if_else(Event == full_phrase & row_num != max(row_num[Event == full_phrase]), 
                             "playing", Event)) %>%
      select(-row_num)
    
    
    trimDF <- trimDF %>% mutate(ActionEvent = ifelse(full_phrase == Event, "DropStart",ActionEvent))
  }
  
  trimDF <- trimDF %>% mutate(cubeColor = "none")
  trimDF <- trimDF %>% mutate(sequenceType = "none")
  
  trimDF <- trimDF %>% mutate(sequenceNum = 0)
  PACdf <- PACdf[order(PACdf$PACstartTime), ]
  
  for (g in 1:nrow(PACdf)) {
    currentPACstartTime <- PACdf$PACstartTime[g]
    currentPACendTime <- PACdf$PACendTime[g]
    currentEvent <- PACdf$PACtype[g]
    currentColor <- PACdf$cubeColor[g]
    
    trimDF <- trimDF %>% mutate(cubeColor = ifelse(currentPACstartTime == Time & currentEvent == "grab", currentColor,cubeColor))
    trimDF <- trimDF %>% mutate(cubeColor = ifelse(currentPACendTime == Time & currentEvent == "grab", currentColor,cubeColor))
    trimDF <- trimDF %>% mutate(cubeColor = ifelse(currentPACstartTime == Time & currentEvent == "place", currentColor,cubeColor))
    trimDF <- trimDF %>% mutate(cubeColor = ifelse(currentPACendTime == Time & currentEvent == "place", currentColor,cubeColor))
    
    trimDF <- trimDF %>% mutate(sequenceNum = ifelse(currentPACstartTime == Time & currentEvent == "grab", g,sequenceNum))
    trimDF <- trimDF %>% mutate(sequenceNum = ifelse(currentPACendTime == Time & currentEvent == "grab", g,sequenceNum))
    trimDF <- trimDF %>% mutate(sequenceNum = ifelse(currentPACstartTime == Time & currentEvent == "place", g,sequenceNum))
    trimDF <- trimDF %>% mutate(sequenceNum = ifelse(currentPACendTime == Time & currentEvent == "place", g,sequenceNum))
    
    trimDF <- trimDF %>% mutate(ActionEvent = ifelse(currentPACstartTime == Time & currentEvent == "grab", "grabLook",ActionEvent))
    trimDF <- trimDF %>% mutate(ActionEvent = ifelse(currentPACstartTime == Time & currentEvent == "place", "placeLook",ActionEvent))
    
    
  }
  
  trimDF <- trimDF %>% 
    mutate(Size = ifelse(ActionEvent == "Grab" | ActionEvent == "Dropped"| ActionEvent == "grabLook"| ActionEvent == "placeLook"
                         | ActionEvent == "DropStart"| ActionEvent == "DropZone Hit"| ActionEvent == "P2 Drop" | ActionEvent == "P2 Grab", 3, 1))
  
  
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
  # subTrimDF <- trimDF %>%
  #   dplyr :: filter(ModTime >= 64.99873 & ModTime <= 68)
  
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
  
  
  
  
  subTrimDF <- subTrimDF %>% mutate(Roll = atan2(2*(sqrt(1 - (xHeadRot^2 + yHeadRot^2 + zHeadRot^2))*xHeadRot + yHeadRot*zHeadRot), 1 - 2*(xHeadRot^2 + yHeadRot^2)))
  subTrimDF <- subTrimDF %>% mutate(Roll = Roll * (180 / pi))
  # subTrimDF <- subTrimDF %>% mutate(Roll = abs(Roll))
  # subTrimDF <- subTrimDF %>% mutate(Roll = Roll - max(Roll))
  # subTrimDF <- subTrimDF %>% mutate(Roll = abs(Roll))
  
  subTrimDF <- subTrimDF %>% mutate(Pitch = asin(2 * (sqrt(1 - (xHeadRot^2 + yHeadRot^2 + zHeadRot^2))*yHeadRot - (xHeadRot * zHeadRot))))
  subTrimDF <- subTrimDF %>% mutate(Pitch = Pitch * (180 / pi))
  # subTrimDF <- subTrimDF %>% mutate(Pitch = abs(Pitch))
  # subTrimDF <- subTrimDF %>% mutate(Pitch = Pitch - max(Pitch))
  # subTrimDF <- subTrimDF %>% mutate(Pitch = abs(Pitch))
  
  subTrimDF <- subTrimDF %>% mutate(Yaw = atan2(2*(sqrt(1 - (xHeadRot^2 + yHeadRot^2 + zHeadRot^2))*zHeadRot + xHeadRot*yHeadRot), 1 - 2*(yHeadRot^2 + zHeadRot^2)))
  subTrimDF <- subTrimDF %>% mutate(Yaw = Yaw * (180 / pi))
  subTrimDF <- subTrimDF %>% mutate(Yaw = abs(Yaw))
  subTrimDF <- subTrimDF %>% mutate(Yaw = Yaw - max(Yaw))
  subTrimDF <- subTrimDF %>% mutate(Yaw = abs(Yaw))
  
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
  
  subTrimDF <- subTrimDF %>% mutate(UnityHeadRotX = HeadRot_X)
  subTrimDF <- subTrimDF %>% mutate(UnityHeadRotY = HeadRot_Y)
  subTrimDF <- subTrimDF %>% mutate(UnityHeadRotZ = HeadRot_Z)
  
  for(i in 0:nrow(subTrimDF)){
    subTrimDF$UnityHeadRotX[i] <- as.numeric(rotationConversionDF$xHandRot[i])
    subTrimDF$UnityHeadRotY[i] <- as.numeric(rotationConversionDF$yHandRot[i])
    subTrimDF$UnityHeadRotZ[i] <- as.numeric(rotationConversionDF$zHandRot[i])
  }
  # subTrimDF <- subTrimDF %>% mutate(UnityHeadRotY = UnityHeadRotY - max(UnityHeadRotY))
  # subTrimDF <- subTrimDF %>% mutate(UnityHeadRotY = abs(UnityHeadRotY))
  # 
  # subTrimDF <- subTrimDF %>% mutate(UnityHeadRotX = UnityHeadRotX - max(UnityHeadRotX))
  # subTrimDF <- subTrimDF %>% mutate(UnityHeadRotX = abs(UnityHeadRotX))
  # 
  # subTrimDF <- subTrimDF %>% mutate(UnityHeadRotZ = UnityHeadRotZ - max(UnityHeadRotZ))
  # subTrimDF <- subTrimDF %>% mutate(UnityHeadRotZ = abs(UnityHeadRotZ))
  
  
  
  p <- subTrimDF %>%
    ggplot(aes(x = ModTime, y = xHand, size = Size, color = ActionEvent)) +
    #geom_line(size = 2)+
    geom_point()+
    # geom_line(aes(y = yHand), color = "red", linetype = "solid") +
    # geom_line(aes(y = zHand), color = "green", linetype = "solid") +
    labs(title = "", x = "Time (s)", y = "Hand Pos X-Axis (m)") +
    theme_minimal() 
  #p + geom_point(aes(color = factor(ActionEvent)), size = 3)
  # p + geom_point(aes (y = yHand),(color = factor(ActionEvent)), size = 3)
  # p + geom_point(aes (y = zHand),(color = factor(ActionEvent)), size = 3)
  p
  # p + scale_y_continuous(limits = c(-1.5,1.5))
  #ggsave("Head Rotation 2D.pdf")
  
  plot_ly(subTrimDF, x = ~xHand, y = ~yHand, z = ~zHand, color = ~ ActionEvent, type = "scatter3d", mode = "markers")
  #plot_ly(subTrimDF, x = ~subTrimDF$EyePos_X, y = ~subTrimDF$EyePos_Y, z = ~subTrimDF$EyePos_Z, color = ~ ActionEvent, type = "scatter3d", mode = "markers")
  
}




# Phase Determination
#___________________________________________________________________________________________________________

# Read in the durationEventDF from GazeDurrationDataCleaning.R

durationEventDF

phaseDF <- subTrimDF %>% mutate(Phase = "none")


for (i in 2:nrow(durationEventDF)) {
  if(durationEventDF$Event[i] == "play_wall" & durationEventDF$Event[i-1] == "build_wall"){
    phaseDF <- phaseDF %>% mutate(Phase = ifelse(Time == durationEventDF$startTime[i],"P1On P5Off",Phase))
  }
}



phaseDF <- phaseDF %>% mutate(Phase = ifelse(ActionEvent == "grabLook","P2On P1Off", Phase))

phaseDF <- phaseDF %>% mutate(Phase = ifelse(ActionEvent == "Grab","P3On P2Off", Phase))

phaseDF <- phaseDF %>% mutate(Phase = ifelse(ActionEvent == "placeLook","P4On P3Off", Phase))


phaseDF <- phaseDF %>% mutate(Phase = ifelse(ActionEvent == "Dropped","P5On P4Off", Phase))



phaseDF <- phaseDF %>% filter(Phase != "none")


