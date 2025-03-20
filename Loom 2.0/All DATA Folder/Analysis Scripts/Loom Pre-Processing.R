# Loom Pre-Processing
# This script is designed to take in the raw data that has already passed through the Unity Rotation Conversion Processing
# Here we will"
# 1. Clean the Gaze Data   2. Fix mis-labeled particpant data  3. Filter unwanted Participants
# and Also:
# Assign in Game event Markers that were not in the raw data

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



# FILE READ ----------------------------------------------------------------------------------------------------------------
 
Patterns <- c("sdP8", "sdP9", "sdP10", "sdP11",
              "sdP12", "sdP13", "sdP14", "sdP15", "sdP10")


Pattern <- ""
for (t in Patterns) {
    Pattern <- t
    print(Pattern)
    
  output_file <- "C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Processed Data/"  # Change this to your desired output file
    
    
  output_file2 <- paste(output_file, Pattern, ".csv", sep = "")
  
  Pattern <- paste(Pattern ,"(\\D|$)", sep = "")
  print(Pattern)

  
  raw_Data_Directory <- "C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Merged Data"
  PAC_Data_Directory <- "C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/PAC Merged Data"
  gaze_Data_Directory <- "C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Gaze Merged Data"
  
  data_files <- list.files(path = raw_Data_Directory, pattern = Pattern, full.names = TRUE)
  participantDataFile <- data_files[1]
  print(participantDataFile)
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  data_files2 <- list.files(path = PAC_Data_Directory, pattern = Pattern, full.names = TRUE)
  participantDataFile2 <- data_files2[1]
  print(participantDataFile2)
  PACdf <- read.csv(participantDataFile2, colClasses=c("PACstartTime" = "integer64",
                                                       "PACendTime" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  data_files3 <- list.files(path = gaze_Data_Directory, pattern = Pattern, full.names = TRUE)
  participantDataFile3 <- data_files3[1]
  print(participantDataFile3)
  gaze_df <- read.csv(participantDataFile3, colClasses=c("startTime" = "integer64",
                                                       "endTime" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  #MIS-LABELED DATA FIXES ------------------------------------------------------------------------------------------------
  #if(df$Group[i] == "1" | df$Group[i] == "f"| is.na(df$Group[i]) | df$Participant[i] == "nuP20"| df$Participant[i] == "nuP17"){
  
  
  
  df <- df %>% mutate(Group = ifelse(Group == "f" | Group == "1","e", Group))
  
  df <- df %>% mutate(Group = ifelse(Participant == "sdP13" | Participant == "sdP13","c", Group))
  
  df <- df %>% mutate(Sex = ifelse(Participant == "sdP12" | Participant == "nuP15" | Participant == "sdP7" | Participant == "sdP15","m", Sex))
  
  df <- df %>% mutate(Sex = ifelse(Participant == "nuP18" | Participant == "nuP19","f", Sex))
  
  df <- df %>% mutate(Age = ifelse(Participant == "sdP13",20, Age))
  
  df <- df %>% mutate(Age = ifelse(Participant == "sdP1",21, Age))
  
  df <- df %>% mutate(Group = ifelse(Participant == "sdP10","e", Group))
  
  df <- df %>% mutate(Sex = ifelse(Participant == "sdP7","m", Sex))
  
  
  
  # GAME EVENT ASSIGNMENT ------------------------------------------------------------------------------------------
  
  df$Time[is.na(df$Time)] <- 0
  df$Time <- as.integer64(df$Time)
  
  participantID <- df$Participant[1]
  PACdf <- PACdf %>% filter(Participant == participantID)
  
  trimDF <- df %>% dplyr :: filter(EyePos_X != "N/A")
  
  trimDF <- trimDF %>%
    group_by(Condition, Trial) %>%
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
  
  
  
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(grepl("dropped by player", Event), "Cube Dropped", ActionEvent))
  
  filtDF <- trimDF %>% filter(ActionEvent == "Dropped")
  
  
  for (c in 1:nrow(filtDF)) {
  
    input_string <- filtDF$Event[c]
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
  
    # trimDF <- trimDF %>% mutate(sequenceNum = ifelse(currentPACstartTime == Time & currentEvent == "grab", g,sequenceNum))
    # trimDF <- trimDF %>% mutate(sequenceNum = ifelse(currentPACendTime == Time & currentEvent == "grab", g,sequenceNum))
    # trimDF <- trimDF %>% mutate(sequenceNum = ifelse(currentPACstartTime == Time & currentEvent == "place", g,sequenceNum))
    # trimDF <- trimDF %>% mutate(sequenceNum = ifelse(currentPACendTime == Time & currentEvent == "place", g,sequenceNum))
    #
    trimDF <- trimDF %>% mutate(ActionEvent = ifelse(currentPACstartTime == Time & currentEvent == "grab", "grabLook",ActionEvent))
    trimDF <- trimDF %>% mutate(ActionEvent = ifelse(currentPACstartTime == Time & currentEvent == "place", "placeLook",ActionEvent))
  
  
  }
  
  trimDF <- trimDF %>%
    mutate(Size = ifelse(ActionEvent == "Grab" | ActionEvent == "Dropped"| ActionEvent == "grabLook"| ActionEvent == "placeLook"
                         | ActionEvent == "DropStart"| ActionEvent == "DropZone Hit"| ActionEvent == "P2 Drop" | ActionEvent == "P2 Grab", 3, 1))
  
  
  # GAZE DATA CLEANING----------------------------------------------------------------------------------------------------------
  
  trimDF$EyePos_X <- as.numeric(trimDF$EyePos_X)
  trimDF$EyePos_Y <- as.numeric(trimDF$EyePos_Y)
  trimDF$EyePos_Z <- as.numeric(trimDF$EyePos_Z)
  
  #trimedDF <- df %>% filter((round(EyePos_X, 2) > -5 & CurrentGazeArea == "play_wall" & round(EyePos_X, 2) < 2.5 ) | CurrentGazeArea == "build_wall"| CurrentGazeArea == "view_wall" | CurrentGazeArea == "background_wall")
  #trimedDF <- df
  trimDF <- trimDF %>%
    filter(EyePos_X > -5) %>%
    filter(EyePos_X != 0) 
  
  trimDF <- trimDF %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "background_wall" & EyePos_X == 1.6 & EyePos_Z >= -15 & EyePos_Z <= 16, "play_wall", CurrentGazeArea))
  trimDF <- trimDF %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "background_wall" & EyePos_X <= 15.5 & EyePos_X >= 9.5, "view_wall", CurrentGazeArea))
  trimDF <- trimDF %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "background_wall" & EyePos_X <= 15.5 & EyePos_X >= 10, "build_wall", CurrentGazeArea))
  
  trimDF <- trimDF %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "view_wall" & EyePos_Z > 0 & EyePos_Z < 12, "build_wall", CurrentGazeArea ))
  trimDF <- trimDF %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "view_wall" & EyePos_Z > 12, "background_wall", CurrentGazeArea ))
  
  x <- trimDF$EyePos_X
  y <- trimDF$EyePos_Y
  z <- trimDF$EyePos_Z
  
  
  trimDF$GazeEvent <- "none"
  startVal <- ""
  endVal <- ""
  
  currentStartTime <- ""
  currentEndTime <- ""
  currentEvent <- ""
  
  for (g in 1:nrow(gaze_df)) {
    currentStartTime <- gaze_df$startTime[g]
    currentEndTime <- gaze_df$endTime[g]
    currentEvent <- gaze_df$Event[g]
    print(currentEvent)
      
    if(currentEvent == "play_wall"){
      startVal <- "play_wall_start"
      endVal <- "play_wall_end"
      trimDF <- trimDF %>% mutate(GazeEvent = ifelse(Time == currentStartTime, startVal, GazeEvent))
      trimDF <- trimDF %>% mutate(GazeEvent = ifelse(Time == currentEndTime, endVal, GazeEvent))
      
      
    }
    if(currentEvent == "build_wall"){
      startVal <- "build_wall_start"
      endVal <- "build_wall_end"
      trimDF <- trimDF %>% mutate(GazeEvent = ifelse(Time == currentStartTime, startVal, GazeEvent))
      trimDF <- trimDF %>% mutate(GazeEvent = ifelse(Time == currentEndTime, endVal, GazeEvent))
    }
    if(currentEvent == "view_wall"){
      startVal <- "view_wall_start"
      endVal <- "view_wall_end"
      trimDF <- trimDF %>% mutate(GazeEvent = ifelse(Time == currentStartTime, startVal, GazeEvent))
      trimDF <- trimDF %>% mutate(GazeEvent = ifelse(Time == currentEndTime, endVal, GazeEvent))
    }
    
  
  }
  
  # Phase Determination
  #___________________________________________________________________________________________________________
  
  # Read in the durationEventDF from GazeDurrationDataCleaning.R
  
  #durationEventDF
  
  trimDF$Phase <- "none"
  
  
  # for (i in 2:nrow(durationEventDF)) {
  #   if(durationEventDF$Event[i] == "play_wall" & (durationEventDF$Event[i-1] == "build_wall")){
  #     phaseDF <- phaseDF %>% mutate(Phase = ifelse(Time == durationEventDF$startTime[i],"Phase 1",Phase))
  #   }
  # }
  
  trimDF <- trimDF %>% mutate(Phase = ifelse(GazeEvent == "build_wall_end","Phase 1", Phase))
  
  
  
  
  trimDF <- trimDF %>% mutate(Phase = ifelse(ActionEvent == "grabLook","Phase 2", Phase))
  
  trimDF <- trimDF %>% mutate(Phase = ifelse(ActionEvent == "Grab","Phase 3", Phase))
  
  trimDF <- trimDF %>% mutate(Phase = ifelse(ActionEvent == "placeLook","Phase 4", Phase))
  
  
  trimDF <- trimDF %>% mutate(Phase = ifelse(ActionEvent == "Dropped","Phase 5", Phase))
  
  trimDF <- trimDF %>% mutate(Phase = ifelse(GazeEvent == "view_wall_start","Int Phase Start", Phase))
  trimDF <- trimDF %>% mutate(Phase = ifelse(GazeEvent == "view_wall_end","Int Phase End", Phase))
  
  trimDF <- trimDF %>% mutate(Phase = ifelse(ActionEvent == "Cube Dropped","Cube Dropped", Phase))
  
  phaseDF <- trimDF %>% filter(Phase != "none")
  
  # upperTime <- 131.72188
  # lowerTime <- 134.96704
  # 
  # phaseDF <- phaseDF %>% filter(ModTime >= upperTime & ModTime <= lowerTime)
  
  
  write.csv(trimDF, output_file2, row.names = FALSE)
}

#write.csv(merged_data, output_file, row.names = FALSE)
