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
data_files <- list.files(pattern = "nuP15_merged")
participantDataFile <- data_files[1]
print(participantDataFile)

df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
df <- df[!duplicated(df$Time), ]

data_files2 <- list.files(pattern = "nuP15_PAC")
participantDataFile2 <- data_files2[1]
print(participantDataFile2)

PACdf <- read.csv(participantDataFile2, colClasses=c("PACstartTime" = "integer64",
                                                     "PACendTime" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)

#MIS-LABELED DATA FIXES ------------------------------------------------------------------------------------------------
#if(df$Group[i] == "1" | df$Group[i] == "f"| is.na(df$Group[i]) | df$Participant[i] == "nuP20"| df$Participant[i] == "nuP17"){
if(df$Group[i] == "1" | df$Group[i] == "f"| is.na(df$Group[i])){

  df$Group[i] <- "e"
}
if(df$Participant[i] == "sdP13"| df$Participant[i] == "sdP2"){
  df$Group[i] <- "c"

}
if(df$Participant[i] == "sd12" | df$Participant[i] == "nuP15"| df$Participant[i] == "sdP7"| df$Participant[i] == "sdP15"){
  df$Sex[i] <- "m"
}

if(df$Participant[i] == "nuP18" | df$Participant[i] == "nuP19"){
  df$Sex[i] <- "f"
}

if(df$Participant[i] == "sdP13"){
  df$Age[i] <- 20
}
if(df$Participant[i] == "sdP1"){
  df$Age[i] <- 21
}

if(df$Participant[1] == "sd10"){
  df <- df %>% mutate(Group = ifelse(Group == 1, "e", Group))
}
if(df$Participant[1] == "sdP7"){
  df <- df %>% mutate(Sex = "m")
}


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


plot3d(x, y, z)


#write.csv(merged_data, output_file, row.names = FALSE)
