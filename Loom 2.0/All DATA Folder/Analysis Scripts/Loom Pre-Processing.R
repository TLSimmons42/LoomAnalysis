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

data_files <- list.files(pattern = "nuP15_PAC")
participantDataFile <- data_files[1]
print(participantDataFile)

PACdf <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
PACdf <- df[!duplicated(df$Time), ]

# MIS-LABELED DATA FIXES ------------------------------------------------------------------------------------------------
#if(df$Group[i] == "1" | df$Group[i] == "f"| is.na(df$Group[i]) | df$Participant[i] == "nuP20"| df$Participant[i] == "nuP17"){
# if(df$Group[i] == "1" | df$Group[i] == "f"| is.na(df$Group[i])){
#   
#   df$Group[i] <- "e"
# }
# if(df$Participant[i] == "sdP13"| df$Participant[i] == "sdP2"){
#   df$Group[i] <- "c"
#   
# }
# if(df$Participant[i] == "sd12" | df$Participant[i] == "nuP15"| df$Participant[i] == "sdP7"| df$Participant[i] == "sdP15"){
#   df$Sex[i] <- "m"
# }
# 
# if(df$Participant[i] == "nuP18" | df$Participant[i] == "nuP19"){
#   df$Sex[i] <- "f"
# }
# 
# if(df$Participant[i] == "sdP13"){
#   df$Age[i] <- 20
# }
# if(df$Participant[i] == "sdP1"){
#   df$Age[i] <- 21
# }
# 
# if(df$Participant[1] == "sd10"){
#   df <- df %>% mutate(Group = ifelse(Group == 1, "e", Group))
# }
# if(df$Participant[1] == "sdP7"){
#   df <- df %>% mutate(Sex = "m")
# }


# gAME EVENT ASSIGNMENT ------------------------------------------------------------------------------------------

df$Time[is.na(df$Time)] <- 0
df$Time <- as.integer64(df$Time)

participantID <- df$Participant[1]
PACdf <- PACdf %>% filter(Participant == participantID)

trimdf <- df %>% dplyr :: filter(EyePos_X != "N/A")

trimdf <- trimdf %>%
  group_by(Condition, Trial)
  mutate(ModTime = (Time - Time[1])/10000000) 

trimdf <- trimdf %>%
  filter(as.numeric(EyePos_X) > -1)


trimdf <- trimdf %>%
  filter(as.numeric(HandPos_X) > 0)

trimdf <- trimdf %>%
  mutate(ActionEvent = ifelse(grepl("picked", Event), Event, "temp"))
trimdf <- trimdf %>%
  mutate(ActionEvent = ifelse(grepl("Player 1 hits", Event), Event, ActionEvent))

trimdf <- trimdf %>%
  mutate(ActionEvent = ifelse(grepl("P2", ActionEvent), "temp", ActionEvent))

trimdf <- trimdf %>%
  mutate(ActionEvent = ifelse(grepl("P1", ActionEvent), "temp", ActionEvent))

trimdf <- trimdf %>%
  mutate(ActionEvent = ifelse(ActionEvent != "temp", "Grab", ActionEvent))

trimdf <- trimdf %>%
  mutate(ActionEvent = ifelse(grepl("dropped", Event), Event, ActionEvent))

trimdf <- trimdf %>%
  mutate(ActionEvent = ifelse(grepl("player", ActionEvent), "temp", ActionEvent))

trimdf <- trimdf %>%
  mutate(ActionEvent = ifelse(grepl("P2", ActionEvent), "temp", ActionEvent))

trimdf <- trimdf %>%
  mutate(ActionEvent = ifelse(grepl("P1", ActionEvent), "temp", ActionEvent))

trimdf <- trimdf %>%
  mutate(ActionEvent = ifelse(grepl("dropped", ActionEvent), "Dropped", ActionEvent))

trimdf <- trimdf %>%
  mutate(ActionEvent = ifelse(ActionEvent == "Grab" | ActionEvent == "Dropped", ActionEvent, CurrentGazeArea))

trimdf <- trimdf %>%
  mutate(ActionEvent = ifelse(grepl("Drop Hit", Event), "DropZone Hit", ActionEvent))
trimdf <- trimdf %>%
  mutate(ActionEvent = ifelse(grepl("P2", Event) & grepl("dropped", Event), "P2 Drop", ActionEvent))
trimdf <- trimdf %>%
  mutate(ActionEvent = ifelse(grepl("P2", Event) & grepl("picked", Event), "P2 Grab", ActionEvent))

filterdf <- trimdf %>% filter(ActionEvent == "Dropped")

trimdf <- trimdf %>%
  mutate(ActionEvent = ifelse(grepl("dropped by player", Event), "Cube Dropped", ActionEvent))

