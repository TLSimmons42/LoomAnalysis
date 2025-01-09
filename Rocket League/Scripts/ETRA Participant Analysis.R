# This script wrangles the post-processing data from the RL competitive analysis, sorts it into sections of gaze strategy
# and analyses the group data

library(plot3D)
library(rgl)
library(dplyr)
library(bit64)
library(stringr)
library(tidyr)
library(ggplot2)


data_files <- list.files(pattern = "Data.csv")

participantDataFile <- data_files[1]
print(participantDataFile)

df <- read.csv(participantDataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)

dfTrim <- df %>% filter(Participant.ID == "P01" | Participant.ID == "P02" | Participant.ID == "P03" | Participant.ID == "P04" | 
                          Participant.ID == "P05" | Participant.ID == "P06" | Participant.ID == "P07" | Participant.ID == "P08" | 
                          Participant.ID == "P09" | Participant.ID == "P12" | Participant.ID == "P13" | Participant.ID == "P14" | 
                          Participant.ID == "P15" | Participant.ID == "P16" | Participant.ID == "P17" | Participant.ID == "P18" | 
                          Participant.ID == "P19" | Participant.ID == "P21" | Participant.ID == "P22" | Participant.ID == "P23" | 
                          Participant.ID == "P24" | Participant.ID == "P25" | Participant.ID == "P26" | Participant.ID == "P27" | 
                          Participant.ID == "P28" | Participant.ID == "P29" | Participant.ID == "P31" | Participant.ID == "P32" | 
                          Participant.ID == "P33" | Participant.ID == "P36" | Participant.ID == "P37" | Participant.ID == "P39" | 
                          Participant.ID == "P41" | Participant.ID == "P44" | Participant.ID == "P45" | Participant.ID == "P49" | 
                          Participant.ID == "P50" | Participant.ID == "P52"| Participant.ID == "P42"| Participant.ID == "P11"| Participant.ID == "P38")


dfExpert <- dfTrim %>% filter(Participant.ID == "P05" | Participant.ID == "P06" | Participant.ID == "P11" | Participant.ID =="P14" | Participant.ID =="P15"| Participant.ID == "P16"| Participant.ID == "P17"
                              | Participant.ID =="P24"| Participant.ID == "P26" | Participant.ID =="P39" | Participant.ID =="P42"| Participant.ID == "P43" | Participant.ID =="P45")
mean(dfExpert$Age)