# This script wrangles the post-processing data from the RL competitive analysis, sorts it into sections of gaze strategy
# and analyses the group data

library(plot3D)
library(rgl)
library(dplyr)
library(bit64)
library(stringr)
library(tidyr)
library(ggplot2)


data_files <- list.files(pattern = "P28.csv")

participantDataFile <- data_files[1]
print(participantDataFile)

df <- Cleaned_Data

dfTrim <- df %>% filter(V5 == "P01" | V5 == "P02" | V5 == "P03" | V5 == "P04" | 
                          V5 == "P05" | V5 == "P06" | V5 == "P07" | V5 == "P08" | 
                          V5 == "P09" | V5 == "P12" | V5 == "P13" | V5 == "P14" | 
                          V5 == "P15" | V5 == "P16" | V5 == "P17" | V5 == "P18" | 
                          V5 == "P19" | V5 == "P21" | V5 == "P22" | V5 == "P23" | 
                          V5 == "P24" | V5 == "P25" | V5 == "P26" | V5 == "P27" | 
                          V5 == "P28" | V5 == "P29" | V5 == "P31" | V5 == "P32" | 
                          V5 == "P33" | V5 == "P36" | V5 == "P37" | V5 == "P39" | 
                          V5 == "P41" | V5 == "P44" | V5 == "P45" | V5 == "P49" | 
                          V5 == "P50" | V5 == "P52")
