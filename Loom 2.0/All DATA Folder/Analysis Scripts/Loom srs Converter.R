# Load required packages
library(dplyr)
library(ggplot2)
library(signal)  # For Butterworth dplyr::filter
library(plotly)
library(bit64)
library(pracma)  # For rad2deg function
setwd("C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Processed Data")


data_files <- list.files(pattern = "nuP25(\\D|$)")
#data_files <- list.files(pattern = ".csv")

for(f in 1:length(data_files))
{
  
  participantDataFile <- data_files[f]
  print(participantDataFile)
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]

  
  print(unique(df$Participant))
  print(unique(df$Group))
  print(unique(df$Age))
  print(unique(df$Condition))
  print(unique(df$Trial))
  print(unique(df$Sex))
  

}

srsData <- srsData %>% tibble::rowid_to_column("row_id")
srsData <- srsData %>% select(-row_id)


srsFinal <- rbind(srsFinal, srsData[3, ])
srsData <- srsData[-3, ]


write.csv(df, participantDataFile, row.names = FALSE)



srsFinal <- srsFinal %>% mutate(ClientId = ifelse(ClientName == "Brian Eckelmen", "nuP25", ClientId))
srsFinal <- srsFinal %>% mutate(Gender = ifelse(ClientName == "Brian Eckelmen", "female", AgeAtTesting))

srsFinal <- srsFinal %>% filter(ClientId != "nuP016")
df <- df %>% mutate(Participant = "sdP15")
df <- df %>% mutate(Age = 18)
df <- df %>% mutate(Sex = "m")
df <- df %>% mutate(Group = "e")
df <- df %>% filter(Condition != "")

srsFinal <- srsFinal %>% mutate(Gender = ifelse(ClientName == "Brian Eckelmen", "female", AgeAtTesting))

 
