# Load required packages
library(dplyr)
library(ggplot2)
library(signal)  # For Butterworth dplyr::filter
library(plotly)
library(bit64)
library(pracma)  # For rad2deg function
setwd("C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Processed Data")


data_files <- list.files(pattern = "nuP16(\\D|$)")
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

srs.2_export_2022.10.01_2025.06.19_8784e2cc.ff23.49df.89c3.3759619d9661 <- srs.2_export_2022.10.01_2025.06.19_8784e2cc.ff23.49df.89c3.3759619d9661 %>% tibble::rowid_to_column("row_id")
srs.2_export_2022.10.01_2025.06.19_8784e2cc.ff23.49df.89c3.3759619d9661 <- srs.2_export_2022.10.01_2025.06.19_8784e2cc.ff23.49df.89c3.3759619d9661 %>% select(-row_id)


srsFinal <- rbind(srsFinal, srs.2_export_2022.10.01_2025.06.19_8784e2cc.ff23.49df.89c3.3759619d9661[29, ])
srsData <- srsData[-3, ]


write.csv(df, participantDataFile, row.names = FALSE)



srsFinal <- srsFinal %>% mutate(ClientId = ifelse(ClientName == "Emyr Eiler", "nuP16", ClientId))
srsFinal <- srsFinal %>% mutate(ClientName = ifelse(ClientName == "Yijun Qian", "Erica Rice", ClientName))

srsFinal <- srsFinal %>% mutate(Gender = ifelse(ClientName == "Brian Eckelmen", "female", AgeAtTesting))

srsFinal <- srsFinal %>% filter(ClientId != "nuP016")
df <- df %>% mutate(Participant = "nuP34")
df <- df %>% mutate(Age = 18)
df <- df %>% mutate(Sex = "m")
df <- df %>% mutate(Group = "e")
df <- df %>% filter(Condition != "")

srsFinal <- srsFinal %>% mutate(Gender = ifelse(ClientName == "Omkar Bhope", "female", AgeAtTesting))
srsFinal <- srsFinal[!duplicated(srsFinal$ClientId), ]

 
write.csv(srsFinal, file = "C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/SICK DATA FRAMES/srsFinal.csv", row.names = FALSE)
