library(plot3D)
library(rgl)
library(dplyr)
library(bit64)

combindedDataFile <- "combindedDataFile P10.csv"

df <- read.csv(combindedDataFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
df <- df[!duplicated(df), ]


singleGazeTransferDF <- data.frame(TimeStamp = numeric(),
                          Participant = factor(),
                          Condition = factor(),
                          Trial = numeric(),
                          Age = numeric(),
                          Gender = factor(),
                          Group = factor(),
                          TransferEvent = factor(),
                          StartLocation = factor(),
                          EndLocation = numeric(),
                          StartPosX = numeric(),
                          StartPosY = numeric(),
                          StartPosZ = numeric(),
                          EndPosX = numeric(),
                          EndPosY = numeric(),
                          EndPosZ = numeric(),
                          stringsAsFactors = FALSE)



df <- df %>% filter(!is.na(areaEvent))
df <- df %>% filter(!is.na(zAreaPos))
df <- df %>% filter(areaEvent == "looking at Play wall" | areaEvent == "looking at Build wall" | areaEvent == "looking at View wall")


gazeTransfer <- function(df){
  df <- df %>% filter()
}