library(plot3D)
library(rgl)
library(dplyr)
library(bit64)
library(stringr)



data_files <- list.files(pattern = "P10_old6")

PACdf <- data.frame(Time = numeric(),
                    Participant = factor(),
                    Condition = factor(),
                    Trial = numeric(),
                    grabPACcount = numeric(),
                    placePACcount = numeric(),
                    avgGrabPAC = numeric(),
                    avgPlacePAC = numeric(),
                    grabAreaPACcount = numeric(),
                    placeAreaPACcount = numeric(),
                    avgGrabAreaPAC = numeric(),
                    avgPlaceAreaPAC = numeric(),
                    stringsAsFactors = FALSE)


individualPACdf <- data.frame(Time = numeric(),
                              Participant = factor(),
                              Condition = factor(),
                              Trial = numeric(),
                              PACtype = factor(),
                              PACtime = numeric(),
                              PACstartTime = numeric(),
                              PACendTime = numeric(),
                              Event = factor(),
                              stringsAsFactors = FALSE)




for(f in 1:length(data_files))
{
  participantDataFile <- data_files[f]
  print(participantDataFile)
  
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]
  df$Event <- gsub("\\(|\\)", "", df$Event)
  df$CurrentGazeTarget <- gsub("\\(|\\)", "", df$CurrentGazeTarget)
  gameOverTime <- df[df$Event == "Game Over",]
  df <- df %>% filter(df$Time <= gameOverTime[1,1])
  
  
  # Find all of the Picked events
  trimedGrabDF <- df[grep("picked", df$Event), ]
  #trimedGrabDF <- trimedGrabDF %>% filter(CurrentGazeArea == "play_wall")
  trimedGrabDF <- trimedGrabDF %>% filter(!grepl("by", Event))
  
  
  trimedPlaceDF <- df[grep("placed", df$Event), ]
  trimedPlaceDF <- trimedPlaceDF %>% filter(!grepl("Hit", Event))
  trimedPlaceDF <- trimedPlaceDF %>% filter(!grepl("HB", Event))
  trimedPlaceDF <- trimedPlaceDF[!duplicated(trimedPlaceDF$Event), ]
  #trimedPlaceDF <- trimedPlaceDF %>% filter(CurrentGazeArea == "build_wall")
  
  
  if(df[1,8] != "solo"){
    trimedP2GrabDF <- df[grep("picked", df$Event), ]
    trimedP2GrabDF <- trimedP2GrabDF %>% filter(grepl("P2", Event))
    
    for(b in 1:nrow(trimedP2GrabDF))
    {
      input_string <- trimedP2GrabDF[b,10]
      
      # Find the position of " was picked up"
      pos <- regexpr(" was picked up by P2", input_string)
      
      # Extract the substring before the target text
      result <- substr(input_string, 1, pos - 1)
      result <- paste(result, "was", sep = "")
      #result <- gsub("(Clone)", "Clone", result)
      
      
      trimedPlaceDF <- trimedPlaceDF %>% filter(!grepl(result, Event))
      
    }
    
    
    
    for(i in 1:nrow(trimedGrabDF))
    {
      currentTime <- trimedGrabDF[i,1]
      currentGazeArea <- trimedGrabDF[i,12]
      currentEvent <- trimedGrabDF[i,10]
      

      subDFpre <- df %>% filter(Time > (currentTime-20000000) & Time <= currentTime)
      subDFpost <- df %>% filter(Time < (currentTime+20000000) & Time >= currentTime)
      
      
      Participant <- trimedGrabDF[2,2]
      Condition <- trimedGrabDF[8,8]
      Trial <- trimedGrabDF[9,9]
      
      
    }
    
    
  }
  
  
  
  
  
  
}