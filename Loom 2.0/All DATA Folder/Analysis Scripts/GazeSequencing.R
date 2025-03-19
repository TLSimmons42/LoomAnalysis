library(plot3D)
library(rgl)
library(dplyr)
library(bit64)
library(scatterplot3d)
library(ggplot2)

data_files <- list.files(pattern = "nuP15.csv")
data_files[]

gazeDurrationTimes <- data.frame(Time = numeric(),
                                 Participant = factor(),
                                 Condition = factor(),
                                 Trial = numeric(),
                                 stringsAsFactors = FALSE)


sequenceDF <- data.frame(Time = numeric(),
                          Participant = factor(),
                          Condition = factor(),
                          Trial = numeric(),
                          Event = factor(),
                          sequenceCounter = numeric(),
                          sequenceType = factor(),
                          sequenceSubType = factor(),
                          stringsAsFactors = FALSE)



for(f in 1:length(data_files))
{
  participantDataFile <- data_files[f]
  print(participantDataFile)
  
  
  
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]
  
  Participant <- df[2,2]
  Condition <- df[8,8]
  Trial <- df[9,9]
  
  sequenceCounter <- 0
  
  lookingForNewSequence <- TRUE
  lookingForS1start <- FALSE
  lookingForS1a <- FALSE
  lookingForS1b <- FALSE
  lookingForS1c <- FALSE
  lookingForS1d <- FALSE
  lookingForS1End <- FALSE
  

  for(i in 1:nrow(df))
  {
    currentGazeArea <- df$CurrentGazeArea[i]
    currentGazeTarget <- df$CurrentGazeTarget[i]
    currentTime <- df$Time[i]
    currentEvent <- df$Event[i]
    
    if(lookingForS1a)
    {
      if(currentGazeArea == "play_wall"){
        
      }else if(currentGazeArea == "background_wall"){
        sequenceType <- "P2V"
        sequenceSubType <- "P2V_a"
        
        newPartRow <- data.frame(currentTime, Participant, Condition, Trial, currentEvent, sequenceCounter, sequenceType, sequenceSubType)
        sequenceDF <- rbind(sequenceDF, newPartRow)
        
        lookingForS1b <- TRUE
        lookingForS1a <- FALSE
      }else{
        lookingForNewSequence <- TRUE
        lookingForS1a <- FALSE

      }
    }
    
    if(lookingForS1b)
    {
      if(currentGazeArea == "background_wall"){
        
      }else if(currentGazeArea == "view_wall"){
        sequenceType <- "P2V"
        sequenceSubType <- "P2V_b"
        
        newPartRow <- data.frame(currentTime, Participant, Condition, Trial, currentEvent, sequenceCounter, sequenceType, sequenceSubType)
        sequenceDF <- rbind(sequenceDF, newPartRow)
        
        lookingForS1c <- TRUE
        lookingForS1b <- FALSE
      }else{
        lookingForNewSequence <- TRUE
        lookingForS1b <- FALSE

        
      }
    }
    
    if(lookingForS1c)
    {
      if(currentGazeArea == "view_wall"){
        
      }else if(currentGazeArea == "background_wall"){

        sequenceType <- "P2V"
        sequenceSubType <- "P2V_c"
        
        newPartRow <- data.frame(currentTime, Participant, Condition, Trial, currentEvent, sequenceCounter, sequenceType, sequenceSubType)
        sequenceDF <- rbind(sequenceDF, newPartRow)
        
        lookingForS1d <- TRUE
        lookingForS1c <- FALSE
      }else{
        lookingForNewSequence <- TRUE
        lookingForS1c <- FALSE
        
      }
    }
    
    if(lookingForS1d)
    {
      if(currentGazeArea == "background_wall"){
        
      }else if(currentGazeArea == "play_wall"){

        sequenceType <- "P2V"
        sequenceSubType <- "P2V_d"
        
        newPartRow <- data.frame(currentTime, Participant, Condition, Trial, currentEvent, sequenceCounter, sequenceType, sequenceSubType)
        sequenceDF <- rbind(sequenceDF, newPartRow)
        
        lookingForS1End <- TRUE
        lookingForS1d <- FALSE
      }else{
        lookingForNewSequence <- TRUE
        lookingForS1d <- FALSE
        
      }
    }
    if(lookingForS1End)
    {
      if(currentGazeArea == "play_wall"){
        
      }else{

        sequenceType <- "P2V"
        sequenceSubType <- "P2V_end"
        
        newPartRow <- data.frame(currentTime, Participant, Condition, Trial, currentEvent, sequenceCounter, sequenceType, sequenceSubType)
        sequenceDF <- rbind(sequenceDF, newPartRow)
        
        lookingForNewSequence <- TRUE
        lookingForS1End <- FALSE
      }
    }
    
    
    
    if(lookingForNewSequence)
    {
      if(currentGazeArea == "play_wall")
      {
        sequenceCounter <- sequenceCounter + 1
        sequenceType <- "P2V"
        sequenceSubType <- "P2V_start"
        newPartRow <- data.frame(currentTime, Participant, Condition, Trial, currentEvent, sequenceCounter, sequenceType, sequenceSubType)
        sequenceDF <- rbind(sequenceDF, newPartRow)
        lookingForS1a <- TRUE
        lookingForNewSequence <- FALSE
      }
    }

  }
  
}

subDF <- sequenceDF %>% filter(sequenceSubType == "P2V_end")
fullSubDF <- sequenceDF  %>% filter(sequenceDF$sequenceCounter == subDF$sequenceCounter[1])

entireSequenceDF <- df %>% filter(Time >= fullSubDF$currentTime[1] &  Time <= fullSubDF$currentTime[6])



entireSequenceDF$EyePos_X <- as.numeric(entireSequenceDF$EyePos_X)
entireSequenceDF$EyePos_Y <- as.numeric(entireSequenceDF$EyePos_Y)
entireSequenceDF$EyePos_Z <- as.numeric(entireSequenceDF$EyePos_Z)

#shortDF <- df[(df$CurrentGazeArea == "play_wall" & df$EyePos_X > (0)),]

# x <- df[,13]
# y <- df[,14]
# z <- df[,15]

x <- entireSequenceDF[,13]
y <- entireSequenceDF[,14]
z <- entireSequenceDF[,15]

entireSequenceDF$CurrentGazeArea <- gsub("play_wall", "blue", entireSequenceDF$CurrentGazeArea,ignore.case = TRUE)
entireSequenceDF$CurrentGazeArea <- gsub("background_wall", "green", entireSequenceDF$CurrentGazeArea)
entireSequenceDF$CurrentGazeArea <- gsub("view_wall", "red", entireSequenceDF$CurrentGazeArea)

# 
# 
# scatterplot3d(
#   x = x,
#   y = y,
#   z = z,
#   color = entireSequenceDF$CurrentGazeArea,  # Use numeric values for color
#   pch = 16,
#   type = "b",
#   angle = 60,
#   main = "3D Scatter Plot with Colors by Group",
#   xlab = "X",
#   ylab = "Y",
#   zlab = "Z",
#   color.legend = TRUE
# )
