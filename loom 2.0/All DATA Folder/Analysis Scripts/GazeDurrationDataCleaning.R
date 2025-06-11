library(plot3D)
library(rgl)
library(dplyr)
library(bit64)
library(plotly)
setwd("C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Merged Data")



#participantDataFile <- "sdP1_old1.csv"

data_files <- list.files(pattern = "nuP27(\\D|$)")
output_file <- "C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Gaze Merged Data/nuP27_Gaze_Events.csv"  # Change this to your desired output file



gazeDurrationTimes <- data.frame(Time = numeric(),
                                 Participant = factor(),
                                 Condition = factor(),
                                 Trial = numeric(),
                                 Group = factor(),
                                 avgPlayWallDurration = numeric(),
                                 avgViewWAllDurration = numeric(),
                                 avgBuildWallDurration = numeric(),
                                 avgPlayerDurration = numeric(),
                                 playWallCounter = numeric(),
                                 viewWallCounter = numeric(),
                                 buildWallCounter = numeric(),
                                 playerCounter = numeric(),
                                 playWallTotalTime = numeric(),
                                 viewWallTotalTime = numeric(),
                                 buildWallTotalTime = numeric(),
                                 playerTotalTime = numeric(),
                                 stringsAsFactors = FALSE)


durationEventDF <- data.frame(Participant = factor(),
                                 Condition = factor(),
                                 Trial = numeric(),
                                 Group = factor(),
                                 Event = factor(),
                                 viewSwitchCounter = numeric(),
                                 startTime = numeric(),
                                 endTime = numeric(),
                                 eventDuration = numeric(),
                                 stringsAsFactors = FALSE)

AddRow <- function(startTime, endTime, eventDuration, counter, Event, df, df2){
  Participant <- df2[2,2]
  Condition <- df2[8,8]
  Trial <- df2[9,9]
  Group <- df2[7,7]
  viewSwitchCounter <<- viewSwitchCounter + 1
  
  
  
  newRow <- data.frame(Participant, Condition, Trial, Group, Event, viewSwitchCounter, startTime, endTime, eventDuration)
  durationEventDF <<- rbind(durationEventDF, newRow)
}



for(f in 1:length(data_files))
{
  participantDataFile <- data_files[f]
  print(participantDataFile)
  
  
  
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]
  
  
  df$EyePos_X <- as.numeric(df$EyePos_X)
  df$EyePos_Y <- as.numeric(df$EyePos_Y)
  df$EyePos_Z <- as.numeric(df$EyePos_Z)
  
  #trimedDF <- df %>% filter((round(EyePos_X, 2) > -5 & CurrentGazeArea == "play_wall" & round(EyePos_X, 2) < 2.5 ) | CurrentGazeArea == "build_wall"| CurrentGazeArea == "view_wall" | CurrentGazeArea == "background_wall")
  trimedDF <- df
  trimedDF <- df %>%
    filter(EyePos_X > -5) %>%
    filter(EyePos_X != 0) 
  
  trimedDF <- trimedDF %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "background_wall" & EyePos_X == 1.6 & EyePos_Z >= -15 & EyePos_Z <= 16, "play_wall", CurrentGazeArea))
  trimedDF <- trimedDF %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "background_wall" & EyePos_X <= 15.5 & EyePos_X >= 9.5, "view_wall", CurrentGazeArea))
  trimedDF <- trimedDF %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "background_wall" & EyePos_X <= 15.5 & EyePos_X >= 10, "build_wall", CurrentGazeArea))
  
  trimedDF <- trimedDF %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "view_wall" & EyePos_Z > 0 & EyePos_Z < 12, "build_wall", CurrentGazeArea ))
  trimedDF <- trimedDF %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "view_wall" & EyePos_Z > 12, "background_wall", CurrentGazeArea ))
  
  x <- trimedDF[,13]
  y <- trimedDF[,14]
  z <- trimedDF[,15]
  
  
  #plot3d(x, y, z)
  
  
  #-------------------------------------------------------------------------------------------
  # Gaze Durration Analysis
  
  
  lookingForNewGazeDurration = TRUE
  lookingForViewWall = FALSE
  lookingForPlayWall = FALSE
  lookingForBuildWall = FALSE
  lookingForPlayer = FALSE
  
  durationCounter <- 0
  firstRowTime <- 0
  
  playWallTotalTime <- 0
  buildWallTotalTime <- 0
  viewWallTotalTime <- 0
  playerTotalTime <- 0
  
  playWallCounter <- 0
  buildWallCounter <- 0 
  viewWallCounter <- 0
  playerCounter <- 0

  startTime <- 0
  endTime <- 0
  viewSwitchCounter <- 0
  
  
  
  
  for (i in 2:nrow(trimedDF))
  {
    currentEvent <- trimedDF[i,12]
    currentTime <- trimedDF[i,1]
    currentEvent <- toString(currentEvent)
    
    if(lookingForNewGazeDurration)
    {
      if(currentEvent == "view_wall")
      {
        firstRowTime <- currentTime
        lookingForViewWall = TRUE
        lookingForNewGazeDurration = FALSE
      }
      if(currentEvent == "play_wall")
      {
        firstRowTime <- currentTime
        lookingForPlayWall = TRUE
        lookingForNewGazeDurration = FALSE
      }
      if(currentEvent == "build_wall")
      {
        firstRowTime <- currentTime
        lookingForBuildWall = TRUE
        lookingForNewGazeDurration = FALSE
      }
      if(currentEvent == "background_wall" | trimedDF[i,11] == "Partner")
      {
        if(trimedDF[i,11] == "Partner"){

          firstRowTime <- currentTime
          lookingForPlayer = TRUE
          lookingForNewGazeDurration = FALSE
        }else{
          lookingForNewGazeDurration = TRUE
        }
      }
    }
    
    if(!lookingForNewGazeDurration)
    {
      if(lookingForViewWall)
      {
        if(currentEvent == "view_wall")
        {
          durationCounter <- durationCounter +1
          
        }else{
          lookingForNewGazeDurration = TRUE
          lookingForViewWall = FALSE
          
          if(durationCounter>1){
            viewWallCounter <- viewWallCounter +1
            newTime <- currentTime - firstRowTime
            viewWallTotalTime <- viewWallTotalTime +newTime
            AddRow(firstRowTime, currentTime, newTime, viewSwitchCounter, "view_wall", durationEventDF, trimedDF)
            
          }
          durationCounter <- 0
        }
      }
      if(lookingForPlayWall)
      {
        if(currentEvent == "play_wall")
        {
          durationCounter <- durationCounter +1
          
        }else{
          lookingForNewGazeDurration = TRUE
          lookingForPlayWall = FALSE
          
          if(durationCounter>1){
            playWallCounter <- playWallCounter +1
            newTime <- currentTime - firstRowTime
            playWallTotalTime <- playWallTotalTime +newTime
            AddRow(firstRowTime, currentTime, newTime, viewSwitchCounter,"play_wall", durationEventDF, trimedDF)
            
          }
          durationCounter <- 0
        }
      }
      
      if(lookingForBuildWall)
      {
        if(currentEvent == "build_wall")
        {
          durationCounter <- durationCounter +1
          
        }else{
          lookingForNewGazeDurration = TRUE
          lookingForBuildWall = FALSE
          
          if(durationCounter>1){
            buildWallCounter <- buildWallCounter +1
            newTime <- currentTime - firstRowTime
            buildWallTotalTime <- buildWallTotalTime +newTime
            AddRow(firstRowTime, currentTime, newTime, viewSwitchCounter,"build_wall", durationEventDF, trimedDF)
          }
          durationCounter <- 0
        }
      }
      
      if(lookingForPlayer)
      {
       if(trimedDF[i,11] == "Partner"){
         durationCounter <- durationCounter +1

         
        }else{
         lookingForNewGazeDurration = TRUE
         lookingForPlayer = FALSE
         
         if(durationCounter > 1){
           playerCounter <- playerCounter + 1
           newTime <- currentTime - firstRowTime
           if(newTime/10000 > 100){
             playerTotalTime <- playerTotalTime + newTime
             AddRow(firstRowTime, currentTime, newTime, viewSwitchCounter,"Partner", durationEventDF, trimedDF)
           }
           
         }
         durationCounter <- 0
       }
      }
    }
  }
  
  avgPlayWallDurration <- playWallTotalTime/playWallCounter/10000
  avgViewWAllDurration <- viewWallTotalTime/viewWallCounter/10000
  avgBuildWallDurration <- buildWallTotalTime/buildWallCounter/10000
  avgPlayerDurration <- playerTotalTime/playerCounter/10000
  
  
  # print(playWallTotalTime)
  # print(viewWallTotalTime)
  # print(buildWallTotalTime)
  # 
  # print(playWallCounter)
  # print(viewWallCounter)
  # print(buildWallCounter)
  # 
  # print(avgPlayWallDurration)
  # print(avgViewWAllDurration)
  # print(avgBuildWallDurration)
  
  
  Participant <- trimedDF[2,2]
  Condition <- trimedDF[8,8]
  Trial <- trimedDF[9,9]
  Group <- trimedDF[7,7]
  
  
  newPartRow <- data.frame(Participant, Condition, Trial, Group, avgPlayWallDurration, avgViewWAllDurration, 
                           avgBuildWallDurration,avgPlayerDurration, playWallCounter, viewWallCounter, buildWallCounter,playerCounter, playWallTotalTime, viewWallTotalTime, buildWallTotalTime,playerTotalTime)
  
  gazeDurrationTimes <- rbind(gazeDurrationTimes, newPartRow)

}
  


durationEventDFFinal <- data.frame()

previousArea <- ""
currentArea <- ""
lastRow <- FALSE

for (i in 1:nrow(durationEventDF)) {
  currentArea <- durationEventDF$Event[i]
  
  if(i == nrow(durationEventDF) & currentArea != previousArea){
    durationEventDFFinal <- rbind(durationEventDFFinal, durationEventDF[i-1,])
    durationEventDFFinal <- rbind(durationEventDFFinal, durationEventDF[i,])
    print("switch")
    break
    
  }else if(i == nrow(durationEventDF) & currentArea == previousArea){
    durationEventDF$startTime[i] <- durationEventDF$startTime[i-1]
    durationEventDFFinal <- rbind(durationEventDFFinal, durationEventDF[i,])
    break
    
  }
  
  
  if(i == 1){
    previousArea <- currentArea
    #durationEventDFFinal <- rbind(durationEventDFFinal, durationEventDF[i,])
  }else if(currentArea == previousArea){
    durationEventDF$startTime[i] <- durationEventDF$startTime[i-1]
    previousArea <- currentArea
    
  }else if(currentArea != previousArea){
    durationEventDFFinal <- rbind(durationEventDFFinal, durationEventDF[i-1,])
    previousArea <- currentArea

  }else{
    print("WHAA")
  }

  
}

durationEventDFFinal <- durationEventDFFinal %>% mutate(eventDuration = (endTime - startTime)/10000)

write.csv(durationEventDFFinal, output_file, row.names = FALSE)
print(output_file)


#write.csv(gazeDurrationTimes, "GazeDurrationTimes 9_2_24.csv, row.names = FALSE)


