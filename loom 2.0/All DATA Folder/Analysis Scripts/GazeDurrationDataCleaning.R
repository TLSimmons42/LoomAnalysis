library(plot3D)
library(rgl)
library(dplyr)
library(bit64)



#participantDataFile <- "sdP1_old1.csv"

data_files <- list.files(pattern = "sdP10_old4")
data_files[]

gazeDurrationTimes <- data.frame(Time = numeric(),
                                 Participant = factor(),
                                 Condition = factor(),
                                 Trial = numeric(),
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
                                 Event = factor(),
                                 viewSwitchCounter = numeric(),
                                 startTime = numeric(),
                                 endTime = numeric(),
                                 eventDuration = numeric(),
                                 stringsAsFactors = FALSE)



for(f in 1:length(data_files))
{
  participantDataFile <- data_files[1]
  print(participantDataFile)
  
  
  
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]
  
  
  #shortDF <- df[(df$CurrentGazeArea == "play_wall" | df$CurrentGazeArea == "build_wall" | df$CurrentGazeArea == "view_wall") & df$CurrentGazeTarget == "none", ]
  #shortDF <- df[(df$CurrentGazeArea == "play_wall" | df$CurrentGazeArea == "build_wall" | df$CurrentGazeArea == "view_wall"), ]
  #shortDF <- df[df$CurrentGazeArea == "play_wall"& df$CurrentGazeTarget == "none",]
  
  #shortDF <- df[(df$CurrentGazeArea == "play_wall" |df$CurrentGazeArea == "background_wall" | df$CurrentGazeArea == "view_wall"| df$CurrentGazeArea == "build_wall"), ]
  
  df$EyePos_X <- as.numeric(df$EyePos_X)
  df$EyePos_Y <- as.numeric(df$EyePos_Y)
  df$EyePos_Z <- as.numeric(df$EyePos_Z)
  #shortDF <- df[(df$CurrentGazeArea == "play_wall" & df$EyePos_X > (0)),]
  trimedDF <- df %>% filter((round(EyePos_X, 2) > -5 & CurrentGazeArea == "play_wall" & round(EyePos_X, 2) < 2.5 ) | CurrentGazeArea == "build_wall"| CurrentGazeArea == "view_wall" | CurrentGazeArea == "background_wall")
  
  # x <- df[,13]
  # y <- df[,14]
  # z <- df[,15]
  
  x <- trimedDF[,13]
  y <- trimedDF[,14]
  z <- trimedDF[,15]
  
  
  plot3d(x, y, z)
  
  
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
           playerTotalTime <- playerTotalTime + newTime
           AddRow(firstRowTime, currentTime, newTime, viewSwitchCounter,"Partner", durationEventDF, trimedDF)
           
           
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
  
  
  newPartRow <- data.frame(Participant, Condition, Trial, avgPlayWallDurration, avgViewWAllDurration, 
                           avgBuildWallDurration,avgPlayerDurration, playWallCounter, viewWallCounter, buildWallCounter,playerCounter, playWallTotalTime, viewWallTotalTime, buildWallTotalTime,playerTotalTime)
  
  gazeDurrationTimes <- rbind(gazeDurrationTimes, newPartRow)

}
  
AddRow <- function(startTime, endTime, eventDuration, counter, Event, df, df2){
  Participant <- df2[2,2]
  Condition <- df2[8,8]
  Trial <- df2[9,9]
  viewSwitchCounter <<- viewSwitchCounter + 1

  
  
  newRow <- data.frame(Participant, Condition, Trial, Event, viewSwitchCounter, startTime, endTime, eventDuration)
  durationEventDF <<- rbind(durationEventDF, newRow)
}





