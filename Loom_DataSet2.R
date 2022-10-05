library(plot3D)
library(rgl)
library(dplyr)


participantDataFile <- "analytics2_P2.csv"
originalDF <- read.csv(participantDataFile, header = TRUE, sep = ",")
#originalDF <- originalDF[originalDF$Condition != "tut",]

startIndexes <- originalDF[originalDF$Event=="Game Start",]
endIndexes <- originalDF[originalDF$Event=="Game Over",]

#originalDF <- originalDF %>% slice(strtoi(rownames(startIndexes[2,])):strtoi(rownames(endIndexes[2,])), strtoi(rownames(startIndexes[3,])):strtoi(rownames(endIndexes[3,])), strtoi(rownames(startIndexes[5,])):strtoi(rownames(endIndexes[4,])), strtoi(rownames(startIndexes[6,])):strtoi(rownames(endIndexes[5,])))
originalDF <- originalDF %>% slice(strtoi(rownames(startIndexes[1,])):strtoi(rownames(endIndexes[1,])), strtoi(rownames(startIndexes[2,])):strtoi(rownames(endIndexes[2,])), strtoi(rownames(startIndexes[3,])):strtoi(rownames(endIndexes[3,])), strtoi(rownames(startIndexes[4,])):strtoi(rownames(endIndexes[4,])))



originalDF <-  originalDF[originalDF$xPos != "c",]

soloDF <- originalDF[originalDF$Condition == "s" &((originalDF$Event =="looking at View wall" & originalDF$zPos < 0) | (originalDF$Event == "looking at Play wall") | (originalDF$Event == "looking at Build wall" &originalDF$zPos > 0)),]
                     
#soloDF <- originalDF[originalDF$Event == "looking at View wall" | originalDF$Event == "looking at Build wall" | originalDF$Event == "looking at Play wall" ,]


coDF <- originalDF[originalDF$Condition == "co" &((originalDF$Event =="looking at View wall" & originalDF$zPos < 0) | (originalDF$Event == "looking at Play wall") | (originalDF$Event == "looking at Build wall" &originalDF$zPos > 0)),]
#coDF <- originalDF[originalDF$Condition == "co" &(originalDF$Event == "looking at View wall" | originalDF$Event == "looking at Build wall" | originalDF$Event == "looking at Play wall"),]


xSolo <- soloDF[,9]
ySolo <- soloDF[,10]
zSolo <- soloDF[ ,11]

xCo <- coDF[,9]
yCo <- coDF[,10]
zCo <- coDF[ ,11]


plot3d(xSolo, ySolo, zSolo)
#plot3d(xCo, yCo, zCo)



if(participantDataFile == "analytics2_P2.csv" | participantDataFile == "analytics2_P3.csv" | participantDataFile == "analytics2_P4.csv" | participantDataFile == "analytics2_P6.csv" |
   participantDataFile == "analytics2_P7.csv" | participantDataFile == "analytics2_P8.csv" | participantDataFile == "analytics2_P10.csv" | participantDataFile == "analytics2_P11.csv" |
   participantDataFile == "analytics2_P12.csv" | participantDataFile == "analytics2_P14.csv" | participantDataFile == "analytics2_P16.csv")
{
  partGroup <- "e"
}else{
  partGroup <- "c"
}

groupCounter <- 0
for(j in 0:1)
{
  shortGroupDF <- data.frame(TimeStamp = numeric(),
                             Participant = factor(),
                             Condition = factor(),
                             Trial = numeric(),
                             Age = numeric(),
                             Gender = factor(),
                             SessionTime = numeric(),
                             Event = factor(),
                             xPos = numeric(),
                             yPos = numeric(),
                             zPos = numeric(),
                             group = factor(),
                             stringsAsFactors = FALSE)
  
  if(j == 0){
    groupDF <- soloDF
    partCondition <- "s"
  }else{
    groupDF <- coDF
    partCondition <- "co"
  }

  
  #Setup Variables
  lookingForViewWall <- FALSE
  lookingForBuildWall <- FALSE
  lookingForPlayWall <- FALSE
  lookingForNewSeq <- TRUE
  
  viewWall <- "looking at View wall"
  viewWall <- toString(viewWall)
  
  playWall <- "looking at Play wall"
  playWall <- toString(playWall)
  
  buildWall <- "looking at Build wall"
  buildWall <- toString(buildWall)
  
  
  #this loop is picking out all of the row that are needed for analysis and putting them into their DF's
  for (i in 2:nrow(groupDF))
  {
    currentEvent <- groupDF[i,8]
    currentEvent <- toString(currentEvent)
    #print(currentEvent)
    
    currentCondition <- groupDF[i,3]
    currentCondition <- toString(currentCondition)
    
  
    if(lookingForNewSeq == FALSE){
        if(lookingForPlayWall){
          if(currentEvent == viewWall | currentEvent == buildWall){
            second_row_to_add <- groupDF[i,]
            #soloDFshort <- rbind(soloDFshort, first_row_to_add)
            shortGroupDF <- rbind(shortGroupDF, second_row_to_add)
            #print("added a new Blue sequence")
            lookingForPlayWall <- FALSE
            lookingForNewSeq <- TRUE
            
          }else if(currentEvent == playWall){
            #print("continue looking at blue")
          }else{
            #print("bad blue")
            lookingForPlayWall <- FALSE
            lookingForNewSeq <- TRUE
          }
        }
        if(lookingForBuildWall){
          if(currentEvent == viewWall | currentEvent == playWall){
            second_row_to_add <- groupDF[i,]
            #soloDFshort <- rbind(soloDFshort, first_row_to_add)
            shortGroupDF <- rbind(shortGroupDF, second_row_to_add)
            #print("added a new Red sequence")
            lookingForBuildWall <- FALSE
            lookingForNewSeq <- TRUE
            
          }else if(currentEvent == buildWall){
            #print("continue looking at Red")
          }else{
            #print("bad red")
            
            lookingForBuildWall <- FALSE
            lookingForNewSeq <- TRUE
          }
        }
        if(lookingForViewWall){
          if(currentEvent == buildWall | currentEvent == playWall){
            second_row_to_add <- groupDF[i,]
            #soloDFshort <- rbind(soloDFshort, first_row_to_add)
            shortGroupDF <- rbind(shortGroupDF, second_row_to_add)
            #print("added a new Invis sequence")
            lookingForViewWall <- FALSE
            lookingForNewSeq <- TRUE
            
          }else if(currentEvent == viewWall){
            #print("continue looking at Invis")
          }else{
            #print("bad invis")
            
            lookingForViewWall <- FALSE
            lookingForNewSeq <- TRUE
          }
        }
    }
    if(lookingForNewSeq){
      if(currentEvent == viewWall){
        first_row_to_add <- groupDF[i,]
        lookingForNewSeq <- FALSE
        lookingForViewWall <- TRUE
      }
      if(currentEvent == playWall){
        first_row_to_add <- groupDF[i,]
        lookingForNewSeq <- FALSE
        lookingForPlayWall <- TRUE
      }
      if(currentEvent == buildWall){
        first_row_to_add <- groupDF[i,]
        lookingForNewSeq <- FALSE
        lookingForBuildWall <- TRUE
      }
    }
    
  }
  
  previousEvent <- ""
  
  previousTime <- 0
  counter <- (-1) # -1 because the first iteration doesn't count 
  
  
  totalViewWallCount <- 0
  totalPlayWallCount <- 0
  totalBuildWallCount <- 0
  
  totalTime <- 0
  totalPlay2View <- 0
  totalPlay2Build <- 0
  totalBuild2Play <- 0
  totalView2Play <- 0
  
  avgTotalTransferTime <- 0
  avgPlay2View <- 0
  avgPlay2Build <- 0
  avgBuild2Play <- 0
  avgView2Play <- 0
  
  play2ViewCounter <- 0
  play2BuildCounter <- 0
  build2PlayCounter <- 0
  view2PlayCounter <- 0
  
  
  for (i in 2:nrow(shortGroupDF))
  {
    counter <- counter +1
    currentEvent <- shortGroupDF[i,8]
    currentEvent <- toString(currentEvent)
    
    currentTime <- shortGroupDF[i,1]/10000
    if(previousTime != 0){
      
      reactionTime <- currentTime - previousTime
    }
    
    if(currentEvent == viewWall){
      if(previousEvent == playWall){
        
        totalViewWallCount <- totalViewWallCount + 1
        totalTime <- totalTime + reactionTime 
        totalPlay2View <- totalPlay2View + reactionTime
        play2ViewCounter <- play2ViewCounter + 1
        
        }else{
        #print("nothin bb")
      }
    }
    
    if(currentEvent == buildWall){
      if(previousEvent == playWall){
        
        totalBuildWallCount <- totalBuildWallCount + 1
        totalTime <- totalTime + reactionTime 
        totalPlay2Build <- totalPlay2Build + reactionTime
        play2BuildCounter <- play2BuildCounter + 1
  
      }else{
        #print("nothin bb")
      }
    }
    
    if(currentEvent == playWall){
      if(previousEvent == viewWall){
        
        totalPlayWallCount <- totalPlayWallCount + 1
        totalTime <- totalTime + reactionTime 
        totalView2Play <- totalView2Play + reactionTime
        view2PlayCounter <- view2PlayCounter + 1
        
      }else if(previousEvent == buildWall){
        
        totalPlayWallCount <- totalPlayWallCount + 1
        totalTime <- totalTime + reactionTime 
        totalBuild2Play <- totalBuild2Play + reactionTime
        build2PlayCounter <- build2PlayCounter + 1
        
      }else{
        #print("nothin bb")
      }
    }
    previousEvent <- currentEvent
    previousTime <- shortGroupDF[i,1]/10000
    
    
  }
  
  
  avgTotalTransferTime = totalTime/counter
  avgView2Play = totalView2Play/view2PlayCounter
  avgBuild2Play = totalBuild2Play/build2PlayCounter
  avgPlay2Build = totalPlay2Build/play2BuildCounter
  avgPlay2View = totalPlay2View/play2ViewCounter
  
  # 
  # 
  # newPartData <- data.frame(Participant = factor(),
  #                           Age = numeric(),
  #                           Gender = factor(),
  #                           AvgTotalTransferTime = numeric(),
  #                           AvgView2Play = numeric(),
  #                           AvgBuild2Play = numeric(),
  #                           AvgPlay2Build = numeric(),
  #                           AvgPlay2View = numeric(),
  #                           group = factor(),
  #                           condition = factor(),
  #                           stringsAsFactors = FALSE)

  newPartRow <- data.frame(shortGroupDF[5,2], shortGroupDF[5,5], shortGroupDF[5,6], avgTotalTransferTime, avgView2Play, avgBuild2Play, avgPlay2Build, avgPlay2View, partCondition, partGroup)
  newPartData <- rbind(newPartData, newPartRow)

  write.csv(newPartData, "AllSubjectGazeData.csv")

}

