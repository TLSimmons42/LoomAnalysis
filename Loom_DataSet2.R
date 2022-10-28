library(plot3D)
library(rgl)
library(dplyr)

participantDataFile <- "analytics2_P29.csv"
originalDF <- read.csv(participantDataFile, header = TRUE, sep = ",")

startIndexes <- originalDF[originalDF$Event=="Game Start",]
endIndexes <- originalDF[originalDF$Event=="Game Over",]

startIndexes <- startIndexes[startIndexes$Condition == "s" | startIndexes$Condition == "co", ]
endIndexes <- endIndexes[endIndexes$Condition == "s" | endIndexes$Condition == "co", ]

originalDF <- originalDF %>% slice(strtoi(rownames(startIndexes[1,])):strtoi(rownames(endIndexes[1,])), strtoi(rownames(startIndexes[2,])):strtoi(rownames(endIndexes[2,])), strtoi(rownames(startIndexes[3,])):strtoi(rownames(endIndexes[3,])), strtoi(rownames(startIndexes[4,])):strtoi(rownames(endIndexes[4,])))



originalDF <-  originalDF[originalDF$xPos != "c",]

# soloDF <- originalDF[originalDF$Condition == "s" &((originalDF$Event =="looking at View wall" & originalDF$zPos < 0) | (originalDF$Event == "looking at Play wall") | (originalDF$Event == "looking at Build wall" &originalDF$zPos > 0)),]
# coDF <- originalDF[originalDF$Condition == "co" &((originalDF$Event =="looking at View wall" & originalDF$zPos < 0) | (originalDF$Event == "looking at Play wall") | (originalDF$Event == "looking at Build wall" &originalDF$zPos > 0)),]

soloDF <- originalDF[originalDF$Condition == "s" &((originalDF$Event =="looking at View wall" & originalDF$zPos < 0) | (originalDF$Event == "looking at Play wall") | (originalDF$Event == "looking at Build wall" &originalDF$zPos > 0) 	
                                                    | (originalDF$Event == "Regular Red Cube(Clone) was picked up" & originalDF$xPos < 5)| (originalDF$Event == "Regular Blue Cube(Clone) was picked up" & originalDF$xPos < 5)| (originalDF$Event == "Regular Neutral Cube(Clone) was picked up"& originalDF$xPos < 5)| (originalDF$Event =="Regular Gold Cube(Clone) was picked up"& originalDF$xPos < 5)
                                                   |originalDF$Event == "Regular Red Cube(Clone)was placed in dropzone" |originalDF$Event == "Regular Blue Cube(Clone)was placed in dropzone" | originalDF$Event == "Regular Neutral Cube(Clone)was placed in dropzone" |originalDF$Event == "Regular Gold Cube(Clone)was placed in dropzone"),]

coDF <- originalDF[originalDF$Condition == "co" &((originalDF$Event =="looking at View wall" & originalDF$zPos < 0) | (originalDF$Event == "looking at Play wall") | (originalDF$Event == "looking at Build wall" &originalDF$zPos > 0)
                                                  | (originalDF$Event == "Network Red Cube(Clone) was picked up" & originalDF$xPos < 1.9) | (originalDF$Event == "Network Blue Cube(Clone) was picked up"& originalDF$xPos < 5)| (originalDF$Event == "Network Neutral Cube(Clone) was picked up"& originalDF$xPos < 5) | (originalDF$Event == "Network Gold Cube(Clone) was picked up"& originalDF$xPos < 5)
                                                  |originalDF$Event == "Network Red Cube(Clone)was placed in dropzone" |originalDF$Event == "Network Blue Cube(Clone)was placed in dropzone" | originalDF$Event == "Network Neutral Cube(Clone)was placed in dropzone" |originalDF$Event == "Network Gold Cube(Clone)was placed in dropzone"),]

soloDF$Event[soloDF$Event == "Regular Red Cube(Clone) was picked up"] <- 'cube picked up'
soloDF$Event[soloDF$Event == "Regular Blue Cube(Clone) was picked up"] <- 'cube picked up'
soloDF$Event[soloDF$Event == "Regular Neutral Cube(Clone) was picked up"] <- 'cube picked up'
soloDF$Event[soloDF$Event == "Regular Gold Cube(Clone) was picked up"] <- 'cube picked up'

coDF$Event[coDF$Event == "Network Red Cube(Clone) was picked up"] <- 'cube picked up'
coDF$Event[coDF$Event == "Network Blue Cube(Clone) was picked up"] <- 'cube picked up'
coDF$Event[coDF$Event == "Network Neutral Cube(Clone) was picked up"] <- 'cube picked up'


soloDF$Event[soloDF$Event == "Regular Red Cube(Clone)was placed in dropzone"] <- 'cube placed'
soloDF$Event[soloDF$Event == "Regular Blue Cube(Clone)was placed in dropzone"] <- 'cube placed'
soloDF$Event[soloDF$Event == "Regular Neutral Cube(Clone)was placed in dropzone"] <- 'cube placed'
soloDF$Event[soloDF$Event == "Regular Gold Cube(Clone)was placed in dropzone"] <- 'cube placed'

coDF$Event[coDF$Event == "Network Red Cube(Clone)was placed in dropzone"] <- 'cube placed'
coDF$Event[coDF$Event == "Network Blue Cube(Clone)was placed in dropzone"] <- 'cube placed'
coDF$Event[coDF$Event == "Network Neutral Cube(Clone)was placed in dropzone"] <- 'cube placed'


xSolo <- soloDF[,9]
ySolo <- soloDF[,10]
zSolo <- soloDF[ ,11]

xCo <- coDF[,9]
yCo <- coDF[,10]
zCo <- coDF[ ,11]


#plot3d(xSolo, ySolo, zSolo)
plot3d(xCo, yCo, zCo)



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
  
  totalViewWallGazeTime <- 0
  totalPlayWallGazeTime <- 0
  totalBuildWallGazeTime <- 0
  lastRowTime <- 0
  lastLastRowTime <- 0
  currentTimeAdd <- 0
  
  viewWall <- "looking at View wall"
  viewWall <- toString(viewWall)
  
  playWall <- "looking at Play wall"
  playWall <- toString(playWall)
  
  buildWall <- "looking at Build wall"
  buildWall <- toString(buildWall)
  
  rCubePickUpR <- "Regular Red Cube(Clone) was picked up"
  rCubeDropR <- "Regular Red Cube(Clone)was placed in dropzone"
  
  bCubePickUpR <- "Regular Blue Cube(Clone) was picked up"
  bCubeDropR <- "Regular Blue Cube(Clone)was placed in dropzone"
  
  nCubePickUpR <- "Regular Neutral Cube(Clone) was picked up"
  nCubeDropR <- "Regular Neutral Cube(Clone)was placed in dropzone"

  nCubePickUpR <- "Regular Gold Cube(Clone) was picked up"
  nCubeDropR <- "Regular Gold Cube(Clone)was placed in dropzone"
  
  rCubePickUpN <- "Network Red Cube(Clone) was picked up"
  rCubeDropN <- "Network Red Cube(Clone)was placed in dropzone"
  
  bCubePickUpN <- "Network Blue Cube(Clone) was picked up"
  bCubeDropN <- "Network Blue Cube(Clone)was placed in dropzone"
  
  nCubePickUpN <- "Network Neutral Cube(Clone) was picked up"
  nCubeDropN <- "Network Neutral Cube(Clone)was placed in dropzone"
  
  
  
  
  
  #this loop is picking out all of the row that are needed for analysis and putting them into their DF's
  for (i in 2:nrow(groupDF))
  {
    currentEvent <- groupDF[i,8]
    currentEvent <- toString(currentEvent)
    #print(currentEvent)
    
    currentCondition <- groupDF[i,3]
    currentCondition <- toString(currentCondition)
    
    if(lastRowTime == 0){
      lastRowTime = groupDF[i,1] /10000
    }
    currentTimeAdd <- groupDF[i,1]/10000 - lastRowTime
    lastLastRowTime <- lastRowTime
    lastRowTime = groupDF[i,1]/10000
    
  
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
            totalPlayWallGazeTime <- totalPlayWallGazeTime + currentTimeAdd
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
            totalBuildWallGazeTime <- totalBuildWallGazeTime + currentTimeAdd
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
            totalViewWallGazeTime <- totalViewWallGazeTime + currentTimeAdd
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
      }else
      if(currentEvent == playWall){
        first_row_to_add <- groupDF[i,]
        lookingForNewSeq <- FALSE
        lookingForPlayWall <- TRUE
      }else
      if(currentEvent == buildWall){
        first_row_to_add <- groupDF[i,]
        lookingForNewSeq <- FALSE
        lookingForBuildWall <- TRUE
      }else{
        first_row_to_add <- groupDF[i,]
        lookingForNewSeq <- TRUE
        shortGroupDF <- rbind(shortGroupDF, first_row_to_add)
      }
      
    }
    
  }
  
  previousEvent <- ""
  
  previousTime <- 0
  previousViewWallTime <- 0
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
  avgTimeBetweenViewWallChecks <- 0
  
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
        if(avgTimeBetweenViewWallChecks == 0){
          previousViewWallTime <- currentTime
        }else{
          avgTimeBetweenViewWallChecks <- avgTimeBetweenViewWallChecks + (currentTime - previousViewWallTime)
        }
        
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
  avgTimeBetweenViewWallChecks = avgTimeBetweenViewWallChecks/totalViewWallCount
  
  
 #----------------------------------------------------
  
  cubePickedUp <- "cube picked up"
  cubePlaced <- "cube placed"
  
  avgGrab2Build <- 0
  grab2BuildCounter <- 0
  drop2Play <- 0
  startOfGrab2Build <- 0

  lookingForGrab2Build1 <- FALSE
  lookingForGrab2Build2 <- FALSE
  lookingForGrab2Build3 <- FALSE
  lookingForGrab2Build4 <- FALSE
  lookingForGrab2Build5 <- FALSE
  
  lookingForDrop2Play <- FALSE
  
  
  for (i in 2:nrow(shortGroupDF))
  {
    currentEvent <- shortGroupDF[i,8]
    currentEvent <- toString(currentEvent)

    currentTime <- shortGroupDF[i,1]/10000
    
    currentCondition <- shortGroupDF[i,3]
    currentCondition <- toString(currentCondition)
    
    if(lastRowTime == 0){
      lastRowTime = shortGroupDF[i,1] /10000
    }
    currentTimeAdd <- shortGroupDF[i,1]/10000 - lastRowTime
    lastLastRowTime <- lastRowTime
    lastRowTime = shortGroupDF[i,1]/10000
    
    
    if(lookingForNewSeq == FALSE){
      if(lookingForGrab2Build1){
        if(currentEvent == cubePickedUp){
          lookingForGrab2Build1 = FALSE
          lookingForGrab2Build2 = TRUE
        }
      }
      if(lookingForGrab2Build2){
        if(currentEvent == playWall){
          lookingForGrab2Build2 = FALSE
          lookingForGrab2Build3 = TRUE
        }
      }
      if(lookingForGrab2Build3){
        if(currentEvent == buildWall){
          lookingForGrab2Build3 = FALSE
          lookingForGrab2Build4 = TRUE
        }
      }
      if(lookingForGrab2Build1){
        if(currentEvent == cubePickedUp){
          lookingForGrab2Build1 = FALSE
          lookingForGrab2Build2 = TRUE
        }
      }

    }
    if(lookingForNewSeq){
      if(currentEvent == playWall){
        lookingForNewSeq = FALSE
        lookingForGrab2Build1 = TRUE
        startOfGrab2Build <- currentTime
        
      }
      
    }
    
  }
  
  
  
  
  
  
  
  


  # newPartData <- data.frame(Participant = factor(),
  #                           Age = numeric(),
  #                           Gender = factor(),
  #                           avgTotalTransferTime = numeric(),
  #                           avgView2Play = numeric(),
  #                           avgBuild2Play = numeric(),
  #                           avgPlay2Build = numeric(),
  #                           avgPlay2View = numeric(),
  #                           totalView2Play = numeric(),
  #                           totalBuild2Play = numeric(),
  #                           totalPlay2Build = numeric(),
  #                           totalPlay2View = numeric(),
  #                           totalBuildWallCount = numeric(),
  #                           totalPlayWallCount = numeric(),
  #                           totalViewWallCount = numeric(),
  #                           totalPlayWallGazeTime = numeric(),
  #                           totalBuildWallGazeTime = numeric(),
  #                           totalViewWallGazeTime = numeric(),
  #                           avgPickUp2GazeFind = numeric(),
  #                           group = factor(),
  #                           condition = factor(),
  #                           stringsAsFactors = FALSE)
  
  

  Participant <- shortGroupDF[5,2]
  Age <- shortGroupDF[5,5]
  Gender <- shortGroupDF[5,6]
  condition <- partCondition
  group <- partGroup
  avgPickUp2GazeFind <- 0
  
  
  newPartRow <- data.frame(Participant, Age, Gender, avgTotalTransferTime, avgView2Play, avgBuild2Play, avgPlay2Build, totalView2Play,totalBuild2Play, totalPlay2Build, totalPlay2View, totalBuildWallCount, totalPlayWallCount,
                           totalViewWallCount,totalPlayWallGazeTime,totalBuildWallGazeTime,totalViewWallGazeTime, avgPickUp2GazeFind, group, condition)
  
  newPartData <- rbind(newPartData, newPartRow)

  write.csv(newPartData, "AllSubjectGazeData.csv")

}

