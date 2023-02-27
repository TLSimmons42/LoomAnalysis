library(plot3D)
library(rgl)
library(dplyr)
library(bit64)

participantDataFile <- "analytics2_P4.csv"
#participantDataFile <- data_files[f]
originalDF <- read.csv(participantDataFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
print(participantDataFile)

startIndexes <- originalDF[originalDF$Event=="Game Start",]
endIndexes <- originalDF[originalDF$Event=="Game Over",]

startIndexes <- startIndexes[startIndexes$Condition == "s" | startIndexes$Condition == "co", ]
endIndexes <- endIndexes[endIndexes$Condition == "s" | endIndexes$Condition == "co", ]

totalSoloGameTime <- 0
totalCoGameTime <- 0
totalCombinedSoloGameTime <- 0
totalCombinedCoGameTime <- 0
totalCombinedGameTime <- 0

oneMinTimeIndexList <- list() #create an empty list
oneMinTimeIndex <- 0

for(d in 1:nrow(startIndexes))
{
  startTime <- startIndexes[d,1]/10000
  endTime <- endIndexes[d,1]/10000
  oneMinTimeIndex <- startIndexes[d,1] + 600000000
  oneMinTimeIndexList[[d]] <- oneMinTimeIndex
  
  if(d == 1 || d == 2){
    print(endTime - startTime)
    totalSoloGameTime <- totalSoloGameTime +(endTime - startTime)
    totalCombinedGameTime <- totalCombinedGameTime +(endTime - startTime)
    
  }else{
    print(endTime - startTime)
    totalCoGameTime <- totalCoGameTime +(endTime- startTime)
    totalCombinedGameTime <- totalSoloGameTime +(endTime- startTime)
  }
}
avgSoloGameTime = totalSoloGameTime/2
avgCoGameTime = totalCoGameTime/2

oneMinTimeIndexDF <- as.data.frame(oneMinTimeIndexList)
oneMinTimeIndexDF[1]


#full df slice
originalDF <- originalDF %>% slice(c(strtoi(rownames(startIndexes[1,])):strtoi(rownames(endIndexes[1,])), strtoi(rownames(startIndexes[2,])):strtoi(rownames(endIndexes[2,])), strtoi(rownames(startIndexes[3,])):strtoi(rownames(endIndexes[3,])), strtoi(rownames(startIndexes[4,])):strtoi(rownames(endIndexes[4,]))))

# 1 min df slice
#originalDF <- filter(originalDF, ((TimeStamp >= startIndexes[1,1]) & (TimeStamp <= oneMinTimeIndexDF[1,1])) | ((TimeStamp >= startIndexes[2,1]) & (TimeStamp <= oneMinTimeIndexDF[1,2])) | ((TimeStamp >= startIndexes[3,1]) & (TimeStamp <= oneMinTimeIndexDF[1,3]))| ((TimeStamp >= startIndexes[4,1]) & (TimeStamp <= oneMinTimeIndexDF[1,4])))



#originalDF <-  originalDF[originalDF$xPos != "c",]

soloDF <- originalDF[originalDF$Condition == "s" &((originalDF$Event =="looking at View wall" & originalDF$zPos < 0) | (originalDF$Event == "looking at Play wall") | (originalDF$Event == "looking at Build wall" &originalDF$zPos > 0) | (originalDF$Event == "Game Start") | (originalDF$Event == "Game Over")),]
coDF <- originalDF[originalDF$Condition == "co" &((originalDF$Event =="looking at View wall" & originalDF$zPos < 0) | (originalDF$Event == "looking at Play wall") | (originalDF$Event == "looking at Build wall" &originalDF$zPos > 0)| (originalDF$Event == "Game Start") | (originalDF$Event == "Game Over")),]

soloDF2 <- originalDF[originalDF$Condition == "s" &((originalDF$Event =="looking at View wall" & originalDF$zPos < 0) | (originalDF$Event == "looking at Play wall") | (originalDF$Event == "looking at Build wall" &originalDF$zPos > 0)| (originalDF$Event == "Game Start") | (originalDF$Event == "Game Over")
                                                    | (originalDF$Event == "Regular Red Cube(Clone) was picked up" & originalDF$xPos < 5)| (originalDF$Event == "Regular Blue Cube(Clone) was picked up" & originalDF$xPos < 5)| (originalDF$Event == "Regular Neutral Cube(Clone) was picked up"& originalDF$xPos < 5)| (originalDF$Event =="Regular Gold Cube(Clone) was picked up"& originalDF$xPos < 5)
                                                    |originalDF$Event == "Regular Red Cube(Clone)was placed in dropzone" |originalDF$Event == "Regular Blue Cube(Clone)was placed in dropzone" | originalDF$Event == "Regular Neutral Cube(Clone)was placed in dropzone" |originalDF$Event == "Regular Gold Cube(Clone)was placed in dropzone"),]

coDF2 <- originalDF[originalDF$Condition == "co" &((originalDF$Event =="looking at View wall" & originalDF$zPos < 0) | (originalDF$Event == "looking at Play wall") | (originalDF$Event == "looking at Build wall" &originalDF$zPos > 0)| (originalDF$Event == "Game Start") | (originalDF$Event == "Game Over")
                                                   | (originalDF$Event == "Network Red Cube(Clone) was picked up" & originalDF$xPos < 1.9) | (originalDF$Event == "Network Blue Cube(Clone) was picked up"& originalDF$xPos < 5)| (originalDF$Event == "Network Neutral Cube(Clone) was picked up"& originalDF$xPos < 5) | (originalDF$Event == "Network Gold Cube(Clone) was picked up"& originalDF$xPos < 5)
                                                   |originalDF$Event == "Network Red Cube(Clone)was placed in dropzone" |originalDF$Event == "Network Blue Cube(Clone)was placed in dropzone" | originalDF$Event == "Network Neutral Cube(Clone)was placed in dropzone" |originalDF$Event == "Network Gold Cube(Clone)was placed in dropzone"),]

soloDF2$Event[soloDF2$Event == "Regular Red Cube(Clone) was picked up"] <- 'cube picked up'
soloDF2$Event[soloDF2$Event == "Regular Blue Cube(Clone) was picked up"] <- 'cube picked up'
soloDF2$Event[soloDF2$Event == "Regular Neutral Cube(Clone) was picked up"] <- 'cube picked up'
soloDF2$Event[soloDF2$Event == "Regular Gold Cube(Clone) was picked up"] <- 'cube picked up'

coDF2$Event[coDF2$Event == "Network Red Cube(Clone) was picked up"] <- 'cube picked up'
coDF2$Event[coDF2$Event == "Network Blue Cube(Clone) was picked up"] <- 'cube picked up'
coDF2$Event[coDF2$Event == "Network Neutral Cube(Clone) was picked up"] <- 'cube picked up'


soloDF2$Event[soloDF2$Event == "Regular Red Cube(Clone)was placed in dropzone"] <- 'cube placed'
soloDF2$Event[soloDF2$Event == "Regular Blue Cube(Clone)was placed in dropzone"] <- 'cube placed'
soloDF2$Event[soloDF2$Event == "Regular Neutral Cube(Clone)was placed in dropzone"] <- 'cube placed'
soloDF2$Event[soloDF2$Event == "Regular Gold Cube(Clone)was placed in dropzone"] <- 'cube placed'

coDF2$Event[coDF2$Event == "Network Red Cube(Clone)was placed in dropzone"] <- 'cube placed'
coDF2$Event[coDF2$Event == "Network Blue Cube(Clone)was placed in dropzone"] <- 'cube placed'
coDF2$Event[coDF2$Event == "Network Neutral Cube(Clone)was placed in dropzone"] <- 'cube placed'


xSolo <- soloDF[,9]
ySolo <- soloDF[,10]
zSolo <- soloDF[ ,11]

xCo <- coDF[,9]
yCo <- coDF[,10]
zCo <- coDF[ ,11]


#plot3d(xSolo, ySolo, zSolo)
#plot3d(xCo, yCo, zCo)



if(participantDataFile == "analytics2_P2.csv" | participantDataFile == "analytics2_P3.csv" | participantDataFile == "analytics2_P4.csv" | participantDataFile == "analytics2_P6.csv" |
   participantDataFile == "analytics2_P7.csv" | participantDataFile == "analytics2_P8.csv" | participantDataFile == "analytics2_P10.csv" | participantDataFile == "analytics2_P11.csv" |
   participantDataFile == "analytics2_P12.csv" | participantDataFile == "analytics2_P14.csv" | participantDataFile == "analytics2_P16.csv")
{
  partGroup <- "e"
}else{
  partGroup <- "c"
}

shortGroupDF2 <- data.frame(Time = numeric(),
                            Participant = factor(),
                            Condition = factor(),
                            Trial = numeric(),
                            StartTime = numeric(),
                            EndTime = numeric(),
                            TrialEvent = factor(),
                            stringsAsFactors = TRUE)


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
  }else if(j == 1){
    groupDF <- coDF
    partCondition <- "co"
  }else if(j == 2){
    groupDF <- soloDF2
    partCondition <- "s"
  }else if(j == 3){
    groupDF <- coDF2
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
  
  gameStart <- "Game Start"
  gameEnd <- "Game Over"
  
  
  
  
  
  #this loop is picking out all of the rows that are needed for analysis and putting them into their a short DF
  for (i in 2:nrow(groupDF))
  {
    currentEvent <- groupDF[i,8]
    currentEvent <- toString(currentEvent)
    #print(lastRowTime)
    
    currentCondition <- groupDF[i,3]
    currentCondition <- toString(currentCondition)
    
    if(lastRowTime == 0){
      lastRowTime = groupDF[i,1] /10000
    }
    currentTimeAdd <- groupDF[i,1]/10000 - lastRowTime
    lastLastRowTime <- lastRowTime
    lastRowTime = groupDF[i,1]/10000
    #print(groupDF[i+1,1]/10000)
    
    if(currentEvent == gameStart | currentEvent == gameEnd){
      row_to_add <- groupDF[i,]
      shortGroupDF <- rbind(shortGroupDF, row_to_add)
      lookingForNewSeq = TRUE
    }
    
    if(lookingForNewSeq == FALSE){
      if(lookingForPlayWall){
        if(currentEvent == viewWall | currentEvent == buildWall){
          second_row_to_add <- groupDF[i,]
          shortGroupDF <- rbind(shortGroupDF, second_row_to_add)
          lookingForPlayWall <- FALSE
          lookingForNewSeq <- TRUE
          
        }else if(currentEvent == playWall){
          totalPlayWallGazeTime <- totalPlayWallGazeTime + currentTimeAdd
        }else{
          lookingForPlayWall <- FALSE
          lookingForNewSeq <- TRUE
        }
      }
      if(lookingForBuildWall){
        if(currentEvent == viewWall | currentEvent == playWall){
          second_row_to_add <- groupDF[i,]
          shortGroupDF <- rbind(shortGroupDF, second_row_to_add)
          lookingForBuildWall <- FALSE
          lookingForNewSeq <- TRUE
          
        }else if(currentEvent == buildWall){
          totalBuildWallGazeTime <- totalBuildWallGazeTime + currentTimeAdd
        }else{
          
          lookingForBuildWall <- FALSE
          lookingForNewSeq <- TRUE
        }
      }
      if(lookingForViewWall){
        if(currentEvent == buildWall | currentEvent == playWall){
          second_row_to_add <- groupDF[i,]
          shortGroupDF <- rbind(shortGroupDF, second_row_to_add)
          lookingForViewWall <- FALSE
          lookingForNewSeq <- TRUE
          
        }else if(currentEvent == viewWall){
          totalViewWallGazeTime <- totalViewWallGazeTime + currentTimeAdd
        }else{
          
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
          }
      else{
        first_row_to_add <- groupDF[i,]
        lookingForNewSeq <- TRUE
        shortGroupDF <- rbind(shortGroupDF, first_row_to_add)
      }
      
    }
  }
  
  if(j < 2){
    previousEvent <- ""
    
    previousTime <- 0
    previousTimeLong <- 0
    
    previousViewWallTime <- 0
    counter <- (-1) # -1 because the first iteration doesn't count
    trialCounter <- 0;
    
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
    go <- 0
    
    for (i in 2:nrow(shortGroupDF))
    {
      counter <- counter +1
      currentEvent <- shortGroupDF[i,8]
      currentEvent <- toString(currentEvent)
      
      currentTime <- shortGroupDF[i,1]/10000
      currentTimeLong <- shortGroupDF[i,1]
      if(previousTime != 0){
        print(currentTime - previousTime)
        if((currentTime - previousTime) < 10000){
          reactionTime <- currentTime - previousTime
          go <- 1
        }else{
          #reactionTime <- 2000
          go <- 0
        }

      }
      
      if(go == 1){
        if(currentEvent == viewWall){
          if(previousEvent == playWall){
            if(previousViewWallTime == 0){
              previousViewWallTime <- currentTime
            }else{
              avgTimeBetweenViewWallChecks <- avgTimeBetweenViewWallChecks + (currentTime - previousViewWallTime)
            }
            trialCounter <- trialCounter + 1
            TrialEvent <- "P2V"
            newTrialRow <- data.frame(reactionTime, shortGroupDF[i,2], partCondition, trialCounter, previousTimeLong, currentTimeLong, TrialEvent)
            shortGroupDF2 <- rbind(shortGroupDF2, newTrialRow)
            
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
            trialCounter <- trialCounter + 1
            TrialEvent <- "P2B"
            newTrialRow <- data.frame(reactionTime, shortGroupDF[i,2], partCondition, trialCounter, previousTimeLong, currentTimeLong,TrialEvent)
            shortGroupDF2 <- rbind(shortGroupDF2, newTrialRow)
            
            
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
            trialCounter <- trialCounter + 1
            TrialEvent <- "V2P"
            newTrialRow <- data.frame(reactionTime, shortGroupDF[i,2], partCondition, trialCounter, previousTimeLong, currentTimeLong,TrialEvent)
            shortGroupDF2 <- rbind(shortGroupDF2, newTrialRow)
            
            totalPlayWallCount <- totalPlayWallCount + 1
            totalTime <- totalTime + reactionTime
            totalView2Play <- totalView2Play + reactionTime
            view2PlayCounter <- view2PlayCounter + 1
            
          }else if(previousEvent == buildWall){
            trialCounter <- trialCounter + 1
            TrialEvent <- "B2P"
            newTrialRow <- data.frame(reactionTime, shortGroupDF[i,2], partCondition, trialCounter, previousTimeLong, currentTimeLong,TrialEvent)
            shortGroupDF2 <- rbind(shortGroupDF2, newTrialRow)
            
            totalPlayWallCount <- totalPlayWallCount + 1
            totalTime <- totalTime + reactionTime
            totalBuild2Play <- totalBuild2Play + reactionTime
            build2PlayCounter <- build2PlayCounter + 1
            
          }else{
            #print("nothin bb")
          }
        }
      }
      previousEvent <- currentEvent
      previousTime <- shortGroupDF[i,1]/10000
      previousTimeLong <- shortGroupDF[i,1]

      
    }
    
    
    avgTotalTransferTime = totalTime/counter
    avgView2Play = totalView2Play/view2PlayCounter
    avgBuild2Play = totalBuild2Play/build2PlayCounter
    avgPlay2Build = totalPlay2Build/play2BuildCounter
    avgPlay2View = totalPlay2View/play2ViewCounter
    avgTimeBetweenViewWallChecks = avgTimeBetweenViewWallChecks/totalViewWallCount
    
    
  }
}


#shortGroupDF2$TrialEvent[shortGroupDF2$TrialEvent == "P2B"] <- 'lol'
#shortGroupDF2$TrialEvent[shortGroupDF2$TrialEvent == "P2V"] <- 'black'
#shortGroupDF2$TrialEvent[shortGroupDF2$TrialEvent == "V2P"] <- 'green'
#shortGroupDF2$TrialEvent[shortGroupDF2$TrialEvent == "B2P"] <- 'blue'

plot(shortGroupDF2$trialCounter,shortGroupDF2$reactionTime,pch=16, col = shortGroupDF2$TrialEvent)


  
#plot(shortGroupDF2$trialCounter,shortGroupDF2$reactionTime,pch=16, ylim = c(0,15000), col = shortGroupDF2$TrialEvent)
  
  
  
  