library(plot3D)
library(rgl)
library(dplyr)
library(bit64)


data_files <- list.files(pattern = "analytics")
data_files[]

bob <- length(data_files)
bob

# newPartData <- data.frame(Participant = factor(),
#                           Age = numeric(),
#                           Gender = factor(),
#                           avgTotalTransferTime = numeric(),
#                           avgTotalTransferTimeReal = numeric(),
#                           avgView2Play = numeric(),
#                           avgBuild2Play = numeric(),
#                           avgPlay2Build = numeric(),
#                           avgPlay2View = numeric(),
#                           avgTotalFix = numeric(),
#                           avgViewFix = numeric(),
#                           avgBuildFix = numeric(),
#                           avgPlayFix = numeric(),
#                           totalBuildWallCount = numeric(),
#                           totalPlayWallCount = numeric(),
#                           totalViewWallCount = numeric(),
#                           totalPlayWallGazeTime = numeric(),
#                           totalBuildWallGazeTime = numeric(),
#                           totalViewWallGazeTime = numeric(),
#                           avgTimeBetweenViewWallChecks = numeric(),
#                           avgGrab2Build = numeric(),
#                           avgGameTime = numeric(),
#                           group = factor(),
#                           condition = factor(),
#                           stringsAsFactors = FALSE)
# #
#  write.csv(newPartData, "AllSubjectGazeData3-11-23_OneMin.csv")

fullShortGroupDF <- data.frame(TimeStamp = numeric(),
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

TransferTimesDF <- data.frame(Time = numeric(),
                              Participant = factor(),
                              Condition = factor(),
                              Trial = numeric(),
                              StartTime = numeric(),
                              EndTime = numeric(),
                              Color = factor(),
                              TrialEvent = factor(),
                              stringsAsFactors = TRUE)

FixationTimesDF <- data.frame(Time = numeric(),
                              Participant = factor(),
                              Condition = factor(),
                              Trial = numeric(),
                              StartTime = numeric(),
                              EndTime = numeric(),
                              Color = factor(),
                              TrialEvent = factor(),
                              stringsAsFactors = TRUE)


write.csv(newPartData, "AllSubjectIndividualPoints3-9-23_FullTime.csv")

for(f in 1:length(data_files))
{
#participantDataFile <- "analytics2_P25.csv"
participantDataFile <- data_files[f]
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
    totalSoloGameTime <- totalSoloGameTime +(endTime- startTime)
    totalCombinedGameTime <- totalCombinedGameTime +(endTime- startTime)

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
#originalDF <- originalDF %>% slice(c(strtoi(rownames(startIndexes[1,])):strtoi(rownames(endIndexes[1,])), strtoi(rownames(startIndexes[2,])):strtoi(rownames(endIndexes[2,])), strtoi(rownames(startIndexes[3,])):strtoi(rownames(endIndexes[3,])), strtoi(rownames(startIndexes[4,])):strtoi(rownames(endIndexes[4,]))))

# 1 min df slice
originalDF <- filter(originalDF, ((TimeStamp >= startIndexes[1,1]) & (TimeStamp <= oneMinTimeIndexDF[1,1])) | ((TimeStamp >= startIndexes[2,1]) & (TimeStamp <= oneMinTimeIndexDF[1,2])) | ((TimeStamp >= startIndexes[3,1]) & (TimeStamp <= oneMinTimeIndexDF[1,3]))| ((TimeStamp >= startIndexes[4,1]) & (TimeStamp <= oneMinTimeIndexDF[1,4])))



originalDF <-  originalDF[originalDF$xPos != "c",]

soloDF <- originalDF[originalDF$Condition == "s" &((originalDF$Event =="looking at View wall" & originalDF$zPos < 0) | (originalDF$Event == "looking at Play wall") | (originalDF$Event == "looking at Build wall" &originalDF$zPos > 0)),]
coDF <- originalDF[originalDF$Condition == "co" &((originalDF$Event =="looking at View wall" & originalDF$zPos < 0) | (originalDF$Event == "looking at Play wall") | (originalDF$Event == "looking at Build wall" &originalDF$zPos > 0)),]

soloDF2 <- originalDF[originalDF$Condition == "s" &((originalDF$Event =="looking at View wall" & originalDF$zPos < 0) | (originalDF$Event == "looking at Play wall") | (originalDF$Event == "looking at Build wall" &originalDF$zPos > 0)
                                                    | (originalDF$Event == "Regular Red Cube(Clone) was picked up" & originalDF$xPos < 5)| (originalDF$Event == "Regular Blue Cube(Clone) was picked up" & originalDF$xPos < 5)| (originalDF$Event == "Regular Neutral Cube(Clone) was picked up"& originalDF$xPos < 5)| (originalDF$Event =="Regular Gold Cube(Clone) was picked up"& originalDF$xPos < 5)
                                                   |originalDF$Event == "Regular Red Cube(Clone)was placed in dropzone" |originalDF$Event == "Regular Blue Cube(Clone)was placed in dropzone" | originalDF$Event == "Regular Neutral Cube(Clone)was placed in dropzone" |originalDF$Event == "Regular Gold Cube(Clone)was placed in dropzone"),]

coDF2 <- originalDF[originalDF$Condition == "co" &((originalDF$Event =="looking at View wall" & originalDF$zPos < 0) | (originalDF$Event == "looking at Play wall") | (originalDF$Event == "looking at Build wall" &originalDF$zPos > 0)
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

xFull <- originalDF[,9]
yFull <- originalDF[,10]
zFull <- originalDF[ ,11]




#plot3d(xSolo, ySolo, zSolo)
#plot3d(xCo, yCo, zCo)
plot3d(xFull, yFull, zFull)



if(participantDataFile == "analytics2_P2.csv" | participantDataFile == "analytics2_P3.csv" | participantDataFile == "analytics2_P4.csv" | participantDataFile == "analytics2_P6.csv" |
   participantDataFile == "analytics2_P7.csv" | participantDataFile == "analytics2_P8.csv" | participantDataFile == "analytics2_P10.csv" | participantDataFile == "analytics2_P11.csv" |
   participantDataFile == "analytics2_P12.csv" | participantDataFile == "analytics2_P14.csv" | participantDataFile == "analytics2_P16.csv")
{
  partGroup <- "e"
}else{
  partGroup <- "c"
}


groupCounter <- 0
for(j in 0:3)
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

    if(lookingForNewSeq == FALSE){
        if(lookingForPlayWall){
          if(currentEvent == viewWall | currentEvent == buildWall){
            
            second_row_to_add <- groupDF[i-1,]
            shortGroupDF <- rbind(shortGroupDF, second_row_to_add)
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
            
            second_row_to_add <- groupDF[i-1,]
            shortGroupDF <- rbind(shortGroupDF, second_row_to_add)
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
            
            second_row_to_add <- groupDF[i-1,]
            shortGroupDF <- rbind(shortGroupDF, second_row_to_add)
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
    counter <- (0) # -1 because the first iteration doesn't count
    fixationCounter <- 0
    trialCounter <- 0
    
    totalViewWallCount <- 0
    totalPlayWallCount <- 0
    totalBuildWallCount <- 0
    
    totalTime <- 0
    totalFixationTime <- 0
    
    totalPlay2View <- 0
    totalPlay2Build <- 0
    totalBuild2Play <- 0
    totalView2Play <- 0
    totalBuild2View <- 0
    totalView2Build <- 0
    
    totalPlayFixationTime <- 0
    totalViewFixationTime <- 0
    totalBuildFixationTime <- 0
    
    avgTotalTransferTime <- 0
    avgPlay2View <- 0
    avgPlay2Build <- 0
    avgBuild2Play <- 0
    avgView2Play <- 0
    avgBuild2View <- 0
    avgView2Build <- 0
    
    avgTotalWallFixation <- 0
    avgPlayWallFixation <- 0
    avgBuildWallFixation <- 0
    avgViewWallFixation <- 0
    
    
    
    avgTimeBetweenViewWallChecks <- 0
    
    play2ViewCounter <- 0
    play2BuildCounter <- 0
    build2PlayCounter <- 0
    view2PlayCounter <- 0
    view2BuildCounter <- 0 
    build2ViewCounter <- 0
    
    viewFixationCounter <- 0
    buildFixationCounter <- 0
    playFixationCounter <- 0
    
    go <- 0
    
    for (i in 2:nrow(shortGroupDF))
    {
      currentEvent <- shortGroupDF[i,8]
      currentEvent <- toString(currentEvent)
      
      currentTime <- shortGroupDF[i,1]/10000
      currentTimeLong <- shortGroupDF[i,1]
      if(previousTime != 0){
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
            Color <- "red"
            newTrialRow <- data.frame(reactionTime, shortGroupDF[i,2], partCondition, trialCounter, previousTimeLong, currentTimeLong,Color, TrialEvent)
            TransferTimesDF <- rbind(TransferTimesDF, newTrialRow)
            
            counter <- counter +1
            totalViewWallCount <- totalViewWallCount + 1
            totalTime <- totalTime + reactionTime
            totalPlay2View <- totalPlay2View + reactionTime
            play2ViewCounter <- play2ViewCounter + 1
            
          }else if(previousEvent == buildWall){
            trialCounter <- trialCounter + 1
            TrialEvent <- "B2V"
            Color <- "yellow"
            newTrialRow <- data.frame(reactionTime, shortGroupDF[i,2], partCondition, trialCounter, previousTimeLong, currentTimeLong,Color,TrialEvent)
            TransferTimesDF <- rbind(TransferTimesDF, newTrialRow)
            
            counter <- counter +1
            totalBuildWallCount <- totalBuildWallCount + 1
            totalTime <- totalTime + reactionTime
            totalView2Build <- totalView2Build + reactionTime
            view2BuildCounter <- view2BuildCounter + 1
          }else if(previousEvent == viewWall){
            trialCounter <- trialCounter + 1
            TrialEvent <- "Vfix"
            Color <- "yellow"
            newTrialRow <- data.frame(reactionTime, shortGroupDF[i,2], partCondition, trialCounter, previousTimeLong, currentTimeLong,Color,TrialEvent)
            FixationTimesDF <- rbind(FixationTimesDF, newTrialRow)
            
            fixationCounter <- fixationCounter +1
            totalFixationTime <- totalFixationTime + reactionTime
            totalViewFixationTime <- totalViewFixationTime + reactionTime
            viewFixationCounter <- viewFixationCounter + 1
          }else{
            #print("nothin bb")
          }
          
        }
        
        if(currentEvent == buildWall){
          if(previousEvent == playWall){
            trialCounter <- trialCounter + 1
            TrialEvent <- "P2B"
            Color <- "blue"
            newTrialRow <- data.frame(reactionTime, shortGroupDF[i,2], partCondition, trialCounter, previousTimeLong, currentTimeLong,Color,TrialEvent)
            TransferTimesDF <- rbind(TransferTimesDF, newTrialRow)
            
            counter <- counter +1
            totalBuildWallCount <- totalBuildWallCount + 1
            totalTime <- totalTime + reactionTime
            totalPlay2Build <- totalPlay2Build + reactionTime
            play2BuildCounter <- play2BuildCounter + 1
            
          }else if(previousEvent == viewWall){
            trialCounter <- trialCounter + 1
            TrialEvent <- "V2B"
            Color <- "orange"
            newTrialRow <- data.frame(reactionTime, shortGroupDF[i,2], partCondition, trialCounter, previousTimeLong, currentTimeLong,Color,TrialEvent)
            TransferTimesDF <- rbind(TransferTimesDF, newTrialRow)
            
            counter <- counter +1
            totalViewWallCount <- totalViewWallCount + 1
            totalTime <- totalTime + reactionTime
            totalBuild2View <- totalBuild2View + reactionTime
            build2ViewCounter <- build2ViewCounter + 1
          }else if(previousEvent == buildWall){
            trialCounter <- trialCounter + 1
            TrialEvent <- "Bfix"
            Color <- "blue"
            newTrialRow <- data.frame(reactionTime, shortGroupDF[i,2], partCondition, trialCounter, previousTimeLong, currentTimeLong,Color,TrialEvent)
            FixationTimesDF <- rbind(FixationTimesDF, newTrialRow)
            
            fixationCounter <- fixationCounter +1
            totalFixationTime <- totalFixationTime + reactionTime
            totalBuildFixationTime <- totalBuildFixationTime + reactionTime
            buildFixationCounter <- buildFixationCounter + 1
          }else{
            #print("nothin bb")
          }
        }
        
        if(currentEvent == playWall){
          if(previousEvent == viewWall){
            trialCounter <- trialCounter + 1
            TrialEvent <- "V2P"
            Color <- "green"
            newTrialRow <- data.frame(reactionTime, shortGroupDF[i,2], partCondition, trialCounter, previousTimeLong, currentTimeLong,Color,TrialEvent)
            TransferTimesDF <- rbind(TransferTimesDF, newTrialRow)
            
            counter <- counter +1
            totalPlayWallCount <- totalPlayWallCount + 1
            totalTime <- totalTime + reactionTime
            totalView2Play <- totalView2Play + reactionTime
            view2PlayCounter <- view2PlayCounter + 1
            
          }else if(previousEvent == buildWall){
            trialCounter <- trialCounter + 1
            TrialEvent <- "B2P"
            Color <- "black"
            newTrialRow <- data.frame(reactionTime, shortGroupDF[i,2], partCondition, trialCounter, previousTimeLong, currentTimeLong,Color,TrialEvent)
            TransferTimesDF <- rbind(TransferTimesDF, newTrialRow)
            
            counter <- counter +1
            totalPlayWallCount <- totalPlayWallCount + 1
            totalTime <- totalTime + reactionTime
            totalBuild2Play <- totalBuild2Play + reactionTime
            build2PlayCounter <- build2PlayCounter + 1
            
          }else if(previousEvent == playWall){
            trialCounter <- trialCounter + 1
            TrialEvent <- "Pfix"
            Color <- "red"
            newTrialRow <- data.frame(reactionTime, shortGroupDF[i,2], partCondition, trialCounter, previousTimeLong, currentTimeLong,Color,TrialEvent)
            FixationTimesDF <- rbind(FixationTimesDF, newTrialRow)
            
            fixationCounter <- fixationCounter +1
            totalFixationTime <- totalFixationTime + reactionTime
            totalPlayFixationTime <- totalPlayFixationTime + reactionTime
            playFixationCounter <- playFixationCounter + 1
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
    avgView2Build = totalView2Build/view2BuildCounter
    avgBuild2View = totalBuild2View/build2ViewCounter
    avgTotalTransferTimeReal = (avgView2Play + avgBuild2Play + avgPlay2Build + avgPlay2View)/4
    
    avgTotalWallFixation = totalFixationTime/fixationCounter
    avgBuildWallFixation = totalBuildFixationTime/buildFixationCounter
    avgViewWallFixation = totalViewFixationTime/viewFixationCounter
    avgPlayWallFixation = totalPlayFixationTime/playFixationCounter
    
    avgTimeBetweenViewWallChecks = avgTimeBetweenViewWallChecks/totalViewWallCount
    
    
  }

 #----------------------------------------------------

  cubePickedUp <- "cube picked up"
  cubePlaced <- "cube placed"

  avgGrab2Build <- 0
  grab2BuildCounter <- 0
  drop2Play <- 0
  startOfGrab2Build <- 0

  lookingForGrab2Build1 <- FALSE #play
  lookingForGrab2Build2 <- FALSE #pick
  lookingForGrab2Build3 <- FALSE #play
  lookingForGrab2Build4 <- FALSE #build
  lookingForGrab2Build5 <- FALSE #place

  lookingForDrop2Play <- FALSE

  if(j >= 2)
  {
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
            startOfGrab2Build <- currentTime
  
          }
        } else if(lookingForGrab2Build2){
          if(currentEvent == playWall){
            lookingForGrab2Build2 = FALSE
            lookingForGrab2Build3 = TRUE
  
          }
        } else if(lookingForGrab2Build3){
          if(currentEvent == buildWall){
            lookingForGrab2Build3 = FALSE
            lookingForGrab2Build4 = TRUE
  
          }
        } else if(lookingForGrab2Build4){
          if(currentEvent == cubePlaced){
            lookingForGrab2Build4 = FALSE
            lookingForGrab2Build5 = TRUE
            grab2BuildCounter <- grab2BuildCounter + 1
            avgGrab2Build <- avgGrab2Build + (currentTime - startOfGrab2Build)
          }
        }else{
          lookingForNewSeq <- TRUE
          lookingForGrab2Build1 = FALSE
          lookingForGrab2Build2 = FALSE
          lookingForGrab2Build3 = FALSE
          lookingForGrab2Build4 = FALSE
          lookingForGrab2Build5 = FALSE
  
        }
      }
       if(lookingForNewSeq){
        if(currentEvent == playWall){
          lookingForNewSeq = FALSE
          lookingForGrab2Build1 = TRUE
        }
      }
  
    }
    avgGrab2Build <- avgGrab2Build/grab2BuildCounter
  }


# # 

  

  Participant <- shortGroupDF[5,2]
  Age <- shortGroupDF[5,5]
  Gender <- shortGroupDF[5,6]
  condition <- partCondition
  group <- partGroup
  
  
  if(j == 0){
    s1avgTotalTransferTime = avgTotalTransferTime
    s1avgTotalTransferTimeReal = avgTotalTransferTimeReal
    s1avgView2Play = avgView2Play
    s1avgBuild2Play = avgBuild2Play
    s1avgPlay2Build = avgPlay2Build
    s1avgPlay2View = avgPlay2View
    s1avgTotalFix = avgTotalWallFixation
    s1avgViewFix = avgViewWallFixation
    s1avgBuildFix = avgBuildWallFixation
    s1avgPlayFix = avgPlayWallFixation
    s1totalBuildWallCount = totalBuildWallCount
    s1totalPlayWallCount = totalPlayWallCount
    s1totalViewWallCount = totalViewWallCount
    s1totalPlayFixationTime = totalPlayFixationTime
    s1totalBuildFixationTime = totalBuildFixationTime
    s1totalViewFixationTime = totalViewFixationTime
    s1avgTimeBetweenViewWallChecks = avgTimeBetweenViewWallChecks
    
  }
  if(j ==1){
    s2avgTotalTransferTime = avgTotalTransferTime
    s2avgTotalTransferTimeReal = avgTotalTransferTimeReal
    s2avgView2Play = avgView2Play
    s2avgBuild2Play = avgBuild2Play
    s2avgPlay2Build = avgPlay2Build
    s2avgPlay2View = avgPlay2View
    s2avgTotalFix = avgTotalWallFixation
    s2avgViewFix = avgViewWallFixation
    s2avgBuildFix = avgBuildWallFixation
    s2avgPlayFix = avgPlayWallFixation
    s2totalBuildWallCount = totalBuildWallCount
    s2totalPlayWallCount = totalPlayWallCount
    s2totalViewWallCount = totalViewWallCount
    s2totalPlayFixationTime = totalPlayFixationTime
    s2totalBuildFixationTime = totalBuildFixationTime
    s2totalViewFixationTime = totalViewFixationTime
    s2avgTimeBetweenViewWallChecks = avgTimeBetweenViewWallChecks
  }
  
  if(j == 2){
    
    avgTotalTransferTime = s1avgTotalTransferTime
    avgTotalTransferTimeReal = s1avgTotalTransferTimeReal
    avgView2Play = s1avgView2Play
    avgBuild2Play = s1avgBuild2Play
    avgPlay2Build = s1avgPlay2Build
    avgPlay2View = s1avgPlay2View
    avgTotalFix = s1avgTotalFix
    avgViewFix = s1avgViewFix
    avgBuildFix = s1avgBuildFix
    avgPlayFix = s1avgPlayFix
    totalBuildWallCount = s1totalBuildWallCount
    totalPlayWallCount = s1totalPlayWallCount
    totalViewWallCount = s1totalViewWallCount
    totalPlayWallGazeTime = s1totalPlayFixationTime
    totalBuildWallGazeTime = s1totalBuildFixationTime
    totalViewWallGazeTime = s1totalViewFixationTime
    avgTimeBetweenViewWallChecks = s1avgTimeBetweenViewWallChecks
    avgGameTime <- avgSoloGameTime
    
    newPartRow <- data.frame(Participant, Age, Gender, avgTotalTransferTime,avgTotalTransferTimeReal, avgView2Play, avgBuild2Play, avgPlay2Build, avgPlay2View, avgTotalFix,avgViewFix,avgBuildFix,avgPlayFix, totalBuildWallCount, totalPlayWallCount,
                             totalViewWallCount,totalPlayWallGazeTime,totalBuildWallGazeTime,totalViewWallGazeTime, avgTimeBetweenViewWallChecks,avgGrab2Build, avgGameTime, group, condition)
    
    newPartData <- rbind(newPartData, newPartRow)
  
    write.csv(newPartData, "AllSubjectGazeData3-11-23_OneMin.csv")
  }
  if(j == 3){
    
    avgTotalTransferTime = s2avgTotalTransferTime
    avgTotalTransferTimeReal = s2avgTotalTransferTimeReal
    avgView2Play = s2avgView2Play
    avgBuild2Play = s2avgBuild2Play
    avgPlay2Build = s2avgPlay2Build
    avgPlay2View = s2avgPlay2View
    avgTotalFix = s2avgTotalFix
    avgViewFix = s2avgViewFix
    avgBuildFix = s2avgBuildFix
    avgPlayFix = s2avgPlayFix
    totalBuildWallCount = s2totalBuildWallCount
    totalPlayWallCount = s2totalPlayWallCount
    totalViewWallCount = s2totalViewWallCount
    totalPlayWallGazeTime = s2totalPlayFixationTime
    totalBuildWallGazeTime = s2totalBuildFixationTime
    totalViewWallGazeTime = s2totalViewFixationTime
    avgTimeBetweenViewWallChecks = s2avgTimeBetweenViewWallChecks
    avgGameTime <- avgCoGameTime
    
    newPartRow <- data.frame(Participant, Age, Gender, avgTotalTransferTime,avgTotalTransferTimeReal, avgView2Play, avgBuild2Play, avgPlay2Build, avgPlay2View, avgTotalFix,avgViewFix,avgBuildFix,avgPlayFix, totalBuildWallCount, totalPlayWallCount,
                             totalViewWallCount,totalPlayWallGazeTime,totalBuildWallGazeTime,totalViewWallGazeTime, avgTimeBetweenViewWallChecks,avgGrab2Build, avgGameTime, group, condition)
    newPartData <- rbind(newPartData, newPartRow)
    
    write.csv(newPartData, "AllSubjectGazeData3-11-23_OneMin.csv")
  }  

}
}
plot(FixationTimesDF$trialCounter,FixationTimesDF$reactionTime,pch=16, col = FixationTimesDF$Color)
plot(TransferTimesDF$trialCounter,TransferTimesDF$reactionTime,pch=16, col = TransferTimesDF$Color)
