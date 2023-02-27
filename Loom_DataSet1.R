library(plot3D)
library(rgl)
library(dplyr)
library(bit64)


data_files <- list.files(pattern = "analytics")


# 
# newPartData <- data.frame(Participant = factor(),
#                           Age = numeric(),
#                           pacStay = numeric(),
#                           pacMove = numeric(),
#                           condition = factor(),
#                           group = factor(),
#                           stringsAsFactors = FALSE)
# write.csv(newPartData, "pacMoving_2-25-23_FullTime.csv")



PACmoveDF <- data.frame(Time = numeric(),
                        Participant = factor(),
                        Condition = factor(),
                        Trial = numeric(),
                        StartTime = numeric(),
                        EndTime = numeric(),
                        TrialEvent = factor(),
                        stringsAsFactors = FALSE)



for(f in 1:length(data_files))
{
 
  
   
  
  #participantDataFile <- "analytics_P30.csv"
  
  print(data_files[f])
  participantDataFile <- data_files[f]
  PerceptionActionDF <- read.csv(participantDataFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",")
  

  PerceptionActionDF <- PerceptionActionDF[(PerceptionActionDF$Condition == "s" | PerceptionActionDF$Condition == "co"),]
  
  
  if(participantDataFile == "analytics_P2.csv" | participantDataFile == "analytics_P3.csv" | participantDataFile == "analytics_P4.csv" | participantDataFile == "analytics_P6.csv" |
     participantDataFile == "analytics_P7.csv" | participantDataFile == "analytics_P8.csv" | participantDataFile == "analytics_P10.csv" | participantDataFile == "analytics_P11.csv" |
     participantDataFile == "analytics_P12.csv" | participantDataFile == "analytics_P14.csv" | participantDataFile == "analytics_P16.csv")
  {
    partGroup <- "e"
  }else{
    partGroup <- "c"
  }
  
  startIndexes <- PerceptionActionDF[PerceptionActionDF$Event=="Game Start",]
  
  oneMinTimeIndexList <- list() #create an empty list
  oneMinTimeIndex <- 0
  
  for(d in 1:nrow(startIndexes))
  {
    oneMinTimeIndex <- startIndexes[d,1] + 600000000
    oneMinTimeIndexList[[d]] <- oneMinTimeIndex
  }
  
  oneMinTimeIndexDF <- as.data.frame(oneMinTimeIndexList)
  oneMinTimeIndexDF[1]
  
  #PerceptionActionDF <- filter(PerceptionActionDF, ((TimeStamp >= startIndexes[1,1]) & (TimeStamp <= oneMinTimeIndexDF[1,1])) | ((TimeStamp >= startIndexes[2,1]) & (TimeStamp <= oneMinTimeIndexDF[1,2])) | ((TimeStamp >= startIndexes[3,1]) & (TimeStamp <= oneMinTimeIndexDF[1,3]))| ((TimeStamp >= startIndexes[4,1]) & (TimeStamp <= oneMinTimeIndexDF[1,4])))
  
  
  
  #Set up the new PAC1 and PAC2 DF's
  PAC1 <- data.frame(TimeStamp = numeric(),
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
                     stringsAsFactors = FALSE)
  
  PAC2 <- data.frame(TimeStamp = numeric(),
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
                     stringsAsFactors = FALSE)
  PAC1Co <- data.frame(TimeStamp = numeric(),
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
                       stringsAsFactors = FALSE)
  
  PAC2Co <- data.frame(TimeStamp = numeric(),
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
                       stringsAsFactors = FALSE)
  
  
  #adding row of a DF to a NEW DF
  
  # first_row_to_add <- PerceptionActionDF[5,]
  # row_to_add2 <- PerceptionActionDF[100,]
  # 
  # PAC1 <- rbind(PAC1, row_to_add2)
  # PAC1 <- rbind(PAC1, row_to_add)
  
  #tail(PAC1)
  
  
  #set up vars
  lookingForNewSeq <- TRUE
  lookingForBlue <- FALSE
  lookingForRed <- FALSE
  lookingForInvis <- FALSE
  lookingForGold <- FALSE
  lookingForDropZone <- FALSE
  
  lookingAtBlueCube <- "looking at blue cube"
  lookingAtBlueCube <- toString(lookingAtBlueCube)
  
  lookingAtRedCube <- "looking at red cube"
  lookingAtRedCube <- toString(lookingAtRedCube)
  
  lookingAtInvisCube <- "looking at invis cube"
  lookingAtInvisCube <- toString(lookingAtInvisCube)
  
  lookingAtGoldCube <- "looking at gold cube"
  lookingAtGoldCube <- toString(lookingAtGoldCube)
  
  lookingAtDropzone <- "looking at Drop Zone"
  lookingAtDropzone <- toString(lookingAtDropzone)
  
  
  
  #Regular Variables
  regRedCubeDrop <- "Regular Red Cube(Clone)was placed in dropzone"
  regRedCubeDrop <- toString(regRedCubeDrop)
  
  regBlueCubeDrop <- "Regular Blue Cube(Clone)was placed in dropzone"
  regBlueCubeDrop <- toString(regBlueCubeDrop)
  
  regInvisCubeDrop <- "Regular Neutral Cube(Clone)was placed in dropzone"
  regInvisCubeDrop <- toString(regInvisCubeDrop)
  
  regGoldCubeDrop <- "Regular Gold Cube(Clone)was placed in dropzone"
  regGoldCubeDrop <- toString(regGoldCubeDrop)
  
  regRedCubePickUp <- "Regular Red Cube(Clone) was picked up"
  regRedCubePickUp <- toString(regRedCubePickUp)
  
  regBlueCubePickUp <- "Regular Blue Cube(Clone) was picked up"
  regBlueCubePickUp <- toString(regBlueCubePickUp)
  
  regInvisCubePickUp <- "Regular Neutral Cube(Clone) was picked up"
  regInvisCubePickUp <- toString(regInvisCubePickUp)
  
  regGoldCubePickUp <- "Regular Gold Cube(Clone) was picked up"
  regGoldCubePickUp <- toString(regGoldCubePickUp)
  
  #NetWork Variables
  netRedCubeDrop <- "Network Red Cube(Clone)was placed in dropzone"
  netRedCubeDrop <- toString(netRedCubeDrop)
  
  netBlueCubeDrop <- "Network Blue Cube(Clone)was placed in dropzone"
  netBlueCubeDrop <- toString(netBlueCubeDrop)
  
  netInvisCubeDrop <- "Network Neutral Cube(Clone)was placed in dropzone"
  netInvisCubeDrop <- toString(netInvisCubeDrop)
  
  netGoldCubeDrop <- "Network Gold Cube(Clone)was placed in dropzone"
  netGoldCubeDrop <- toString(netGoldCubeDrop)
  
  netLeftGoldCubeDrop <- "Network Gold Left Half(Clone)was placed in dropzone"
  netLeftGoldCubeDrop <- toString(netLeftGoldCubeDrop)
  
  netGoldRightCubeDrop <- "Network Gold Right Half(Clone)was placed in dropzone"
  netGoldRightCubeDrop <- toString(netGoldRightCubeDrop)
  
  
  
  
  netRedCubePickUp <- "Network Red Cube(Clone) was picked up"
  netRedCubePickUp <- toString(netRedCubePickUp)
  
  netBlueCubePickUp <- "Network Blue Cube(Clone) was picked up"
  netBlueCubePickUp <- toString(netBlueCubePickUp)
  
  netInvisCubePickUp <- "Network Neutral Cube(Clone) was picked up"
  netInvisCubePickUp <- toString(netInvisCubePickUp)
  
  netGoldCubePickUp <- "Network Gold Cube(Clone) was picked up"
  netGoldCubePickUp <- toString(netGoldCubePickUp)
  
  
  counter <- 0
  
  #this loop is picking out all of the row that are needed for analysis and putting them into their DF's
  for (i in 2:nrow(PerceptionActionDF))
  {
    counter <- counter +1
    currentEvent <- PerceptionActionDF[i,8]
    currentEvent <- toString(currentEvent)
    
    currentCondition <- PerceptionActionDF[i,3]
    currentCondition <- toString(currentCondition)
    
    if(currentCondition == "s"){
      if(lookingForNewSeq == FALSE){
        if(lookingForBlue){
          if(currentEvent == regBlueCubePickUp){
            second_row_to_add <- PerceptionActionDF[i,]
            PAC1 <- rbind(PAC1, first_row_to_add)
            PAC1 <- rbind(PAC1, second_row_to_add)
            #print("added a new Blue sequence")
            lookingForNewSeq <- TRUE
            
          }else if(currentEvent == lookingAtBlueCube){
            #print("continue looking at blue")
          }else{
            #print("bad blue")
            lookingForBlue <- FALSE
            lookingForNewSeq <- TRUE
          }
        }
        if(lookingForRed){
          if(currentEvent == regRedCubePickUp){
            second_row_to_add <- PerceptionActionDF[i,]
            PAC1 <- rbind(PAC1, first_row_to_add)
            PAC1 <- rbind(PAC1, second_row_to_add)
            #print("added a new Red sequence")
            lookingForNewSeq <- TRUE
            
          }else if(currentEvent == lookingAtRedCube){
            #print("continue looking at Red")
          }else{
            #print("bad red")
            
            lookingForRed <- FALSE
            lookingForNewSeq <- TRUE
          }
        }
        if(lookingForInvis){
          if(currentEvent == regInvisCubePickUp){
            second_row_to_add <- PerceptionActionDF[i,]
            PAC1 <- rbind(PAC1, first_row_to_add)
            PAC1 <- rbind(PAC1, second_row_to_add)
            #print("added a new Invis sequence")
            lookingForNewSeq <- TRUE
            
          }else if(currentEvent == lookingAtInvisCube){
            #print("continue looking at Invis")
          }else{
            #print("bad invis")
            
            lookingForInvis <- FALSE
            lookingForNewSeq <- TRUE
          }
        }
        if(lookingForGold){
          if(currentEvent == regGoldCubePickUp){
            second_row_to_add <- PerceptionActionDF[i,]
            PAC1 <- rbind(PAC1, first_row_to_add)
            PAC1 <- rbind(PAC1, second_row_to_add)
            #print("added a new Gold sequence")
            lookingForNewSeq <- TRUE
            
          }else if(currentEvent == lookingAtGoldCube){
            #print("continue looking at Gold")
          }else{
            #print("bad gold")
            lookingForGold <- FALSE
            lookingForNewSeq <- TRUE
          }
        }
        if(lookingForDropZone){
          if(currentEvent == regRedCubeDrop || currentEvent == regBlueCubeDrop || currentEvent == regInvisCubeDrop || currentEvent == regGoldCubeDrop){
            second_row_to_add <- PerceptionActionDF[i,]
            PAC2 <- rbind(PAC2, first_row_to_add)
            PAC2 <- rbind(PAC2, second_row_to_add)
            #print("added a new Drop sequence")
            lookingForNewSeq <- TRUE
            
          }else if(currentEvent == lookingAtDropzone){
            #print("continue looking at Drop")
          }else{
            #print("bad Drop")
            lookingForDropZone <- FALSE
            lookingForNewSeq <- TRUE
          }
        }
      }
    }
    if(currentCondition == "co"){
      if(lookingForNewSeq == FALSE){
        if(lookingForBlue){
          if(currentEvent == netBlueCubePickUp){
            second_row_to_add <- PerceptionActionDF[i,]
            PAC1Co <- rbind(PAC1Co, first_row_to_add)
            PAC1Co <- rbind(PAC1Co, second_row_to_add)
            #print("added a new Blue sequence")
            lookingForNewSeq <- TRUE
            
          }else if(currentEvent == lookingAtBlueCube){
            #print("continue looking at blue")
          }else{
            #print("bad blue")
            lookingForBlue <- FALSE
            lookingForNewSeq <- TRUE
          }
        }
        if(lookingForRed){
          if(currentEvent == netRedCubePickUp){
            second_row_to_add <- PerceptionActionDF[i,]
            PAC1Co <- rbind(PAC1Co, first_row_to_add)
            PAC1Co <- rbind(PAC1Co, second_row_to_add)
            #print("added a new Red sequence")
            lookingForNewSeq <- TRUE
            
          }else if(currentEvent == lookingAtRedCube){
            #print("continue looking at Red")
          }else{
            #print("bad red")
            
            lookingForRed <- FALSE
            lookingForNewSeq <- TRUE
          }
        }
        if(lookingForInvis){
          if(currentEvent == netInvisCubePickUp){
            second_row_to_add <- PerceptionActionDF[i,]
            PAC1Co <- rbind(PAC1Co, first_row_to_add)
            PAC1Co <- rbind(PAC1Co, second_row_to_add)
            #print("added a new Invis sequence")
            lookingForNewSeq <- TRUE
            
          }else if(currentEvent == lookingAtInvisCube){
            #print("continue looking at Invis")
          }else{
            #print("bad invis")
            
            lookingForInvis <- FALSE
            lookingForNewSeq <- TRUE
          }
        }
        if(lookingForGold){
          if(currentEvent == netGoldCubePickUp){
            second_row_to_add <- PerceptionActionDF[i,]
            PAC1Co <- rbind(PAC1Co, first_row_to_add)
            PAC1Co <- rbind(PAC1Co, second_row_to_add)
            #print("added a new Gold sequence")
            lookingForNewSeq <- TRUE
            
          }else if(currentEvent == lookingAtGoldCube){
            #print("continue looking at Gold")
          }else{
            #print("bad gold")
            lookingForGold <- FALSE
            lookingForNewSeq <- TRUE
          }
        }
        if(lookingForDropZone){
          if(currentEvent == netRedCubeDrop || currentEvent == netBlueCubeDrop || currentEvent == netInvisCubeDrop || currentEvent == netGoldCubeDrop || currentEvent == netLeftGoldCubeDrop|| currentEvent == netGoldRightCubeDrop){
            second_row_to_add <- PerceptionActionDF[i,]
            PAC2Co <- rbind(PAC2Co, first_row_to_add)
            PAC2Co <- rbind(PAC2Co, second_row_to_add)
            #print("added a new Drop sequence")
            lookingForNewSeq <- TRUE
            
          }else if(currentEvent == lookingAtDropzone){
            #print("continue looking at Drop")
          }else{
            #print("bad Drop")
            lookingForDropZone <- FALSE
            lookingForNewSeq <- TRUE
          }
        }
      }
    }
    
    
    
    if(lookingForNewSeq){
      if(currentEvent == lookingAtBlueCube){
        first_row_to_add <- PerceptionActionDF[i,]
        lookingForNewSeq <- FALSE
        lookingForBlue <- TRUE
        #print("looking at blue cube")
      }
      if(currentEvent == lookingAtRedCube){
        first_row_to_add <- PerceptionActionDF[i,]
        lookingForNewSeq <- FALSE
        lookingForRed <- TRUE
        #print("looking at red cube")
      }
      if(currentEvent == lookingAtInvisCube){
        first_row_to_add <- PerceptionActionDF[i,]
        lookingForNewSeq <- FALSE
        lookingForInvis <- TRUE
        #print("looking at invis cube")
      }
      if(currentEvent == lookingAtGoldCube){
        first_row_to_add <- PerceptionActionDF[i,]
        lookingForNewSeq <- FALSE
        lookingForGold <- TRUE
        #print("looking at gold cube")
      }
      if(currentEvent == lookingAtDropzone){
        first_row_to_add <- PerceptionActionDF[i,]
        lookingForNewSeq <- FALSE
        lookingForDropZone <- TRUE
        #print("looking at Drop Zone")
      }
    }
  }  
  
  # This will average all of the sequence times for PCA sequence 1
  avgPCA1Time <- 0
  counter <- 0
  totalCounter <- 0
  totalTime <- 0
  startValue <- 0
  PACmoveTrailCounter <- 0
  
  soloCondition <- "s"
  coCondition <- "co"
  PACmoveString <- "PACmove"
  PACstayString <- "PACstay"
  
  for (i in 1:nrow(PAC1))
  {
    counter <- counter + 1
    if(counter == 1) {
      startValue <- PAC1[i,1]/10000
    } else {
      if(((PAC1[i,1]/10000) - startValue) < 5000){
        reactionTime <- (PAC1[i,1]/10000) - startValue
        totalTime <- totalTime + reactionTime
        counter <- 0
        totalCounter <- totalCounter + 1
        
        PACmoveTrailCounter <- PACmoveTrailCounter + 1
        Time <- reactionTime
        Participant <- PAC1[i,2]
        Condition <- PAC1[i,3]
        Trial <- PACmoveTrailCounter
        StartTime = PAC1[i-1,1]
        EndTime <- PAC1[i,1]
        TrialEvent <- PACmoveString
        
        newTrialRow <- data.frame(Time, Participant, Condition, Trial, StartTime, EndTime, TrialEvent)
        PACmoveDF <- rbind(PACmoveDF, newTrialRow)
      }else{
        print("bad")
      }
    }
  }
  
  avgPCA1Time <- totalTime/totalCounter
  
  
  # This will average all of the sequence times for PCA sequence 2
  avgPCA2Time <- 0
  counter <- 0
  totalCounter <- 0
  totalTime <- 0
  startValue <- 0
  
  for (i in 1:nrow(PAC2))
  {
    counter <- counter + 1
    if(counter == 1) {
      startValue <- PAC2[i,1]/10000
    } else {
      if(((PAC1[i,1]/10000) - startValue) < 5000){
        reactionTime <- (PAC2[i,1]/10000) - startValue
        totalTime <- totalTime + reactionTime
        counter <- 0
        totalCounter <- totalCounter + 1
        
        PACmoveTrailCounter <- PACmoveTrailCounter + 1
        Time <- reactionTime
        Participant <- PAC2[i,2]
        Condition <- PAC2[i,3]
        Trial <- PACmoveTrailCounter
        StartTime = PAC2[i-1,1]
        EndTime <- PAC2[i,1]
        TrialEvent <- PACstayString
        
        newTrialRow <- data.frame(Time, Participant, Condition, Trial, StartTime, EndTime, TrialEvent)
        PACmoveDF <- rbind(PACmoveDF, newTrialRow)
      }else{
        print("bad")
      }
    }
  }
  
  avgPCA2Time <- totalTime/totalCounter
  
  
  
  # THIS IS THE COOPERATIVE ANALYSIS
  
  # This will average all of the sequence times for PCA sequence 1
  avgPCA1CoTime <- 0
  counter <- 0
  totalCounter <- 0
  totalTime <- 0
  startValue <- 0
  
  for (i in 1:nrow(PAC1Co))
  {
    counter <- counter + 1
    if(counter == 1) {
      startValue <- PAC1Co[i,1]/10000
    } else {
      if(((PAC1[i,1]/10000) - startValue) < 5000){
        reactionTime <- (PAC1Co[i,1]/10000) - startValue
        totalTime <- totalTime + reactionTime
        counter <- 0
        totalCounter <- totalCounter + 1
        
        PACmoveTrailCounter <- PACmoveTrailCounter + 1
        Time <- reactionTime
        Participant <- PAC1Co[i,2]
        Condition <- PAC1Co[i,3]
        Trial <- PACmoveTrailCounter
        StartTime = PAC1Co[i-1,1]
        EndTime <- PAC1Co[i,1]
        TrialEvent <- PACmoveString
        
        newTrialRow <- data.frame(Time, Participant, Condition, Trial, StartTime, EndTime, TrialEvent)
        PACmoveDF <- rbind(PACmoveDF, newTrialRow)
      }else{
        print("bad")
      }
    }
  }
  
  avgPCA1CoTime <- totalTime/totalCounter
  
  
  # This will average all of the sequence times for PCA sequence 2
  avgPCA2CoTime <- 0
  counter <- 0
  totalCounter <- 0
  totalTime <- 0
  startValue <- 0
  
  for (i in 1:nrow(PAC2Co))
  {
    counter <- counter + 1
    if(counter == 1) {
      startValue <- PAC2Co[i,1]/10000
    } else {
      if(((PAC1[i,1]/10000) - startValue) < 5000){
        reactionTime <- (PAC2Co[i,1]/10000) - startValue
        totalTime <- totalTime + reactionTime
        counter <- 0
        totalCounter <- totalCounter + 1
        
        PACmoveTrailCounter <- PACmoveTrailCounter + 1
        Time <- reactionTime
        Participant <- PAC2Co[i,2]
        Condition <- PAC2Co[i,3]
        Trial <- PACmoveTrailCounter
        StartTime = PAC2Co[i-1,1]
        EndTime <- PAC2Co[i,1]
        TrialEvent <- PACstayString
        
        newTrialRow <- data.frame(Time, Participant, Condition, Trial, StartTime, EndTime, TrialEvent)
        PACmoveDF <- rbind(PACmoveDF, newTrialRow)
      }else{
        print("bad")
      }
    }
  }
  
  avgPCA2CoTime <- totalTime/totalCounter

  
  Age <- strtoi(PerceptionActionDF[5,5])
  Participant <- PerceptionActionDF[5,2]
  
  condition = "s"
  pacStay = avgPCA2Time
  pacMove = avgPCA1Time
  newPartRow <- data.frame(Participant,Age, pacStay, pacMove, condition, partGroup)
  newPartData <- rbind(newPartData, newPartRow)
  
  condition = "co"
  pacStay = avgPCA2CoTime
  pacMove = avgPCA1CoTime
  newPartRow2 <- data.frame(Participant,Age, pacStay, pacMove, condition, partGroup)
  newPartData <- rbind(newPartData, newPartRow2)
  
  write.csv(newPartData, "pacMoving_2-25-23_FullTime.csv")


}

plot(PACmoveDF$Trial,PACmoveDF$Time,pch=16, col = PACmoveDF$TrialEvent)



