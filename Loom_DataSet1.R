#check the working directory
getwd()
#set the WD under the Session dropdown



# import the Perception action coupling csv file
# header = TRUE means that the first like is a header line
# PerceptionAction1 <- read.csv("analytics_P2.csv", header = TRUE, sep = ",")
# PerceptionActionDF1 <- data.frame(PerceptionAction1)
# 
# PerceptionAction2 <- read.csv("analytics_P3.csv", header = TRUE, sep = ",")
# PerceptionActionDF2 <- data.frame(PerceptionAction2)
# 
# PerceptionAction3 <- read.csv("analytics_P4.csv", header = TRUE, sep = ",")
# PerceptionActionDF3 <- data.frame(PerceptionAction3)
# 
# PerceptionAction4 <- read.csv("analytics_P5.csv", header = TRUE, sep = ",")
# PerceptionActionDF4 <- data.frame(PerceptionAction4)
# 
# PerceptionAction5 <- read.csv("analytics_P6.csv", header = TRUE, sep = ",")
# PerceptionActionDF5 <- data.frame(PerceptionAction5)
# 
# PerceptionAction6 <- read.csv("analytics_P7.csv", header = TRUE, sep = ",")
# PerceptionActionDF6 <- data.frame(PerceptionAction6)
# 
# PerceptionAction7 <- read.csv("analytics_P8.csv", header = TRUE, sep = ",")
# PerceptionActionDF7 <- data.frame(PerceptionAction7)
# 
# PerceptionAction8 <- read.csv("analytics_P9.csv", header = TRUE, sep = ",")
# PerceptionActionDF8 <- data.frame(PerceptionAction8)
# 
# PerceptionAction9 <- read.csv("analytics_P10.csv", header = TRUE, sep = ",")
# PerceptionActionDF9 <- data.frame(PerceptionAction9)
# 
# PerceptionAction10 <- read.csv("analytics_P11.csv", header = TRUE, sep = ",")
# PerceptionActionDF10 <- data.frame(PerceptionAction10)
# 
# PerceptionAction11 <- read.csv("analytics_P12.csv", header = TRUE, sep = ",")
# PerceptionActionDF11 <- data.frame(PerceptionAction11)
# 
# PerceptionAction12 <- read.csv("analytics_P13.csv", header = TRUE, sep = ",")
# PerceptionActionDF12 <- data.frame(PerceptionAction12)
# 
# PerceptionAction13 <- read.csv("analytics_P14.csv", header = TRUE, sep = ",")
# PerceptionActionDF13 <- data.frame(PerceptionAction13)
# 
# PerceptionAction14 <- read.csv("analytics_P16.csv", header = TRUE, sep = ",")
# PerceptionActionDF14 <- data.frame(PerceptionAction14)
# 
# listOfDataFrames <- list(PerceptionActionDF1, PerceptionActionDF2, PerceptionActionDF3, PerceptionActionDF4, PerceptionActionDF5, PerceptionActionDF6,
#                          PerceptionActionDF7, PerceptionActionDF8, PerceptionActionDF9, PerceptionActionDF10, PerceptionActionDF11, PerceptionActionDF12,
#                          PerceptionActionDF13,PerceptionActionDF14)
# 
# library(data.table)
# files <- list.files(pattern = ".csv")
# temp <- lapply(files, fread, sep = ",")
# data <- rbind(temp)
# write.csv(data, "Compleate_PCA.csv",row.names = FALSE)


PerceptionActionDF <- read.csv("analytics_P3.csv", header = TRUE, sep = ",")
partGroup <- "e"


#Set up the new PAC1 and PAC2 DF's
PAC1 <- data.frame(TimeStamp = numeric(),
                   Participant = factor(),
                   Condition = factor(),
                   Trial = numeric(), b
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
  
  
  # This will average all of the sequence times for PCA sequence 1
  avgPCA1Time <- 0
  counter <- 0
  totalCounter <- 0
  totalTime <- 0
  startValue <- 0
  
  for (i in 1:nrow(PAC1))
  {
    counter <- counter + 1
    if(counter == 1) {
      startValue <- PAC1[i,1]/10000
    } else {
      reactionTime <- (PAC1[i,1]/10000) - startValue
      totalTime <- totalTime + reactionTime
      counter <- 0
      totalCounter <- totalCounter + 1
    }
  }
  
  avgPCA1Time <- totalTime/totalCounter
  print(avgPCA1Time)
  
  
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
      reactionTime <- (PAC2[i,1]/10000) - startValue
      totalTime <- totalTime + reactionTime
      counter <- 0
      totalCounter <- totalCounter + 1
    }
  }
  
  avgPCA2Time <- totalTime/totalCounter
  print(avgPCA2Time)
  
  
  
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
      reactionTime <- (PAC1Co[i,1]/10000) - startValue
      totalTime <- totalTime + reactionTime
      counter <- 0
      totalCounter <- totalCounter + 1
    }
  }
  
  avgPCA1CoTime <- totalTime/totalCounter
  print(avgPCA1CoTime)
  
  
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
      reactionTime <- (PAC2Co[i,1]/10000) - startValue
      totalTime <- totalTime + reactionTime
      counter <- 0
      totalCounter <- totalCounter + 1
    }
  }
  
  avgPCA2CoTime <- totalTime/totalCounter
  print(avgPCA2CoTime)
  
  
  
}



# newPartData <- data.frame(Participant = factor(),
#                           pacStay = numeric(),
#                           pacMove = numeric(),
#                           pacStayCo = numeric(),
#                           pacoMoveCo = numeric(),
#                           group = factor(),
#                           stringsAsFactors = FALSE)


newPartRow <- data.frame(PerceptionActionDF[5,2], avgPCA2Time, avgPCA1Time, avgPCA2CoTime, avgPCA1CoTime, partGroup)
newPartData <- rbind(newPartData, newPartRow)

write.csv(newPartData, "pacMoving.csv")






