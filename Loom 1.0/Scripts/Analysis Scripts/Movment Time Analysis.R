library(plot3D)
library(rgl)
library(dplyr)
library(bit64)


singleGrab2PlaceMTdf <- data.frame(Participant = factor(),
                                   Condition = factor(),
                                   Trial = numeric(),
                                   Age = numeric(),
                                   Gender = factor(),
                                   Group = factor(),
                                   MovementTime = factor(),
                                   StartTime = factor(),
                                   EndTime = numeric(), 
                                   stringsAsFactors = FALSE)

avgGazeTranferTimes <- data.frame(TimeStamp = numeric(),
                                  Participant = factor(),
                                  Condition = factor(),
                                  Trial = numeric(),
                                  Age = numeric(),
                                  Gender = factor(),
                                  Group = factor(),
                                  avgPlay2Build = numeric(),
                                  avgPlay2View = numeric(),
                                  avgBuild2Play = numeric(),
                                  avgView2Play = numeric(),
                                  totalPlay2Build = numeric(),
                                  totalPlay2View = numeric(),
                                  totalBuild2Play = numeric(),
                                  totalView2Play = numeric(),
                                  stringsAsFactors = FALSE)


data_files <- list.files(pattern = "26.csv")

# combindedDataFile <- "combindedDataFile P4.csv"
# data_files <- combindedDataFile
# print(data_files)
movementEventDF <- ""

for(f in 1:length(data_files))
{
  combindedDataFile <- data_files[1]
  print(combindedDataFile)
  
  
  df <- read.csv(combindedDataFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df), ]
  
  df <- df %>% mutate(CubeColor = ifelse(is.na(targetEvent), NA,
                                         ifelse(targetEvent == "Regular Red Cube(Clone) was picked up" | targetEvent == "Regular Red Cube(Clone)was placed in dropzone" | targetEvent == "Network Red Cube(Clone) was picked up" | targetEvent == "Network Red Cube(Clone)was placed in dropzone", "red",
                                                ifelse(targetEvent == "Regular Gold Cube(Clone) was picked up" | targetEvent == "Regular Gold Cube(Clone)was placed in dropzone", "gold",
                                                       ifelse(targetEvent == "Regular Blue Cube(Clone) was picked up" | targetEvent == "Regular Blue Cube(Clone)was placed in dropzone" | targetEvent == "Network Blue Cube(Clone) was picked up" | targetEvent == "Network Blue Cube(Clone)was placed in dropzone", "blue",
                                                              ifelse(targetEvent == "Regular Neutral Cube(Clone) was picked up" | targetEvent == "Regular Neutral Cube(Clone)was placed in dropzone" | targetEvent == "Network Neutral Cube(Clone) was picked up" | targetEvent == "Network Neutral Cube(Clone)was placed in dropzone", "gray", NA))))))
  
  df <- df %>% mutate(MovementEvent = ifelse(is.na(targetEvent), NA,
                                         ifelse(targetEvent == "Regular Red Cube(Clone) was picked up" | targetEvent == "Network Red Cube(Clone) was picked up" |
                                                  targetEvent == "Regular Blue Cube(Clone) was picked up" | targetEvent == "Network Blue Cube(Clone) was picked up" |
                                                  targetEvent == "Regular Neutral Cube(Clone) was picked up" | targetEvent == "Network Neutral Cube(Clone) was picked up" |
                                                  targetEvent == "Regular Gold Cube(Clone) was picked up", "grab",
                                                ifelse(targetEvent == "Regular Red Cube(Clone)was placed in dropzone" | targetEvent == "Network Red Cube(Clone)was placed in dropzone" |
                                                         targetEvent == "Regular Blue Cube(Clone)was placed in dropzone" | targetEvent == "Network Blue Cube(Clone)was placed in dropzone" |
                                                         targetEvent == "Regular Neutral Cube(Clone)was placed in dropzone" | targetEvent == "Network Neutral Cube(Clone)was placed in dropzone" |
                                                         targetEvent == "Regular Gold Cube(Clone)was placed in dropzone", "place", NA))))
  
  
  df <- df %>% filter(!is.na(MovementEvent))
  
  
  # xFull <- as.numeric(df$xAreaPos)
  # yFull <-as.numeric(df$yAreaPos)
  # zFull <- as.numeric(df$zAreaPos)
  # 
  # plot3d(xFull, yFull, zFull)
  
  
  for(j in 1:4){
    if(combindedDataFile == "combindedDataFile P4.csv"){
      j <-2
    }
    if(j == 1){
      trialDF <- df %>% filter(Condition == "s" & Trial == 1)
      print(nrow(trialDF))
    }else if(j == 2){
      trialDF <- df %>% filter(Condition == "s" & Trial == 2)
      print(nrow(trialDF))
    }else if(j == 3){
      trialDF <- df %>% filter(Condition == "co" & Trial == 1)
      print(nrow(trialDF))
    }else if(j == 4){
      trialDF <- df %>% filter(Condition == "co" & Trial == 2)
      print(nrow(trialDF))
    }
    
    lookingForGrab <- TRUE
    lookingForPlace <- FALSE

    
    startOfSequence <- ""
    enventCounter <- 0
    
    StartPosX = ""
    StartPosY = ""
    StartPosZ = ""
    
    for (i in 1:nrow(trialDF)) {
      currentEvent <- trialDF$MovementEvent[i]
      TimeStamp <- trialDF$TimeStamp[i]
      
      currentXpos <- trialDF$xAreaPos[i]
      currentYpos <- trialDF$yAreaPos[i]
      currentZpos <- trialDF$zAreaPos[i]
      
      Participant <- trialDF$Participant[i]
      Condition <- trialDF$Condition[i]
      Trial <- trialDF$Trial[i]
      Age <- trialDF$Age[i]
      Gender <- trialDF$Gender[i]
      Group <- trialDF$Group[i]
      
      
      if(!lookingForGrab){
        
        if(lookingForPlayWall){
          if(currentEvent == "looking at Play wall"){
            sequenceCounter <- sequenceCounter + 1
          }else if(currentEvent != "looking at Play wall" & sequenceCounter > 1){
            lookingForSequenceStart <- TRUE
            lookingForPlayWall <- FALSE
            sequenceCounter <- 0
            
            if(currentEvent == "looking at View wall"){
              TransferEvent <- "P2V"
            }else if(currentEvent == "looking at Build wall"){
              TransferEvent <- "P2B"
            }
            StartTime <- trialDF$TimeStamp[i-1]
            EndTime <- trialDF$TimeStamp[i]
            EndPosX <- currentXpos
            EndPosY <- currentYpos
            EndPosZ <- currentZpos
            
            
            newPartRow <- data.frame(TimeStamp, Participant, Condition, Trial, Age, Gender, Group,
                                     TransferEvent, StartTime, EndTime, StartPosX, StartPosY,
                                     StartPosZ,EndPosX, EndPosY, EndPosZ)
            singleGazeTransferDF <- rbind(singleGazeTransferDF, newPartRow)
            
          }else{
            lookingForSequenceStart <- TRUE
            lookingForPlayWall <- FALSE
            sequenceCounter <- 0
          }
        }
        if(lookingForBuildWall){
          if(currentEvent == "looking at Build wall"){
            sequenceCounter <- sequenceCounter + 1
          }else if(currentEvent != "looking at Build wall" & sequenceCounter > 1){
            lookingForSequenceStart <- TRUE
            lookingForBuildWall <- FALSE
            sequenceCounter <- 0
            
            if(currentEvent == "looking at View wall"){
              TransferEvent <- "B2V"
            }else if(currentEvent == "looking at Play wall"){
              TransferEvent <- "B2P"
            }
            StartTime <- trialDF$TimeStamp[i-1]
            EndTime <- trialDF$TimeStamp[i]
            EndPosX <- currentXpos
            EndPosY <- currentYpos
            EndPosZ <- currentZpos
            
            
            newPartRow <- data.frame(TimeStamp, Participant, Condition, Trial, Age, Gender, Group,
                                     TransferEvent, StartTime, EndTime, StartPosX, StartPosY,
                                     StartPosZ,EndPosX, EndPosY, EndPosZ)
            singleGazeTransferDF <- rbind(singleGazeTransferDF, newPartRow)
            
          }else{
            lookingForSequenceStart <- TRUE
            lookingForBuildWall <- FALSE
            sequenceCounter <- 0
          }
        }
        
        if(lookingForGrab){
          if(currentEvent == "looking at View wall"){
            sequenceCounter <- sequenceCounter + 1
          }else if(currentEvent != "looking at View wall" & sequenceCounter > 1){
            lookingForSequenceStart <- TRUE
            lookingForViewWall <- FALSE
            sequenceCounter <- 0
            
            if(currentEvent == "looking at Build wall"){
              TransferEvent <- "V2B"
            }else if(currentEvent == "looking at Play wall"){
              TransferEvent <- "V2P"
            }
            StartTime <- trialDF$TimeStamp[i-1]
            EndTime <- trialDF$TimeStamp[i]
            EndPosX <- currentXpos
            EndPosY <- currentYpos
            EndPosZ <- currentZpos
            
            
            newPartRow <- data.frame(TimeStamp, Participant, Condition, Trial, Age, Gender, Group,
                                     TransferEvent, StartTime, EndTime, StartPosX, StartPosY,
                                     StartPosZ,EndPosX, EndPosY, EndPosZ)
            singleGazeTransferDF <- rbind(singleGazeTransferDF, newPartRow)
            
          }else{
            lookingForSequenceStart <- TRUE
            lookingForBuildWall <- FALSE
            sequenceCounter <- 0
          }
        }
      }
      
      
      if(lookingForSequenceStart){
        if(currentEvent == "looking at Play wall"){
          lookingForSequenceStart <- FALSE
          
          lookingForPlayWall <- TRUE
          startOfSequence <- trialDF$TimeStamp[i]
          StartPosX <- trialDF$xAreaPos[i]
          StartPosY <- trialDF$yAreaPos[i]
          StartPosZ <- trialDF$zAreaPos[i]
          
        }
        if(currentEvent == "looking at Build wall"){
          lookingForSequenceStart <- FALSE
          
          lookingForBuildWall <- TRUE
          startOfSequence <- trialDF$TimeStamp[i]
        }
        if(currentEvent == "looking at View wall"){
          lookingForSequenceStart <- FALSE
          
          lookingForViewWall <- TRUE
          startOfSequence <- trialDF$TimeStamp[i]
        }
      }
      
      
    }
    
    
  }
  
}  


singleGazeTransferDF <- singleGazeTransferDF %>% mutate(B2P = ifelse(TransferEvent == "B2P",(EndTime - StartTime)/10000, NA))

singleGazeTransferDF <- singleGazeTransferDF %>% mutate(B2V = ifelse(TransferEvent == "B2V",(EndTime - StartTime)/10000, NA))
singleGazeTransferDF <- singleGazeTransferDF %>% mutate(P2V = ifelse(TransferEvent == "P2V",(EndTime - StartTime)/10000, NA))
singleGazeTransferDF <- singleGazeTransferDF %>% mutate(P2B = ifelse(TransferEvent == "P2B",(EndTime - StartTime)/10000, NA))
singleGazeTransferDF <- singleGazeTransferDF %>% mutate(V2P = ifelse(TransferEvent == "V2P",(EndTime - StartTime)/10000, NA))
singleGazeTransferDF <- singleGazeTransferDF %>% mutate(V2B = ifelse(TransferEvent == "V2B",(EndTime - StartTime)/10000, NA))

write.csv(singleGazeTransferDF, "singleGazeTransferDF.csv", row.names = FALSE)

