library(plot3D)
library(rgl)
library(dplyr)
library(bit64)


singleGazeTransferDF <- data.frame(TimeStamp = numeric(),
                                   Participant = factor(),
                                   Condition = factor(),
                                   Trial = numeric(),
                                   Age = numeric(),
                                   Gender = factor(),
                                   Group = factor(),
                                   TransferEvent = factor(),
                                   StartTime = factor(),
                                   EndTime = numeric(), 
                                   StartPosX = numeric(),
                                   StartPosY = numeric(),
                                   StartPosZ = numeric(),
                                   EndPosX = numeric(),
                                   EndPosY = numeric(),
                                   EndPosZ = numeric(),
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


for(f in 1:length(data_files))
{
  combindedDataFile <- data_files[1]
  print(combindedDataFile)
  
  
  
  
  df <- read.csv(combindedDataFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df), ]
  df <- df %>% filter(!(areaEvent == "looking at View wall" & zAreaPos > 0))
  df <- df %>% filter(!(areaEvent == "looking at Build wall" & zAreaPos < 0))
  
  
  # df <- df %>% filter(!is.na(areaEvent))
  # df <- df %>% filter(!is.na(zAreaPos))
  df <- df %>% filter(areaEvent == "looking at Play wall" | areaEvent == "looking at Build wall" | areaEvent == "looking at View wall" 
                      | targetEvent == "looking at red cube" | targetEvent == "looking at gold cube" | targetEvent == "looking at blue cube"
                      | targetEvent == "looking at invis cube" | targetEvent == "looking at Drop Zone")
  
  df <- df %>% mutate(GazeEvents = ifelse(is.na(targetEvent), areaEvent,
                                          ifelse(targetEvent == "looking at red cube" | targetEvent == "looking at gold cube" | targetEvent == "looking at blue cube"
                                                 | targetEvent == "looking at invis cube", "looking at Play wall", 
                                                 ifelse(targetEvent == "looking at Drop Zone"| areaEvent == "looking at Build wall", "looking at Build wall",
                                                        ifelse(areaEvent == "looking at View wall", "looking at View wall", "poo")))))
  
  
  
  
  
  xFull <- as.numeric(df$xAreaPos)
  yFull <-as.numeric(df$yAreaPos)
  zFull <- as.numeric(df$zAreaPos)
  
  plot3d(xFull, yFull, zFull)
  
  
  for(j in 1:4){
    if(combindedDataFile == "combindedDataFile P4.csv"){
      print("hello")
      j <-2
    }
    
    if(j == 1){
      print("bye")
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
    
    lookingForSequenceStart <- TRUE
    lookingForPlayWall <- FALSE
    lookingForBuildWall <- FALSE
    lookingForViewWall <- FALSE 
    
    startOfSequence <- ""
    sequenceCounter <- 0
    
    StartPosX = ""
    StartPosY = ""
    StartPosZ = ""
    
    for (i in 2:nrow(trialDF)) {
      currentEvent <- trialDF$GazeEvents[i]
      previousEvent <- trialDF$GazeEvents[i-1]
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
      
      
      if(!lookingForSequenceStart){
        
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
        
        if(lookingForViewWall){
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

