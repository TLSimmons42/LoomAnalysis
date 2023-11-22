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
                                   MovementTime = numeric(),
                                   GrabTime = numeric(),
                                   PlaceTime = numeric(),
                                   Color = factor(),
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


data_files <- list.files(pattern = ".csv")

# combindedDataFile <- "combindedDataFile P4.csv"
# data_files <- combindedDataFile
# print(data_files)
movementEventDF <- ""

for(f in 1:length(data_files))
{
  combindedDataFile <- data_files[f]
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

    if(j == 1){
      trialDF <- df %>% filter(Condition == "s" & Trial == 1 )
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
    if(nrow(trialDF) < 1){
      next
    }
    
    lookingForGrab <- TRUE
    lookingForPlace <- FALSE

    
    startOfSequence <- ""
    enventCounter <- 0
    
    StartPosX = ""
    StartPosY = ""
    StartPosZ = ""
    grabColor <- ""
    grabTime <- ""
    
    for (i in 1:nrow(trialDF)) {
      currentEvent <- trialDF$MovementEvent[i]
      currentColor <- trialDF$CubeColor[i]
      TimeStamp <- trialDF$TimeStamp[i]
      
      Participant <- trialDF$Participant[i]
      Condition <- trialDF$Condition[i]
      Trial <- trialDF$Trial[i]
      Age <- trialDF$Age[i]
      Gender <- trialDF$Gender[i]
      Group <- trialDF$Group[i]
      
      
      if(!lookingForGrab){
        lookingForGrab <- TRUE
        if(currentEvent == "place"){
          if(currentColor == grabColor){
            
            MovementTime <- (TimeStamp - grabTime)/10000
            newPartRow <- data.frame(Participant, Condition, Trial, Age, Gender, Group,
                                     MovementTime, grabTime, TimeStamp, currentColor)
            singleGrab2PlaceMTdf <- rbind(singleGrab2PlaceMTdf, newPartRow)
          }else{
            # print("bad")
            # print(currentColor)
            # print(grabColor)
          }
        }else{
          # print("Not a place")
          # print(currentColor)
          # print(grabColor)
        }
      }
      
      
      if(lookingForGrab){
        if(currentEvent == "grab"){
          lookingForGrab <- FALSE
          grabTime <- TimeStamp
          # lookingForPlace <- TRUE
          
          if(currentColor == "red"){
            grabColor <- "red"
          }
          if(currentColor == "blue"){
            grabColor <- "blue"
          }
          if(currentColor == "gray"){
            grabColor <- "gray"
          }
          if(currentColor == "gold"){
            grabColor <- "gold"
          }
        }
        
      }
    }
  }
}

singleGrab2PlaceMTdf <- singleGrab2PlaceMTdf %>% filter(MovementTime > 50)

# write.csv(singleGrab2PlaceMTdf, "singleGrab2PlaceMTdf.csv", row.names = FALSE)

