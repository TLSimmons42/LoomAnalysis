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


singlePlacedf <- data.frame(Participant = factor(),
                            Condition = factor(),
                            Trial = numeric(),
                            Age = numeric(),
                            Gender = factor(),
                            Group = factor(),
                            MovementTime = numeric(),
                            PlaceStart = numeric(),
                            PlaceEnd = numeric(),
                            Color = factor(),
                            stringsAsFactors = FALSE)

singleGrabdf <- data.frame(Participant = factor(),
                            Condition = factor(),
                            Trial = numeric(),
                            Age = numeric(),
                            Gender = factor(),
                            Group = factor(),
                            MovementTime = numeric(),
                            GrabStart = numeric(),
                            GrabEnd = numeric(),
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
  df <- df %>% mutate(areaEvent = ifelse(is.na(areaEvent),"none",areaEvent))
  df <- df %>% filter(!(areaEvent == "looking at View wall" & zAreaPos > 0))
  df <- df %>% filter(!(areaEvent == "looking at Build wall" & zAreaPos < 0))

  
  
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
  
  
  grab2placeDF <- df %>% filter(!is.na(MovementEvent))
  grabDF <- df %>% filter(MovementEvent == "grab")
  placeDF <- df %>% filter(MovementEvent == "place")
  
  # xFull <- as.numeric(df$xAreaPos)
  # yFull <-as.numeric(df$yAreaPos)
  # zFull <- as.numeric(df$zAreaPos) 
  # 
  # plot3d(xFull, yFull, zFull)

  
  #Place movement loop
  #__________________________________

  temp <- nrow(placeDF) - 13
  for(i in 1:temp){
    currentTime <- placeDF$TimeStamp[i]
    currentColor <- placeDF$CubeColor[i]
    shortDF <- df %>% filter(TimeStamp <= currentTime & TimeStamp >= currentTime - 20000000)
    shortDF <- shortDF %>% mutate(targetEvent = ifelse(is.na(targetEvent),"none",targetEvent))
    shortDF <- shortDF %>% mutate(areaEvent = ifelse(is.na(areaEvent),"none",areaEvent))
    
    print(currentColor)


    counter<- 0
    temp2 <- nrow(shortDF)-1
    for(b in temp2:1){
      if(nrow(shortDF)<2){
        break
      }
      Participant <- shortDF$Participant[temp2 + 1]
      Condition <- shortDF$Condition[temp2 + 1]
      Age <- shortDF$Age[temp2 + 1]
      Trial <- shortDF$Trial[temp2 + 1]
      Gender <- shortDF$Gender[temp2 + 1]
      Group <- shortDF$Group[temp2 + 1]
      Color <- shortDF$CubeColor[temp2 + 1]

      if(shortDF$targetEvent[b] != "looking at Drop Zone" & shortDF$areaEvent[b] != "looking at Build wall"){
        print(counter)

        PlaceEnd <- shortDF$TimeStamp[temp2 + 1]
        PlaceStart <- shortDF$TimeStamp[b + 1]
        MovementTime <- (PlaceEnd - PlaceStart)/10000

        if(counter > 0){
          newPartRow <- data.frame(Participant, Condition, Trial, Age, Gender, Group,
                                   MovementTime, PlaceStart, PlaceEnd, Color)
          singlePlacedf <- rbind(singlePlacedf, newPartRow)
        }
        counter<- 0

        break
      }else{
        counter <- counter + 1
      }

      
      if(b == 1){
        print("we all done")
        print(counter)
        PlaceEnd <- shortDF$TimeStamp[temp2 + 1]
        PlaceStart <- shortDF$TimeStamp[b + 1]
        MovementTime <- (PlaceEnd - PlaceStart)/10000

        newPartRow <- data.frame(Participant, Condition, Trial, Age, Gender, Group,
                                 MovementTime, PlaceStart, PlaceEnd, Color)
        singlePlacedf <- rbind(singlePlacedf, newPartRow)
      }
    }
  }


  
  
  # #grab movement loop
  # #__________________________________
  # 
  # temp <- nrow(grabDF) 
  # for(i in 1:temp){
  #   currentTime <- grabDF$TimeStamp[i]
  #   currentColor <- grabDF$CubeColor[i]
  #   shortDF <- df %>% filter(TimeStamp <= currentTime & TimeStamp >= currentTime - 20000000)
  #   shortDF <- shortDF %>% filter(!is.na(targetEvent))
  #   print(currentColor)
  #   
  #   
  #   counter<- 0
  #   temp2 <- nrow(shortDF)-1
  #   for(b in temp2:1){
  #     if(nrow(shortDF)<2){
  #       break
  #     }
  #     Participant <- shortDF$Participant[temp2 + 1]
  #     Condition <- shortDF$Condition[temp2 + 1]
  #     Age <- shortDF$Age[temp2 + 1]
  #     Trial <- shortDF$Trial[temp2 + 1]
  #     Gender <- shortDF$Gender[temp2 + 1]
  #     Group <- shortDF$Group[temp2 + 1]
  #     Color <- shortDF$CubeColor[temp2 + 1]
  #     
  #     if(currentColor == "blue"){
  #       if(shortDF$targetEvent[b] != "looking at blue cube"){
  #         print(shortDF$targetEvent[b])
  #         counter<- 0
  #         
  #         GrabEnd <- shortDF$TimeStamp[temp2 + 1]
  #         GrabStart <- shortDF$TimeStamp[b + 1]
  #         MovementTime <- (GrabEnd - GrabStart)/10000
  #         
  #         if(counter > 0){
  #           newPartRow <- data.frame(Participant, Condition, Trial, Age, Gender, Group,
  #                                    MovementTime, GrabStart, GrabEnd, Color)
  #           singleGrabdf <- rbind(singleGrabdf, newPartRow)
  #         }
  #         counter<- 0
  #         
  #         break
  #       }else{
  #         # print(counter)
  #         counter <- counter + 1
  #       }
  #       
  #     }
  #     if(currentColor == "red"){
  #       if(shortDF$targetEvent[b] != "looking at red cube"){
  #         print(shortDF$targetEvent[b])
  #         
  #         GrabEnd <- shortDF$TimeStamp[temp2 + 1]
  #         GrabStart <- shortDF$TimeStamp[b + 1]
  #         MovementTime <- (GrabEnd - GrabStart)/10000
  #         
  #         if(counter > 0){
  #           newPartRow <- data.frame(Participant, Condition, Trial, Age, Gender, Group,
  #                                    MovementTime, GrabStart, GrabEnd, Color)
  #           singleGrabdf <- rbind(singleGrabdf, newPartRow)
  #         }
  #         counter<- 0
  #         break
  #       }else{
  #         # print(counter)
  #         counter <- counter + 1
  #       }
  #       
  #     }
  #     if(currentColor == "gray"){
  #       if(shortDF$targetEvent[b] != "looking at invis cube"){
  #         print(shortDF$targetEvent[b])
  #         
  #         GrabEnd <- shortDF$TimeStamp[temp2 + 1]
  #         GrabStart <- shortDF$TimeStamp[b + 1]
  #         MovementTime <- (GrabEnd - GrabStart)/10000
  #         
  #         if(counter > 0){
  #           newPartRow <- data.frame(Participant, Condition, Trial, Age, Gender, Group,
  #                                    MovementTime, GrabStart, GrabEnd, Color)
  #           singleGrabdf <- rbind(singleGrabdf, newPartRow)
  #         }
  #         counter<- 0
  #         break
  #       }else{
  #         # print(counter)
  #         counter <- counter + 1
  #       }
  #       
  #     }
  #     if(currentColor == "gold"){
  #       if(shortDF$targetEvent[b] != "looking at gold cube"){
  #         print(shortDF$targetEvent[b])
  #         
  #         GrabEnd <- shortDF$TimeStamp[temp2 + 1]
  #         GrabStart <- shortDF$TimeStamp[b + 1]
  #         MovementTime <- (GrabEnd - GrabStart)/10000
  #         
  #         if(counter > 0){
  #           newPartRow <- data.frame(Participant, Condition, Trial, Age, Gender, Group,
  #                                    MovementTime, GrabStart, GrabEnd, Color)
  #           singleGrabdf <- rbind(singleGrabdf, newPartRow)
  #         }
  #         counter<- 0
  #         break
  #       }else{
  #         # print(counter)
  #         counter <- counter + 1
  #       }
  #       
  #     }
  #     if(b == 1){
  #       print("we all done")
  #       print(counter)
  #       GrabEnd <- shortDF$TimeStamp[temp2 + 1]
  #       GrabStart <- shortDF$TimeStamp[b]
  #       MovementTime <- (GrabEnd - GrabStart)/10000
  #       
  #       newPartRow <- data.frame(Participant, Condition, Trial, Age, Gender, Group,
  #                                MovementTime, GrabStart, GrabEnd, Color)
  #       singleGrabdf <- rbind(singleGrabdf, newPartRow)
  #     }
  #   }
  # }
  # 
  # 
  
  #grab to place movement loop
  #__________________________________
  # for(j in 1:4){
  # 
  #   if(j == 1){
  #     trialDF <- grab2placeDF %>% filter(Condition == "s" & Trial == 1 )
  #     print(nrow(trialDF))
  #   }else if(j == 2){
  #     trialDF <- grab2placeDF %>% filter(Condition == "s" & Trial == 2)
  #     print(nrow(trialDF))
  #   }else if(j == 3){
  #     trialDF <- grab2placeDF %>% filter(Condition == "co" & Trial == 1)
  #     print(nrow(trialDF))
  #   }else if(j == 4){
  #     trialDF <- grab2placeDF %>% filter(Condition == "co" & Trial == 2)
  #     print(nrow(trialDF))
  #   }
  #   if(nrow(trialDF) < 1){
  #     next
  #   }
  #   
  #   lookingForGrab <- TRUE
  #   lookingForPlace <- FALSE
  # 
  #   
  #   startOfSequence <- ""
  #   enventCounter <- 0
  #   
  #   StartPosX = ""
  #   StartPosY = ""
  #   StartPosZ = ""
  #   grabColor <- ""
  #   grabTime <- ""
  #   
  #   
  #  
  #   
  # 
  #   for (i in 1:nrow(trialDF)) {
  #     currentEvent <- trialDF$MovementEvent[i]
  #     currentColor <- trialDF$CubeColor[i]
  #     TimeStamp <- trialDF$TimeStamp[i]
  #     
  #     Participant <- trialDF$Participant[i]
  #     Condition <- trialDF$Condition[i]
  #     Trial <- trialDF$Trial[i]
  #     Age <- trialDF$Age[i]
  #     Gender <- trialDF$Gender[i]
  #     Group <- trialDF$Group[i]
  #     
  #     
  #     if(!lookingForGrab){
  #       lookingForGrab <- TRUE
  #       if(currentEvent == "place"){
  #         if(currentColor == grabColor){
  #           
  #           MovementTime <- (TimeStamp - grabTime)/10000
  #           newPartRow <- data.frame(Participant, Condition, Trial, Age, Gender, Group,
  #                                    MovementTime, grabTime, TimeStamp, currentColor)
  #           singleGrab2PlaceMTdf <- rbind(singleGrab2PlaceMTdf, newPartRow)
  #         }else{
  #           # print("bad")
  #           # print(currentColor)
  #           # print(grabColor)
  #         }
  #       }else{
  #         # print("Not a place")
  #         # print(currentColor)
  #         # print(grabColor)
  #       }
  #     }
  #     
  #     
  #     if(lookingForGrab){
  #       if(currentEvent == "grab"){
  #         lookingForGrab <- FALSE
  #         grabTime <- TimeStamp
  #         # lookingForPlace <- TRUE
  #         
  #         if(currentColor == "red"){
  #           grabColor <- "red"
  #         }
  #         if(currentColor == "blue"){
  #           grabColor <- "blue"
  #         }
  #         if(currentColor == "gray"){
  #           grabColor <- "gray"
  #         }
  #         if(currentColor == "gold"){
  #           grabColor <- "gold"
  #         }
  #       }
  #       
  #     }
  #   }
  # }
}

# singleGrab2PlaceMTdf <- singleGrab2PlaceMTdf %>% filter(MovementTime > 50)

# write.csv(singleGrab2PlaceMTdf, "singleGrab2PlaceMTdf.csv", row.names = FALSE)
# write.csv(singleGrabdf, "singleGrabDF.csv", row.names = FALSE)
write.csv(singlePlacedf, "singlePlaceDF.csv", row.names = FALSE)

