library(plot3D)
library(rgl)
library(dplyr)
library(bit64)
library(stringr)
setwd("C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Merged Data")


data_files <- list.files(pattern = "nuP39(\\D|$)")
output_file <- "C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/PAC Merged Data/nuP39_PAC.csv"  # Change this to your desired output file


strings_to_filter <- c("nuP2_old1","nuP2_old2","nuP2_old3","nuP2_old4")
data_files <- data_files[!(grepl(paste(strings_to_filter, collapse="|"), data_files))]

 

PACdf <- data.frame(Time = numeric(),
                   Participant = factor(),
                   Condition = factor(), 
                   Trial = numeric(),
                   Group = factor(),
                   grabPACcount = numeric(),
                   placePACcount = numeric(),
                   avgGrabPAC = numeric(),
                   avgPlacePAC = numeric(),
                   grabAreaPACcount = numeric(),
                   placeAreaPACcount = numeric(),
                   avgGrabAreaPAC = numeric(),
                   avgPlaceAreaPAC = numeric(),
                   avgGoldCubeGrab = numeric(),
                   totalGoldCubeGrabCount = numeric(),
                   avgBlueCubeGrab = numeric(),
                   totalBlueCubeGrabCount = numeric(),
                   avgRedCubeGrab = numeric(),
                   totalRedCubeGrabCount = numeric(),
                   avgWhiteCubeGrab = numeric(),
                   totalWhiteCubeGrabCount = numeric(),
                   grab2PlaceTime = numeric(),
                   grab2PlaceCount = numeric(),
                   stringsAsFactors = FALSE)

PACAreadf <- data.frame(Time = numeric(),
                    Participant = factor(),
                    Condition = factor(),
                    Trial = numeric(),
                    Group = factor(),
                    grabAreaPACcount = numeric(),
                    placeAreaPACcount = numeric(),
                    avgGrabAreaPAC = numeric(),
                    avgPlaceAreaPAC = numeric(),
                    stringsAsFactors = FALSE)

individualPACdf <- data.frame(Time = numeric(),
                    Participant = factor(),
                    Condition = factor(),
                    Trial = numeric(),
                    Group = factor(),
                    PACtype = factor(),
                    PACtime = numeric(),
                    PACstartTime = numeric(),
                    PACendTime = numeric(),
                    Event = factor(),
                    cubeColor = numeric(),
                    stringsAsFactors = FALSE)

individualAreaPACdf <- data.frame(Time = numeric(),
                              Participant = factor(),
                              Condition = factor(),
                              Trial = numeric(),
                              Group = factor(),
                              PACtype = factor(),
                              PACtime = numeric(),
                              PACstartTime = numeric(),
                              PACendTime = numeric(),
                              Event = factor(),
                              stringsAsFactors = FALSE)
ColorCubeAnalysis <- function(colorName, eventDuration){
  #print(colorName)
  #print(eventDuration)
  result1 <- grepl("Red", colorName)
  result2 <- grepl("Blue", colorName)
  result3 <- grepl("Gold", colorName)
  result4 <- grepl("Neutral", colorName)
  
  # #result1 <- regexpr("Red", colorName)
  # result2 <- regexpr("Blue", colorName)
  # result3 <- regexpr("Gold", colorName)
  # result4 <- regexpr("Neutral", colorName)
  
  if (result1) {
    colorName <- "Red"
  }
  if (result2) {
    colorName <- "Blue"
  }
  if (result3) {
    colorName <- "Gold"
  }
  if (result4) {
    colorName <- "White"
  }
  
  #print(colorName)
  
  if(colorName == "Blue")
  {
    totalBlueCubeGrabCount <<- totalBlueCubeGrabCount + 1
    totalBlueCubeGrabTime <<- totalBlueCubeGrabTime + eventDuration
    
  }
  if(colorName == "Red")
  {
    totalRedCubeGrabCount <<- totalRedCubeGrabCount + 1
    totalRedCubeGrabTime <<- totalRedCubeGrabTime + eventDuration
    
  }
  if(colorName == "Gold")
  {
    totalGoldCubeGrabCount <<- totalGoldCubeGrabCount + 1
    totalGoldCubeGrabTime <<- totalGoldCubeGrabTime + eventDuration
    
  }
  if(colorName == "White")
  {
    totalWhiteCubeGrabCount <<- totalWhiteCubeGrabCount + 1
    totalWhiteCubeGrabTime <<- totalWhiteCubeGrabTime + eventDuration
    
  }
  return(colorName)
  
}


#Grab2PlaceAnalysis(trimedGrabDF, trimedPlaceDF)

Grab2PlaceAnalysis <- function(grabDF, placeDF){
  Participant <- grabDF[2,2]
  Condition <- grabDF[8,8]
  Trial <- grabDF[9,9]
  Group <- grabDF[7,7]
  if(nrow(placeDF)== 0 | nrow(grabDF) == 0){
    
  }else{
    
    for(i in 1:nrow(grabDF))
    {
      currentEvent <- grabDF[i,10]
      # print(currentEvent)
      
      # Your string
      input_string <- currentEvent
      
      result <- str_extract(input_string, "CubeClone(\\d+)\\swas")
      #print(result)
      # Extracted number
      if (!is.na(result)) {
        extracted_Grabnumber <- str_match(result, "CubeClone(\\d+)\\swas")[, 2]
        #print(extracted_Grabnumber)
        for(f in 1:nrow(placeDF))
        {
          currentPlaceEvent <- placeDF[f,10]
          # print(currentPlaceEvent)
          
          input_string <- currentPlaceEvent
          
          result <- str_extract(input_string, "CubeClone(\\d+)\\was")
          #print(result)
          if (!is.na(result)){
            extracted_number <- str_match(result, "CubeClone(\\d+)\\was")[, 2]
            # print(extracted_number)
            if(extracted_number == extracted_Grabnumber){
              SequenceOrder <<- SequenceOrder + 1
              
              GrabTime <- grabDF[i,1]
              PlaceTime <- placeDF[f,1]
              GrabEvent <- currentEvent
              PlaceEvent <- currentPlaceEvent
              newRow <- data.frame(SequenceOrder, Participant, Condition, Trial, Group, GrabTime,PlaceTime, GrabEvent, PlaceEvent)
              individualGrab2Placedf <<- rbind(individualGrab2Placedf, newRow)
              break
            }
          }else{}
        }
      } else{}
    }
  }
}

BadParticipantAnalysis <- function(event, dfFunc){
  
  dfFunc <- dfFunc %>% filter(Time >= (last(dfFunc$Time)-15000000))
  
  
  foundCube <- FALSE
  result1 <- grepl("Red", event)
  result2 <- grepl("Blue", event)
  result3 <- grepl("Gold", event)
  result4 <- grepl("Neutral", event)
  
  lookForObj <- ""
  returnTime <- 0
  if(result1){
    lookForObj <- "red_cube"
  }
  if(result2){
    lookForObj <- "blue_cube"
  }
  if(result3){
    lookForObj <- "gold_cube"
  }
  if(result4){
    lookForObj <- "white_cube"
  }
  temp <- nrow(dfFunc)
  for (i in 1:nrow(dfFunc)) {
    
    
    
    if(dfFunc$CurrentGazeTarget[i] == lookForObj){
      returnTime <- dfFunc$Time[i]
      #print("FOUND IT")
      foundCube <- TRUE
      break
      #print(returnTime)
    }
    
    if(i == temp){
      #print("got to the end")
    }
  }
  # a <- last(df$Time) - returnTime
  # print(a)
  
  
  return(list(found = foundCube, cubetime = returnTime))
  
}




for(f in 1:length(data_files))
{
  participantDataFile <- data_files[f]
  print(participantDataFile)
  

  
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]
  
  
  df <- df %>%
    filter(as.numeric(HandPos_X) > 0)
  
  df$Event <- gsub("\\(|\\)", "", df$Event)
  df$CurrentGazeTarget <- gsub("\\(|\\)", "", df$CurrentGazeTarget)
  gameOverTime <- df[df$Event == "Game Over",]
  df <- df %>% filter(df$Time <= gameOverTime[1,1])

  
  # Find all of the Picked events
  #trimedGrabDF <- df[grep("picked", df$Event), ]
  trimedGrabDF <- df %>% filter(grepl("picked", Event) | grepl("Player 1 hits", Event))
  trimedGrabDF <- trimedGrabDF %>% filter(CurrentGazeArea == "play_wall")
  trimedGrabDF <- trimedGrabDF %>% filter(!grepl("by", Event))
  trimedGrabDF <- trimedGrabDF %>% filter(!grepl("HB", Event))
  

  trimedPlaceDF <- df[grep("DropzonePlaceHolderClone", df$Event), ]
  trimedPlaceDF <- trimedPlaceDF %>% filter(!grepl("by", Event))
  
  # 
  # trimedPlaceDF <- df[grep("placed", df$Event), ]
  # trimedPlaceDF <- trimedPlaceDF %>% filter(!grepl("Hit", Event))
  # trimedPlaceDF <- trimedPlaceDF %>% filter(!grepl("Hit", Event))
  # trimedPlaceDF <- trimedPlaceDF %>% filter(!grepl("HB", Event))
  # trimedPlaceDF <- trimedPlaceDF[!duplicated(trimedPlaceDF$Event), ]
  # trimedPlaceDF <- trimedPlaceDF %>% filter(CurrentGazeArea == "build_wall")
  
  
  if(df[1,8] != "solo"){
    trimedP2GrabDF <- df[grep("picked", df$Event), ]
    trimedP2GrabDF <- trimedP2GrabDF %>% filter(grepl("P2", Event))
    
    for(b in 1:nrow(trimedP2GrabDF))
    {
      input_string <- trimedP2GrabDF[b,10]
      
      # Find the position of " was picked up"
      pos <- regexpr(" was picked up by P2", input_string)
      
      # Extract the substring before the target text
      result <- substr(input_string, 1, pos - 1)
      result <- paste(result, "was", sep = "")
      #result <- gsub("(Clone)", "Clone", result)
      
    
      #trimedPlaceDF <- trimedPlaceDF %>% filter(!grepl(result, Event))
      
    }
  }
  
  
  
 
  #trimedGrabDF <- trimedGrabDF %>% filter(grepl("Red", Event))
  
  SequenceOrder <- 0
  
  grabPACcount <- 0
  placePACcount <- 0
  grabAreaPACcount <- 0
  placeAreaPACcount <- 0
  
  totalGrabPAC <- 0
  totalPlacePAC <- 0
  totalGrabAreaPAC <- 0
  totalPlaceAreaPAC <- 0
  
  avgGrabPAC <- 0
  avgPlacePAC <- 0
  avgGrabAreaPAC <- 0
  avgPlaceAreaPAC <- 0

  
  
  PACstartTime <- 0
  PACendTime <- 0
  
  
  avgGoldCubeGrab <- 0
  totalGoldCubeGrabCount <- 0
  totalGoldCubeGrabTime <- 0
  
  avgBlueCubeGrab <- 0
  totalBlueCubeGrabCount <- 0
  totalBlueCubeGrabTime <- 0
  
  avgRedCubeGrab <- 0
  totalRedCubeGrabCount <- 0
  totalRedCubeGrabTime <- 0
  
  avgWhiteCubeGrab <- 0
  totalWhiteCubeGrabCount <- 0
  totalWhiteCubeGrabTime <- 0
  
  cubeColor <- ""
  
  

  
  
  #trimedGrabDF <- trimedGrabDF %>% filter(grepl("Gold",Event))
  for(i in 1:nrow(trimedGrabDF))
  #for(i in 1:1)
  {
    currentTime <- trimedGrabDF[i,1]
    currentGazeArea <- trimedGrabDF[i,12]
    currentEvent <- trimedGrabDF[i,10]
    currentHandX <- trimedGrabDF[i,25]
    currentHandY <- trimedGrabDF[i,26]
    currentHandZ <- trimedGrabDF[i,27]
    
    subDF <- df %>% filter(Time > (currentTime-20000000) & Time <= currentTime)

    for (v in nrow(subDF):1){
      if(subDF$CurrentGazeArea[v] != "play_wall"){
        subDF <- subDF[(v+1):nrow(subDF),]
        break
      }else{
      }
    }
    
    Participant <- trimedGrabDF[2,2]
    Condition <- trimedGrabDF[8,8]
    Trial <- trimedGrabDF[9,9]
    Group <- trimedGrabDF[7,7]
    
    
    for(m in nrow(subDF):1)
    {
      if(subDF[m,12] != "play_wall")
      {
        PACtype <- "grab"
        PACendTime <- currentTime
        PACstartTime <- subDF[m,1]
        PACtime <- (PACendTime - PACstartTime)/10000
        cubeColor <- ColorCubeAnalysis(currentEvent, PACtime)
        
        
        
        newPartRow <- data.frame(Participant, Condition, Trial, Group, PACtype, PACtime, PACstartTime, PACendTime, currentEvent)
        individualAreaPACdf <- rbind(individualAreaPACdf, newPartRow)
        
        grabAreaPACcount <- grabAreaPACcount + 1
        totalGrabAreaPAC <- totalGrabAreaPAC + PACtime
        break
      }
    }
  
    input_string <- trimedGrabDF[i,10]
    #print(input_string)
    # Find the position of " was picked up"
    pos <- regexpr(" was picked up", input_string)
    #print(pos)
    
    # Extract the substring before the target text
    result <- substr(input_string, 1, pos - 1)
    # Print the result
    if(result == ""){
      pattern <- "Network.*"
      result <- str_extract(input_string, pattern)
      #print(result)
    }
    first_instance <- which(subDF$CurrentGazeTarget == result)[1]

    
    badPartRow <- FALSE
    badTime <- 0
    if(df$Participant[5] == "P3"){
      badTime <- BadParticipantAnalysis(input_string, subDF)
      badPartRow <- badTime$found
      badTime <- badTime$cubetime 
      
      if(badPartRow){
        grabPACcount <- grabPACcount + 1
        totalGrabPAC <- totalGrabPAC + (currentTime - badTime)/10000
        
        PACtype <- "grab"
        PACtime <- (currentTime - badTime)/10000
        
        cubeColor <- ColorCubeAnalysis(currentEvent, PACtime)
        #print(cubeColor)
        PACstartTime <- badTime
        PACendTime <- currentTime
        Event <- input_string
        
        newPartRow <- data.frame(Participant, Condition, Trial, Group, PACtype, PACtime, PACstartTime, PACendTime, Event, cubeColor)
        individualPACdf <- rbind(individualPACdf, newPartRow)
      }
      #print(badTime)
      badPartRow <- FALSE
    }
    
    if (!is.na(first_instance)) {
      first_instance_row <- subDF[first_instance, ]
      if((currentTime - first_instance_row[1,1])/10000 > 0)
      {
        
        grabPACcount <- grabPACcount + 1
        totalGrabPAC <- totalGrabPAC + (currentTime - first_instance_row[1,1])/10000
        
        PACtype <- "grab"
        PACtime <- (currentTime - first_instance_row[1,1])/10000
        
        cubeColor <- ColorCubeAnalysis(currentEvent, PACtime)
        #print(cubeColor)
        PACstartTime <- first_instance_row[1,1]
        PACendTime <- currentTime
        Event <- input_string
  
        #acceleration <- velocity/PACtime
        
        #print(distance)
        #print(acceleration)
        
        newPartRow <- data.frame(Participant, Condition, Trial, Group, PACtype, PACtime, PACstartTime, PACendTime, Event, cubeColor)
        individualPACdf <- rbind(individualPACdf, newPartRow)
        
      }
    }
    
    yMin <- 5
    yMax <- 12.6
    zMin <- -1.1
    zMax <- .1
    subDFGold <- subDF %>% filter(EyePos_Y != "N/A")
    goldCheck <- grepl("Gold", currentEvent)
    if(goldCheck & (trimedGrabDF$Condition[i] == "comp" | trimedGrabDF$Condition[i] == "co" | trimedGrabDF$Condition[i] == "solo")){
      
      
      # first_nonPlay_wall_index <- tail(which(subDFGold$CurrentGazeArea != "play_wall")[1])
      # if(is.na(first_nonPlay_wall_index)){
      # }else{
      #   subDFGold <- subDFGold[1:(first_nonPlay_wall_index-1),]
      # }
      # 
      for (v in nrow(subDFGold):1) {
        if(subDFGold$CurrentGazeArea[v] != "play_wall"){
          subDFGold <- subDFGold[(v+1):nrow(subDFGold),]
          break
        }else{
        }
      }
      
      if(subDFGold$Event[nrow(subDFGold)] == "Network Gold Cube WholeClone313 was picked up"){
        goldDFtest <- subDFGold
        #print("mooooooooooooooooooooo")
      }

      for (g in 1:nrow(subDFGold)){
       # if(as.numeric(subDFGold$EyePos_Y[g]) >= yMin & as.numeric(subDFGold$EyePos_Y[g]) <= yMax & as.numeric(subDFGold$EyePos_Z[g]) >= zMin & as.numeric(subDFGold$EyePos_Z[g]) <= zMax){
       if(as.numeric(subDFGold$EyePos_X[g]) != 1.635337){
         grabPACcount <- grabPACcount + 1
         totalGrabPAC <- totalGrabPAC + (currentTime - subDFGold$Time[g])/10000

         PACtype <- "grab"
         PACtime <- (currentTime - subDFGold$Time[g])/10000
         
         cubeColor <- ColorCubeAnalysis(currentEvent, PACtime)
         
         PACstartTime <- subDFGold$Time[g]
         PACendTime <- currentTime
         Event <- trimedGrabDF$Event[i]
         
         #acceleration <- velocity/PACtime
         
         #print(Event)
         #print(acceleration)
         
         newPartRow <- data.frame(Participant, Condition, Trial, Group, PACtype, PACtime, PACstartTime, PACendTime, Event, cubeColor)
         individualPACdf <- rbind(individualPACdf, newPartRow)
         break
       } 
      }
    }

  }
  
  
  if(nrow(trimedPlaceDF ) != 0){
    for(i in 1:nrow(trimedPlaceDF))
    {
      currentTime <- trimedPlaceDF[i,1]
      currentGazeArea <- trimedPlaceDF[i,12]
      currentEvent <- trimedPlaceDF[i,10]
      
      subDF1 <- df %>% filter(Time > (currentTime-13000000) & Time <= currentTime)
      subDF1 <- subDF1 %>% filter(EyePos_X != "N/A")
      
      Participant <- trimedPlaceDF[2,2]
      Condition <- trimedPlaceDF[8,8]
      Trial <- trimedPlaceDF[9,9]
      Group <-trimedPlaceDF[7,7]
      
      dropZoneLocation <- as.numeric(sub(".*DropzonePlaceHolderClone", "", trimedPlaceDF$Event[i]))

      
      
      for(v in nrow(subDF1):1)
      {
        if(subDF1[v,12] != "build_wall")
        {
          PACtype <- "place"
          PACendTime <- currentTime
          PACstartTime <- subDF1[v,1]
          PACtime <- (PACendTime - PACstartTime)/10000
          
          newPartRow <- data.frame(Participant, Condition, Trial,Group, PACtype, PACtime, PACstartTime, PACendTime, currentEvent)
          individualAreaPACdf <- rbind(individualAreaPACdf, newPartRow)
          
          placeAreaPACcount <- placeAreaPACcount + 1
          totalPlaceAreaPAC <- totalPlaceAreaPAC + PACtime
          break
        }
      }
      
      for(t in 1:nrow(subDF1)){
        
        currentPlaceTime <- subDF1$Time[t]
        currentXPos <- subDF$EyePos_X[t]
        currentYPos <- subDF$EyePos_Z[t]
        
        minX <- dropZoneLocation + 9.4
        maxX <- dropZoneLocation + 10.6
        minY <- 5
        
      
        if(as.numeric(subDF1$EyePos_X[t]) > minX & as.numeric(subDF1$EyePos_X[t]) < maxX & as.numeric(subDF1$EyePos_Y[t]) > minY){
          placePACcount <- placePACcount + 1
          totalPlacePAC <- totalPlacePAC + (currentTime - currentPlaceTime)/10000
          
          PACtype <- "place"
          PACtime <- (currentTime - currentPlaceTime)/10000
          Event <- currentEvent
          
          PACstartTime <- currentPlaceTime
          PACendTime <- currentTime
          cubeColor <- ColorCubeAnalysis(currentEvent, PACtime)

          newPartRow <- data.frame(Participant, Condition, Trial,Group, PACtype, PACtime, PACstartTime, PACendTime, Event, cubeColor)
          individualPACdf <- rbind(individualPACdf, newPartRow)

          break
        }
      }
      
      
      
# 
#       input_string <- trimedPlaceDF[i,10]
# 
#       # Find the position of " was picked up"
#       pos <- regexpr("was placed in dropzone", input_string)
# 
#       # Extract the substring before the target text
#       result <- substr(input_string, 1, pos - 1)
# 
#       # Print the result
# 
# 
#       first_instance <- which(subDF1$CurrentGazeTarget == "GameObject")[1]
# 
#       if (!is.na(first_instance)) {
#         first_instance_row <- subDF1[first_instance, ]
#         if((currentTime - first_instance_row[1,1])/10000 > 0)
#         {
#           placePACcount <- placePACcount + 1
#           totalPlacePAC <- totalPlacePAC + (currentTime - first_instance_row[1,1])/10000
# 
#           PACtype <- "place"
#           PACtime <- (currentTime - first_instance_row[1,1])/10000
# 
# 
#           PACstartTime <- first_instance_row[1,1]
#           PACendTime <- currentTime
# 
#           newPartRow <- data.frame(Participant, Condition, Trial,Group, PACtype, PACtime, PACstartTime, PACendTime, input_string)
#           individualPACdf <- rbind(individualPACdf, newPartRow)
# 
#         }
#       }
      
    }
  }
   avgGrabPAC <- totalGrabPAC/grabPACcount
   avgPlacePAC <- totalPlacePAC/placePACcount
   avgPlaceAreaPAC <- totalPlaceAreaPAC/placeAreaPACcount
   avgGrabAreaPAC <- totalGrabAreaPAC/grabAreaPACcount
   
   avgGoldCubeGrab <- totalGoldCubeGrabTime/totalGoldCubeGrabCount
   avgBlueCubeGrab <- totalBlueCubeGrabTime/totalBlueCubeGrabCount
   avgRedCubeGrab <- totalRedCubeGrabTime/totalRedCubeGrabCount
   avgWhiteCubeGrab <- totalWhiteCubeGrabTime/totalWhiteCubeGrabCount

   
   
   individualGrab2Placedf <- data.frame(SequenceOrder = numeric(),
                                        Participant = factor(),
                                        Condition = factor(),
                                        Trial = numeric(),
                                        Group = factor(),
                                        GrabTime = numeric(),
                                        PlaceTime = numeric(),
                                        GrabEvent = factor(),
                                        PlaceEvent = factor(),
                                        stringsAsFactors = FALSE)
   
   Grab2PlaceAnalysis(trimedGrabDF, trimedPlaceDF)
   grab2PlaceCount <- nrow(individualGrab2Placedf)
   totalGrab2PlaceTime <- 0
   
   for (b in 1:nrow(individualGrab2Placedf)) {
     place <- individualGrab2Placedf$PlaceTime[b]/ 10000
     grab <- individualGrab2Placedf$GrabTime[b]/ 10000
     
     # if((place - grab)> 0){
     #   
     # 
     #  totalGrab2PlaceTime <- totalGrab2PlaceTime + (place - grab)
     # }
   }
   grab2PlaceTime <- totalGrab2PlaceTime/grab2PlaceCount
   
   newPartRow <- data.frame(Participant, Condition, Trial, Group, grabPACcount,placePACcount, avgGrabPAC, avgPlacePAC, grabAreaPACcount, placeAreaPACcount, avgGrabAreaPAC, avgPlaceAreaPAC,
                            avgGoldCubeGrab, totalGoldCubeGrabCount,avgBlueCubeGrab, totalBlueCubeGrabCount, avgRedCubeGrab, totalRedCubeGrabCount, avgWhiteCubeGrab, totalWhiteCubeGrabCount, grab2PlaceTime, grab2PlaceCount)
   
   PACdf <- rbind(PACdf, newPartRow)

}



individualGrab2Placedf <- individualGrab2Placedf  %>% mutate(EventTime = (PlaceTime/ 10000) - (GrabTime/ 10000))


# This seciont below is going to be used to take individual grabs or drops from the PACdataCleaning.R 
# Then seperate out the time into invidual csv's so that they can be exported to Unity for simulation

dfsim <- individualPACdf

dfsim <- dfsim %>% filter(PACtime == 251.7297)
#dfsim <- dfsim %>% filter(PACtime == 373.0018)

# upperTime <- dfsim$PACstartTime[1] - 20000000
# lowerTime <- dfsim$PACendTime[1] + 20000000

upperTime <- dfsim$PACstartTime[1] - 10000000/1.5
lowerTime <- dfsim$PACendTime[1] + 10000000/1.5

dfsimTrim <- df %>% filter(Time >= upperTime & Time <= lowerTime)
dfsimTrim <- dfsimTrim %>% filter(EyePos_X != 0)


dfsimTrim$EyePos_X <- as.numeric(dfsimTrim$EyePos_X)
dfsimTrim$EyePos_Y <- as.numeric(dfsimTrim$EyePos_Y)
dfsimTrim$EyePos_Z <- as.numeric(dfsimTrim$EyePos_Z)

dfsimTrimFront <- dfsimTrim %>% filter(Time <= dfsim$PACstartTime[1])
dfsimTrimDurring <- dfsimTrim %>% filter(Time >= dfsim$PACstartTime[1] & Time <= dfsim$PACendTime[1])
dfsimTrimEnd <- dfsimTrim %>% filter(Time >= dfsim$PACendTime[1])

# dfsimTrim <- dfsimTrim %>% mutate(LRcolor = ifelse(Time >= dfsim$PACstartTime[1] & Time <= dfsim$PACendTime[1], "red","black"))
# 
# dfsimTrim <- dfsimTrim %>% mutate(LRcolor = ifelse( Time > dfsim$PACendTime[1], "blue",LRcolor))
# dfsimTrim <- dfsimTrim %>% mutate(LRcolor = ifelse( Time < dfsim$PACstartTime[1], "green",LRcolor))



# write.csv(dfsimTrimFront, "C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Simulation CSVs/SimFront.csv", row.names = FALSE)
# write.csv(dfsimTrimDurring, "C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Simulation CSVs/SimDurring.csv", row.names = FALSE)
# write.csv(dfsimTrimEnd, "C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Simulation CSVs/SimEnd.csv", row.names = FALSE)
# 
# write.csv(dfsimTrim, "C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Simulation CSVs/Grab Pres1.csv", row.names = FALSE)
write.csv(individualPACdf, output_file, row.names = FALSE)
print(output_file)
# 

