library(plot3D)
library(rgl)
library(dplyr)
library(bit64)
library(stringr)



data_files <- list.files(pattern = "sdP11")

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
  
  

  
  
  
  for(i in 1:nrow(trimedGrabDF))
  {
    currentTime <- trimedGrabDF[i,1]
    currentGazeArea <- trimedGrabDF[i,12]
    currentEvent <- trimedGrabDF[i,10]
    currentHandX <- trimedGrabDF[i,25]
    currentHandY <- trimedGrabDF[i,26]
    currentHandZ <- trimedGrabDF[i,27]
    
    subDF <- df %>% filter(Time > (currentTime-20000000) & Time <= currentTime)
    
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
        ColorCubeAnalysis(currentEvent, PACtime)
        
        
        newPartRow <- data.frame(Participant, Condition, Trial, Group, PACtype, PACtime, PACstartTime, PACendTime, currentEvent)
        individualAreaPACdf <- rbind(individualAreaPACdf, newPartRow)
        
        grabAreaPACcount <- grabAreaPACcount + 1
        totalGrabAreaPAC <- totalGrabAreaPAC + PACtime
        break
      }
    }
  
    input_string <- trimedGrabDF[i,10]
    
    # Find the position of " was picked up"
    pos <- regexpr(" was picked up", input_string)
    
    # Extract the substring before the target text
    result <- substr(input_string, 1, pos - 1)
    
    # Print the result
    #print(result)

    first_instance <- which(subDF$CurrentGazeTarget == result)[1]
    
    
    
    if (!is.na(first_instance)) {
      first_instance_row <- subDF[first_instance, ]
      if((currentTime - first_instance_row[1,1])/10000 > 0)
      {
        
        grabPACcount <- grabPACcount + 1
        totalGrabPAC <- totalGrabPAC + (currentTime - first_instance_row[1,1])/10000
        
        PACtype <- "grab"
        PACtime <- (currentTime - first_instance_row[1,1])/10000
        
        ColorCubeAnalysis(currentEvent, PACtime)
        
        PACstartTime <- first_instance_row[1,1]
        PACendTime <- currentTime
        Event <- input_string
  
        #acceleration <- velocity/PACtime
        
        
        #print(distance)
        #print(acceleration)
        
        newPartRow <- data.frame(Participant, Condition, Trial, Group, PACtype, PACtime, PACstartTime, PACendTime, Event)
        individualPACdf <- rbind(individualPACdf, newPartRow)
        
        
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
      print(dropZoneLocation)
      
      
      
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

          newPartRow <- data.frame(Participant, Condition, Trial,Group, PACtype, PACtime, PACstartTime, PACendTime, Event)
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


ColorCubeAnalysis <- function(colorName, eventDuration){
  #print(colorName)
  result1 <- regexpr("Red", colorName)
  result2 <- regexpr("Blue", colorName)
  result3 <- regexpr("Gold", colorName)
  result4 <- regexpr("Neutral", colorName)
  
  if (result1 != -1) {
    colorName <- "Red"
  } 
  if (result2 != -1) {
    colorName <- "Blue"
  }   
  if (result3 != -1) {
    colorName <- "Gold"
  }   
  if (result4 != -1) {
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

# write.csv(individualPACdf, "PACdf tester 5_13.csv", row.names = FALSE)


individualGrab2Placedf <- individualGrab2Placedf  %>% mutate(EventTime = (PlaceTime/ 10000) - (GrabTime/ 10000))
