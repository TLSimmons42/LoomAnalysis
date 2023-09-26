library(plot3D)
library(rgl)
library(dplyr)
library(bit64)
library(stringr)



data_files <- list.files(pattern = "sdP15.csv")

PACdf <- data.frame(Time = numeric(),
                   Participant = factor(),
                   Condition = factor(),
                   Trial = numeric(),
                   grabPACcount = numeric(),
                   placePACcount = numeric(),
                   avgGrabPAC = numeric(),
                   avgPlacePAC = numeric(),
                   grabAreaPACcount = numeric(),
                   placeAreaPACcount = numeric(),
                   avgGrabAreaPAC = numeric(),
                   avgPlaceAreaPAC = numeric(),
                   stringsAsFactors = FALSE)

PACAreadf <- data.frame(Time = numeric(),
                    Participant = factor(),
                    Condition = factor(),
                    Trial = numeric(),
                    grabAreaPACcount = numeric(),
                    placeAreaPACcount = numeric(),
                    avgGrabAreaPAC = numeric(),
                    avgPlaceAreaPAC = numeric(),
                    stringsAsFactors = FALSE)

individualPACdf <- data.frame(Time = numeric(),
                    Participant = factor(),
                    Condition = factor(),
                    Trial = numeric(),
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
  df$Event <- gsub("\\(|\\)", "", df$Event)
  df$CurrentGazeTarget <- gsub("\\(|\\)", "", df$CurrentGazeTarget)
  gameOverTime <- df[df$Event == "Game Over",]
  df <- df %>% filter(df$Time <= gameOverTime[1,1])
  
  
  


  
  # Find all of the Picked events
  trimedGrabDF <- df[grep("picked", df$Event), ]
  trimedGrabDF <- trimedGrabDF %>% filter(CurrentGazeArea == "play_wall")
  trimedGrabDF <- trimedGrabDF %>% filter(!grepl("by", Event))

  
  trimedPlaceDF <- df[grep("placed", df$Event), ]
  trimedPlaceDF <- trimedPlaceDF %>% filter(!grepl("Hit", Event))
  trimedPlaceDF <- trimedPlaceDF %>% filter(!grepl("HB", Event))
  trimedPlaceDF <- trimedPlaceDF[!duplicated(trimedPlaceDF$Event), ]
  trimedPlaceDF <- trimedPlaceDF %>% filter(CurrentGazeArea == "build_wall")
  
  
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
      
    
      trimedPlaceDF <- trimedPlaceDF %>% filter(!grepl(result, Event))
      
    }
  }
  
  
  
 
  #trimedGrabDF <- trimedGrabDF %>% filter(grepl("Red", Event))
  
  
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
  
  
  for(i in 1:nrow(trimedGrabDF))
  {
    currentTime <- trimedGrabDF[i,1]
    currentGazeArea <- trimedGrabDF[i,12]
    currentEvent <- trimedGrabDF[i,10]
    
    subDF <- df %>% filter(Time > (currentTime-20000000) & Time <= currentTime)
    
    Participant <- trimedGrabDF[2,2]
    Condition <- trimedGrabDF[8,8]
    Trial <- trimedGrabDF[9,9]
    
    
    for(m in nrow(subDF):1)
    {
      if(subDF[m,12] != "play_wall")
      {
        PACtype <- "grab"
        PACendTime <- currentTime
        PACstartTime <- subDF[m,1]
        PACtime <- (PACendTime - PACstartTime)/10000
        
        newPartRow <- data.frame(Participant, Condition, Trial, PACtype, PACtime, PACstartTime, PACendTime, currentEvent)
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
        
        
        PACstartTime <- first_instance_row[1,1]
        PACendTime <- currentTime
        
        newPartRow <- data.frame(Participant, Condition, Trial, PACtype, PACtime, PACstartTime, PACendTime, input_string)
        individualPACdf <- rbind(individualPACdf, newPartRow)
        
        
      }
    }

  }
  
  
  for(i in 1:nrow(trimedPlaceDF))
  {
    currentTime <- trimedPlaceDF[i,1]
    currentGazeArea <- trimedPlaceDF[i,12]
    currentEvent <- trimedPlaceDF[i,10]
    
    subDF1 <- df %>% filter(Time > (currentTime-20000000) & Time <= currentTime)
    
    Participant <- trimedPlaceDF[2,2]
    Condition <- trimedPlaceDF[8,8]
    Trial <- trimedPlaceDF[9,9]
    

    
    for(v in nrow(subDF1):1)
    {
      if(subDF1[v,12] != "build_wall")
      {
        PACtype <- "place"
        PACendTime <- currentTime
        PACstartTime <- subDF1[v,1]
        PACtime <- (PACendTime - PACstartTime)/10000
        
        newPartRow <- data.frame(Participant, Condition, Trial, PACtype, PACtime, PACstartTime, PACendTime, currentEvent)
        individualAreaPACdf <- rbind(individualAreaPACdf, newPartRow)
        
        placeAreaPACcount <- placeAreaPACcount + 1
        totalPlaceAreaPAC <- totalPlaceAreaPAC + PACtime
        break
      }
    }
    
    
    # dfLength <- length(subDF1)
    # areaCheck <- subDF1[dfLength,12]
    # print(areaCheck)
    # while(areaCheck == currentGazeArea)
    # {
    #   dfLength <- dfLength - 1
    #   areaCheck <- subDF1[dfLength,12]
    #   print(dfLength)
    #   print(subDF1[dfLength,10])
    #   #print(areaCheck)
    # }
    # 
    # print("hmmmmmmmmm")
    
    input_string <- trimedPlaceDF[i,10]
    
    # Find the position of " was picked up"
    pos <- regexpr("was placed in dropzone", input_string)
    
    # Extract the substring before the target text
    result <- substr(input_string, 1, pos - 1)
    
    # Print the result
    
    
    first_instance <- which(subDF1$CurrentGazeTarget == "GameObject")[1]
    
    if (!is.na(first_instance)) {
      first_instance_row <- subDF1[first_instance, ]
      if((currentTime - first_instance_row[1,1])/10000 > 0)
      {
        placePACcount <- placePACcount + 1
        totalPlacePAC <- totalPlacePAC + (currentTime - first_instance_row[1,1])/10000
        
        PACtype <- "place"
        PACtime <- (currentTime - first_instance_row[1,1])/10000
        
        
        PACstartTime <- first_instance_row[1,1]
        PACendTime <- currentTime
        
        newPartRow <- data.frame(Participant, Condition, Trial, PACtype, PACtime, PACstartTime, PACendTime, input_string)
        individualPACdf <- rbind(individualPACdf, newPartRow)
        
        
      }
    }
    
  }
  
   avgGrabPAC <- totalGrabPAC/grabPACcount
   avgPlacePAC <- totalPlacePAC/placePACcount
   avgPlaceAreaPAC <- totalPlaceAreaPAC/placeAreaPACcount
   avgGrabAreaPAC <- totalGrabAreaPAC/grabAreaPACcount
   
   
   newPartRow <- data.frame(Participant, Condition, Trial, grabPACcount,placePACcount, avgGrabPAC, avgPlacePAC, grabAreaPACcount, placeAreaPACcount, avgGrabAreaPAC, avgPlaceAreaPAC)
   
   PACdf <- rbind(PACdf, newPartRow)

}



