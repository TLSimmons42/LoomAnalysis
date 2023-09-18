library(plot3D)
library(rgl)
library(dplyr)
library(bit64)



data_files <- list.files(pattern = "P12")

PACdf <- data.frame(Time = numeric(),
                   Participant = factor(),
                   Condition = factor(),
                   Trial = numeric(),
                   grabPACcount = numeric(),
                   placePACcount = numeric(),
                   avgGrabPAC = numeric(),
                   avgPlacePAC = numeric(),
                   stringsAsFactors = FALSE)

individualPACdf <- data.frame(Time = numeric(),
                    Participant = factor(),
                    Condition = factor(),
                    Trial = numeric(),
                    PACtype = factor(),
                    PACtime = numeric(),
                    PACstartTime = numeric(),
                    PACendTime = numeric(),
                    stringsAsFactors = FALSE)


for(f in 1:length(data_files))
{
  participantDataFile <- data_files[f]
  print(participantDataFile)
  
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]
  

  # Find all of the Picked events
  trimedGrabDF <- df[grep("picked", df$Event), ]
  trimedGrabDF <- trimedGrabDF %>% filter(CurrentGazeArea == "play_wall")
  trimedGrabDF <- trimedGrabDF %>% filter(!grepl("by", Event))
  
  trimedPlaceDF <- df[grep("placed", df$Event), ]
  trimedPlaceDF <- trimedPlaceDF %>% filter(!grepl("Hit", Event))
  trimedPlaceDF <- trimedPlaceDF %>% filter(!grepl("HB", Event))
  trimedPlaceDF <- trimedPlaceDF[!duplicated(trimedPlaceDF$Event), ]
  
 
  #trimedGrabDF <- trimedGrabDF %>% filter(grepl("Red", Event))
  
  
  grabPACcount <- 0
  placePACcount <- 0
  
  totalGrabPAC <- 0
  totalPlacePAC <- 0
  
  avgGrabPAC <- 0
  avgPlacePAC <- 0
  
  PACstartTime <- 0
  PACendTime <- 0
  
  
  
  for(i in 1:length(trimedGrabDF))
  {
    currentTime <- trimedGrabDF[i,1]
    subDF <- df %>% filter(Time < (10000000+currentTime) & Time > (currentTime-10000000) )
    
    Participant <- trimedGrabDF[2,2]
    Condition <- trimedGrabDF[8,8]
    Trial <- trimedGrabDF[9,9]
    
    
    input_string <- trimedGrabDF[i,10]
    
    # Find the position of " was picked up"
    pos <- regexpr(" was picked up", input_string)
    
    # Extract the substring before the target text
    result <- substr(input_string, 1, pos - 1)
    
    # Print the result


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
        
        newPartRow <- data.frame(Participant, Condition, Trial, PACtype, PACtime, PACstartTime, PACendTime)
        individualPACdf <- rbind(individualPACdf, newPartRow)
        
        
      }
    }

  }
  
  
  for(i in 1:length(trimedPlaceDF))
  {
    currentTime <- trimedPlaceDF[i,1]
    subDF1 <- df %>% filter(Time < (10000000+currentTime) & Time > (currentTime-10000000) )
    
    Participant <- trimedPlaceDF[2,2]
    Condition <- trimedPlaceDF[8,8]
    Trial <- trimedPlaceDF[9,9]
    
    
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
        
        newPartRow <- data.frame(Participant, Condition, Trial, PACtype, PACtime, PACstartTime, PACendTime)
        individualPACdf <- rbind(individualPACdf, newPartRow)
        
        
      }
    }
    
  }
  
   avgGrabPAC <- totalGrabPAC/grabPACcount
   avgPlacePAC <- totalPlacePAC/placePACcount
   
   

   
   newPartRow <- data.frame(Participant, Condition, Trial, grabPACcount,placePACcount, avgGrabPAC, avgPlacePAC)
   
   PACdf <- rbind(PACdf, newPartRow)

}



