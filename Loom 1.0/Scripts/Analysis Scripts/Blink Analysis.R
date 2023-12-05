library(plot3D)
library(rgl)
library(dplyr)
library(bit64)


#dataFile <- "singleGazeTransferDF.csv"


data_files <- list.files(pattern = "analytics3")
data_files[]


dataFile <- "totalCombinedDF.csv"
combinedDF <- read.csv(dataFile, colClasses=c("TimeStamp" = "integer64", "grabTime" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)


singleBlinkDF <- data.frame(Participant = factor(),
                             Condition = factor(),
                             Trial = numeric(),
                             Age = numeric(),
                             Gender = factor(),
                             Group = factor(),
                             BlinkTime = numeric(),
                             BlinkStartTime = numeric(),
                             BlinkEndTime = numeric(),
                             blinkRowCounter = numeric(),
                             stringsAsFactors = FALSE)

BlinRatekDF <- data.frame(Participant = factor(),
                            Condition = factor(),
                            Trial = numeric(),
                            Age = numeric(),
                            Gender = factor(),
                            Group = factor(),
                            BlinkRate = numeric(),
                            BlinkCount = numeric(),
                            TotalTime = numeric(),
                            stringsAsFactors = FALSE)





for (j in 1:length(data_files)){
  participantDataFile <- data_files[j]
  print(participantDataFile)
  pupilDF <- read.csv(participantDataFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  individualCombinedData <- combinedDF %>% filter(Participant == pupilDF$participant[5])
  print(pupilDF$participant[5])
  
  # 
  # StartIndexes <- individualCombinedData %>% filter(areaEvent == "Game Start")
  # EndIndexes <- individualCombinedData %>% filter(targetEvent == "Game Over")
  # 
  
  blinkCounter <- 0
  for(k in 1:4){
    if(k == 1){
      tempDF <- individualCombinedData %>% filter(Condition == "s" & Trial == 1)
      print(nrow(tempDF))
    }else if(k == 2){
      tempDF <- individualCombinedData %>% filter(Condition == "s" & Trial == 2)
      print(nrow(tempDF))
    }else if(k == 3){
      tempDF <- individualCombinedData %>% filter(Condition == "co" & Trial == 1)
      print(nrow(tempDF))
    }else if(k == 4){
      tempDF <- individualCombinedData %>% filter(Condition == "co" & Trial == 2)
      print(nrow(tempDF))
    }
    
    if(k == 4 & nrow(tempDF) == 0){
      print("break")
      break
    }

    
    blinkCounter <- 0

    totalTime <- (tempDF$TimeStamp[nrow(tempDF)] - tempDF$TimeStamp[1])/10000000
    print(totalTime)
    trialDF <- pupilDF %>% filter(TimeStamp >= tempDF$TimeStamp[1] & TimeStamp <= tempDF$TimeStamp[nrow(tempDF)])
    trialDF <- trialDF %>% filter(!is.na(leftEye))
    
    if(nrow(trialDF) == 0){
      print("next")
      next
    }
   

      
    
    Participant <- trialDF$participant[5]
    Condition <- trialDF$Condition[5]
    Trial <- trialDF$Tiral[5]
    Age <- trialDF$Age[5]
    Gender <- trialDF$Gender[5]
    Group <- individualCombinedData$Group[5]

    looking4Blink <- TRUE
    blinkRowCounter <- 0
    BlinkStartTime <- 0
    
    
    for(i in 1:nrow(trialDF)){
      currentPupilSize <- trialDF$leftEye[i]
      currentTime <- trialDF$TimeStamp[i]
      
      if(!looking4Blink){
        if(currentPupilSize == (-1)){
          blinkRowCounter <- blinkRowCounter + 1 
        }else{
          if(blinkRowCounter > 1){
            BlinkEndTime <- currentTime
            BlinkTime <- (BlinkEndTime - BlinkStartTime)/10000
            
            newPartRow <- data.frame(Participant, Condition, Trial, Age, Gender, Group,
                                     BlinkTime, BlinkStartTime, BlinkEndTime, blinkRowCounter)
            singleBlinkDF <- rbind(singleBlinkDF, newPartRow)
            
            if(BlinkTime >= 70 & BlinkTime <= 300){
              blinkCounter <- blinkCounter + 1
            }
          }
          looking4Blink <- TRUE
          blinkRowCounter <- 0
        }
      }
      
      if(looking4Blink){
        if(currentPupilSize == (-1)){
          looking4Blink <- FALSE
          BlinkStartTime <- currentTime
          
        }
      }
    }
    BlinkCount <- blinkCounter
    TotalTime <- totalTime
    BlinkRate <- BlinkCount/TotalTime
    
    newPartRow <- data.frame(Participant, Condition, Trial, Age, Gender, Group,
                             BlinkRate, BlinkCount, TotalTime)
    BlinRatekDF <- rbind(BlinRatekDF, newPartRow)
  }

  
}

# df <- read.csv(DataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
# 
# df <- df %>% filter(BlinkTime >= 100 & BlinkTime <= 400)
# 
# df <- df %>% mutate(Group = ifelse(Group == "e", "Aut",
#                                    ifelse(Group == "c", "Non-Aut", "nothing")))

# Specify the value you want to count
# value_to_count <- "P8"
# 
# # Count the number of rows with the specified value in the 'Value' column
# count <- sum(df$Participant == value_to_count)
# count

# blinkDF <- df  %>%
#   group_by(Participant, Group, Condition, Trial) %>%
#   summarize(BlinkCount = sum(df$Participant == Participant & df$Condition == Condition & df$Trial == Trial))
# 
# blinkDF <- blinkDF %>% filter(Participant != "P9")




# singleBlinkDF <- singleBlinkDF %>% mutate(Group = ifelse(Participant == "P4", "e", Group))
# 
# checkDF <- singleBlinkDF %>% filter(Condition == "co")
# write.csv(singleBlinkDF, "singleBlinkDF.csv", row.names = FALSE)
write.csv(BlinRatekDF, "blinkRateDF2.csv", row.names = FALSE)

