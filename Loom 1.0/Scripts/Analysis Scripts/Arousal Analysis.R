library(plot3D)
library(rgl)
library(dplyr)
library(bit64)


#dataFile <- "singleGazeTransferDF.csv"
dataFile <- "grab2PlaceTimes.csv"

data_files <- list.files(pattern = "analytics3")
data_files[]



grab2PlaceDF <- read.csv(dataFile, colClasses=c("TimeStamp" = "integer64", "grabTime" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
# df <- read.csv(dataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)


# sd <-sd(df$MovementTime)
# df <- df %>% filter(MovementTime < sd*3)
newArousalDF <- data.frame()

for (j in 1:length(data_files)) {
  participantDataFile <- data_files[j]
  print(participantDataFile)
  pupilDF <- read.csv(participantDataFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  arousalDF <- pupilDF %>% filter(leftEye != (-1))
  arousalDF <- arousalDF %>% filter(Event == "RightPupil")
  sd <-sd(arousalDF$leftEye)
  upperLimit <- mean(arousalDF$leftEye + (sd*3))
  lowerLimit <- mean(arousalDF$leftEye - (sd*3))
  
  arousalDF <- arousalDF %>% filter(leftEye > lowerLimit & leftEye < upperLimit)
  
  currentParticipant <- pupilDF$participant[j]
  tempGrabDF <- grab2PlaceDF %>% filter(Participant == currentParticipant)  
  
  for(i in 1:nrow(tempGrabDF)){
    # baselineDF <- arousalDF %>% filter(TimeStamp <= tempGrabDF$grabTime[i] & TimeStamp >= tempGrabDF$grabTime[i] - 20000000)
    # shortDF <- arousalDF %>% filter(TimeStamp >= tempGrabDF$grabTime[i] & TimeStamp <= tempGrabDF$TimeStamp[i])
    
    baselineDF <- arousalDF %>% filter(TimeStamp >= tempGrabDF$grabTime[i] & TimeStamp <= tempGrabDF$TimeStamp[i]+ 20000000)
    shortDF <- arousalDF %>% filter(TimeStamp >= tempGrabDF$grabTime[i] & TimeStamp <= tempGrabDF$TimeStamp[i] + 20000000)
    
    currentTime <- shortDF$TimeStamp
    baseline <- mean(baselineDF$leftEye)
    totalTime <- shortDF$TimeStamp[nrow(shortDF)] - shortDF$TimeStamp[1]
    
    shortDF <- shortDF %>%
      mutate(PercentChange = (leftEye - baseline) / baseline * 100)
    
    
    shortDF <- shortDF  %>%
      mutate(TimeEpoch = round(((totalTime -(shortDF$TimeStamp[nrow(shortDF)] - TimeStamp))/totalTime)/.05) * .05)
      # mutate(TimeEpoch = round((totalTime -(shortDF$TimeStamp[nrow(shortDF)] - TimeStamp))/totalTime,1))
    
    
    newArousalDF <- rbind(newArousalDF, shortDF)
    
    


    
  }
}





# write.csv(newArousalDF, "arousalGrab2PlaceDF2diffBasline.csv", row.names = FALSE)
