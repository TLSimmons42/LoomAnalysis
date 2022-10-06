library(plot3D)
library(rgl)
library(dplyr)


participantDataFile <- "analytics3_P26.csv"
originalDF <- read.csv(participantDataFile, header = TRUE, sep = ",")
#df <- originalDF[originalDF$Condition != "tut",]

startIndexes <- originalDF[originalDF$Event=="Game Start",]
endIndexes <- originalDF[originalDF$Event=="Game Over",]

startIndexes <- startIndexes[startIndexes$Condition == "s" | startIndexes$Condition == "co", ]
endIndexes <- endIndexes[endIndexes$Condition == "s" | endIndexes$Condition == "co", ]



originalDF <- originalDF %>% slice(strtoi(rownames(startIndexes[1,])):strtoi(rownames(endIndexes[1,])), strtoi(rownames(startIndexes[2,])):strtoi(rownames(endIndexes[2,])), strtoi(rownames(startIndexes[3,])):strtoi(rownames(endIndexes[3,])), strtoi(rownames(startIndexes[4,])):strtoi(rownames(endIndexes[4,])))
#originalDF <- originalDF[originalDF$rightEye ,]

sDF <- originalDF[originalDF$Condition == "s" & originalDF$Event =="RightPupil"& (originalDF$rightEye != -1 |originalDF$leftEye != -1),]
coDF <- originalDF[originalDF$Condition == "co"& originalDF$Event =="RightPupil" & (originalDF$rightEye != -1 |originalDF$leftEye != -1),]



meanLeftEyeSolo <- mean(sDF$leftEye)
meanRightEyeSolo <- mean(sDF$rightEye)

meanLeftEyeCo <- mean(coDF$leftEye)
meanRightEyeCo <- mean(coDF$rightEye)

avgSolo <- (meanLeftEyeSolo + meanRightEyeSolo)/2
avgcolo <- (meanLeftEyeCo + meanRightEyeCo)/2