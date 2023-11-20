library(plot3D)
library(rgl)
library(dplyr)
library(bit64)


areaFile <- "analytics2_P8.csv"
targetFile <- "analytics_P8.csv"
combindedDataFile <- "combindedDataFile P8.csv"
# pupilFile <- "analytics3_P10.csv"
# headFile <- "analytics4_P10.csv"


areaDF <- read.csv(areaFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
targetDF <- read.csv(targetFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
# pupilDF <- read.csv(pupilFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
# headDF <- read.csv(headFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)


areaDF <- areaDF %>% filter(Condition != "tut")
targetDF <- targetDF %>% filter(Condition != "tut")
# pupilDF <- pupilDF %>% filter(Condition != "tut")
# headDF <- headDF %>% filter(Condition != "tut")
# 

areaStartIndexes <- areaDF[areaDF$Event=="Game Start",]
areaEndIndexes <- areaDF[areaDF$Event=="Game Over",]


areaDF <- areaDF %>% filter((TimeStamp >= areaStartIndexes[1,1] & TimeStamp <= areaEndIndexes[1,1]) | 
                              (TimeStamp >= areaStartIndexes[2,1] & TimeStamp <= areaEndIndexes[2,1])| 
                              (TimeStamp >= areaStartIndexes[3,1] & TimeStamp <= areaEndIndexes[3,1])| 
                              (TimeStamp >= areaStartIndexes[4,1] & TimeStamp <= areaEndIndexes[4,1]))

targetDF <- targetDF %>% filter((TimeStamp >= areaStartIndexes[1,1] & TimeStamp <= areaEndIndexes[1,1])| 
                                  (TimeStamp >= areaStartIndexes[2,1] & TimeStamp <= areaEndIndexes[2,1])| 
                                  (TimeStamp >= areaStartIndexes[3,1] & TimeStamp <= areaEndIndexes[3,1])| 
                                  (TimeStamp >= areaStartIndexes[4,1] & TimeStamp <= areaEndIndexes[4,1]))

# pupilDF <- pupilDF %>% filter((TimeStamp >= areaStartIndexes[1,1] & TimeStamp <= areaEndIndexes[1,1]) | 
#                               (TimeStamp >= areaStartIndexes[2,1] & TimeStamp <= areaEndIndexes[2,1])| 
#                               (TimeStamp >= areaStartIndexes[3,1] & TimeStamp <= areaEndIndexes[3,1])| 
#                               (TimeStamp >= areaStartIndexes[4,1] & TimeStamp <= areaEndIndexes[4,1]))
# 
# headDF <- headDF %>% filter((TimeStamp >= areaStartIndexes[1,1] & TimeStamp <= areaEndIndexes[1,1])| 
#                                   (TimeStamp >= areaStartIndexes[2,1] & TimeStamp <= areaEndIndexes[2,1])| 
#                                   (TimeStamp >= areaStartIndexes[3,1] & TimeStamp <= areaEndIndexes[3,1])| 
#                                   (TimeStamp >= areaStartIndexes[4,1] & TimeStamp <= areaEndIndexes[4,1]))
# 


combindedDF <- data.frame(TimeStamp = numeric(),
                         Participant = factor(),
                         Condition = factor(),
                         Trial = numeric(),
                         Age = numeric(),
                         Gender = factor(),
                         Group = factor(),
                         areaEvent = factor(),
                         targetEvent = factor(),
                         xAreaPos = numeric(),
                         yAreaPos = numeric(),
                         zAreaPos = numeric(),
                         xTargetPos = numeric(),
                         yTargetPos = numeric(),
                         zTargetPos = numeric(),
                         stringsAsFactors = FALSE)

currentTargetdfCounter <- 1

for (i in 1:nrow(areaDF)) {
  TimeStamp <- areaDF$TimeStamp[i]
  Participant <- areaDF[i,2]
  Condition <- areaDF[i,3]
  Trial <- areaDF[i,4]
  Age <- areaDF[i,5]
  Gender <- areaDF[i,6]
  Group <- areaDF[i,12]
  areaEvent <- areaDF$Event[i]
  targetEvent <- NA
  xAreaPos <- areaDF$xPos[i]
  yAreaPos <- areaDF$yPos[i]
  zAreaPos <- areaDF$zPos[i]
  xTargetPos <- NA
  yTargetPos <- NA
  zTargetPos <- NA

  if(areaDF$TimeStamp[i]>= targetDF$TimeStamp[currentTargetdfCounter]){
    TimeStamp <- targetDF$TimeStamp[currentTargetdfCounter]
    targetEvent <- targetDF$Event[currentTargetdfCounter]
    xTargetPos <- targetDF$xPos[currentTargetdfCounter]
    yTargetPos <- targetDF$yPos[currentTargetdfCounter]
    zTargetPos <- targetDF$zPos[currentTargetdfCounter]

    if(areaDF[i,1] == targetDF[currentTargetdfCounter,1]){
      print("wow they are equal")
      newPartRow <- data.frame(TimeStamp, Participant, Condition, Trial, Age, Gender, Group,
                               areaEvent, targetEvent, xAreaPos, yAreaPos, zAreaPos,
                               xTargetPos,yTargetPos, zTargetPos)
      combindedDF <- rbind(combindedDF, newPartRow)
      currentTargetdfCounter <- currentTargetdfCounter + 1

    }else{
      while (areaDF[i,1] > targetDF[currentTargetdfCounter,1]) {
        Participant <- targetDF$participant[currentTargetdfCounter]
        Condition <- targetDF$Condition[currentTargetdfCounter]
        Trial <- targetDF$Tiral[currentTargetdfCounter]
        areaEvent <- NA
        targetEvent <- targetDF$Event[currentTargetdfCounter]
        xAreaPos <- NA
        yAreaPos <- NA
        zAreaPos <- NA


        newPartRow <- data.frame(TimeStamp, Participant, Condition, Trial, Age, Gender, Group,
                                 areaEvent, targetEvent, xAreaPos, yAreaPos, zAreaPos,
                                 xTargetPos,yTargetPos, zTargetPos)
        combindedDF <- rbind(combindedDF, newPartRow)
        currentTargetdfCounter <- currentTargetdfCounter + 1
        print(currentTargetdfCounter)
      }
    }
  }else{
    newPartRow <- data.frame(TimeStamp, Participant, Condition, Trial, Age, Gender, Group,
                             areaEvent, targetEvent, xAreaPos, yAreaPos, zAreaPos,
                             xTargetPos,yTargetPos, zTargetPos)
    combindedDF <- rbind(combindedDF, newPartRow)
  }




}


write.csv(combindedDF, combindedDataFile, row.names = FALSE)



