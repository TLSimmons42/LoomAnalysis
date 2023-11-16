library(plot3D)
library(rgl)
library(dplyr)
library(bit64)


data_files <- list.files(pattern = "analytics")
data_files[]

bob <- length(data_files)
bob

combindedDF <- data.frame(TimeStamp = numeric(),
                         Participant = factor(),
                         Condition = factor(),
                         Trial = numeric(),
                         Age = numeric(),
                         Gender = factor(),
                         SessionTime = numeric(),
                         areaEvent = factor(),
                         targetEvent = factor(),
                         xAreaPos = numeric(),
                         yAreaPos = numeric(),
                         zAreaPos = numeric(),
                         xTargetPos = numeric(),
                         yTargetPos = numeric(),
                         zTargetPos = numeric(),
                         group = factor(),
                         stringsAsFactors = FALSE)


areaFile <- "analytics2_P10.csv"
targetFile <- "analytics_P10.csv"


areaDF <- read.csv(areaFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
targetDF <- read.csv(targetFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)


areaDF <- areaDF %>% filter(Condition != "tut")
targetDF <- targetDF %>% filter(Condition != "tut")


areaStartIndexes <- areaDF[areaDF$Event=="Game Start",]
areaEndIndexes <- areaDF[areaDF$Event=="Game Over",]

targetStartIndexes <- targetDF[targetDF$Event=="Game Start",]
targetEndIndexes <- targetDF[targetDF$Event=="Game Over",]





