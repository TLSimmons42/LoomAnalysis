library(plot3D)
library(rgl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(ggpubr)
library(ez)
library(cowplot)
library(ggsci)
library(gridExtra)
library(ggsignif)
library(bit64)


data_files <- list.files(pattern = "P14")

participantDataFile <- data_files[1]
print(participantDataFile)


df <- read.csv(participantDataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)
#df <- df[!duplicated(df$Time), ]

dfTrim <- df %>% filter(GazePoint_X > 0)
dfTrim <- dfTrim %>% filter(GazePoint_X <= 1920)

dfTrim <- dfTrim %>% filter(GazePoint_Y > 0)
dfTrim <- dfTrim %>% filter(GazePoint_Y <= 1080)


dfSmall <- df %>% filter(GazePoint_Y < 500)
dfSmall <- dfSmall %>% filter(GazePoint_X < 600)
dfSmall <- dfSmall %>% filter(GazePoint_Y >= 0)
#dfSmall <- dfSmall %>% filter(Frame < 2254 & Frame > 2259)

plot(dfSmall$GazePoint_X, dfSmall$GazePoint_Y)

dfTrim <- df %>% filter(Frame < 47115 & Frame > 47100)
dfHold <- dfTrim

# write.csv(dfTrim, "FilterGaze 9-17-24.csv", row.names = FALSE)

dfTrim <- df
# dfTrim <- dfTrim %>% filter(!(row_number() %in% 2996:2998))
# dfTrim <- dfTrim %>% filter(GazePoint_X < GazePoint_X[2998] & GazePoint_X > GazePoint_X[2998 + 1])

# dfTrim <- dfTrim %>% filter(Frame < 3000 | Frame > 3000+zeroThreshold)

lookingForZero = TRUE
startIndex <- 0
endIndex <- 0
startFRame <- 0
endFrame <- 0
zeroCounter <- 0
zeroThreshold <- 1
for (i in 1:nrow(dfTrim)) {
  # print(i)
  # print(dfTrim$GazePoint_X[i])
  if(!is.na(dfTrim$GazePoint_X[i])){
    if(!lookingForZero){
      if(dfTrim$GazePoint_X[i] == 0){
        zeroCounter <- zeroCounter +1
      }else{
        lookingForZero <- TRUE
        endIndex <- i
        endFrame <- dfTrim$Frame[i]
        print(dfTrim$GazePoint_X[startIndex -1])
        #dfTrim <- dfTrim %>% filter(!(row_number() %in% endIndex:(endIndex + zeroThreshold)))
        dfTrim <- dfTrim %>% filter(Frame > startFrame | Frame < startFrame-zeroThreshold)
        dfTrim <- dfTrim %>% filter(Frame < endFrame | Frame > endFrame+zeroThreshold)
        
        # print(zeroCounter)
        # print("this is the end of the zeros")
        # print(i)
        zeroCounter <- 0
        
        
        next
      }
    }
    
    
    if(lookingForZero){
      if(dfTrim$GazePoint_X[i] == 0){
        # print("this is the start of zeros")
        # print(i)
        startIndex <- i
        startFrame <- dfTrim$Frame[i]
        #dfTrim <- dfTrim %>% filter(!(row_number() %in% (startIndex - zeroThreshold):startIndex))
        lookingForZero = FALSE
      }
    }
  }

}


dfTrim <- dfTrim %>% filter(GazePoint_X > 0)
dfTrim <- dfTrim %>% filter(GazePoint_X <= 1920)

dfTrim <- dfTrim %>% filter(GazePoint_Y > 0)
dfTrim <- dfTrim %>% filter(GazePoint_Y <= 1080)

dfSmall <- dfTrim %>% filter(GazePoint_Y < 500)
dfSmall <- dfSmall %>% filter(GazePoint_X < 600)


plot(dfTrim$GazePoint_X, dfTrim$GazePoint_Y)
