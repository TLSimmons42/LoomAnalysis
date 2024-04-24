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
library(signal)


data_files <- list.files(pattern = "merge")


gazeDurationTimes <- data.frame(Participant = factor(),
                        Trial = numeric(),
                        Group = factor(),
                        gazeObject = factor(),
                        gazeTime = numeric(),
                        gazeStartTime = numeric(),
                        gazeEndTime = numeric(),
                        frameStart = numeric(),
                        frameEnd = numeric(),
                        stringsAsFactors = FALSE)

participantDataFile <- data_files[1]
print(participantDataFile)


df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
#df <- df[!duplicated(df$Time), ]

df <- df %>% mutate(Timestamp = Timestamp*1000)
df <- dplyr:: filter(df, df$GazeInsideBox != "") 
# df <- df %>% mutate(GazeInsideBox = ifelse(GazeInsideBox != "False" & GazeInsideBox != "True", "False", "moop"))

#finding the gaze duration times

participantList <- unique(df$participant)
for(f in 1:length(participantList)){
  print(participantList[f])
  dfTrimmed <- df %>% dplyr:: filter(participant == participantList[f])
  print(nrow(dfTrimmed))
  
  trialNumbers <- unique(dfTrimmed$Trial_Num)
  for (t in 1:length(trialNumbers)){
    print(length(trialNumbers))
    startTime <- 0
    endTime <- 0
    totalTime <- 0
    
    currentObj <- ""
    frameCheck <- 0
    looking4New <- TRUE
    frameStart <- 0
    frameEnd <- 0
    
    dfTrimmed2 <- dfTrimmed %>% dplyr:: filter(Trial_Num == trialNumbers[t])
    if(nrow(dfTrimmed) == 0){
      break
    }
    for (i in 1:nrow(dfTrimmed2)){
      if(looking4New){
        if(dfTrimmed2$GazeInsideBox[i] == "True"){
          looking4New <- FALSE
          currentObj <- dfTrimmed2$ClassName[i]
          frameCheck <- dfTrimmed2$Frame[i] + 1 
          startTime <- dfTrimmed2$Timestamp[i]
          frameStart <- dfTrimmed2$Frame[i]
        }
      }else
        if(dfTrimmed2$Frame[i] == frameCheck){
          if(currentObj == dfTrimmed2$ClassName[i] & dfTrimmed2$GazeInsideBox[i] == "True"){
            frameCheck <- dfTrimmed2$Frame[i] + 1 
          }
        }else if(dfTrimmed2$Frame[i] > frameCheck & dfTrimmed2$Frame[i] < frameCheck +20){
          endTime <- dfTrimmed2$Timestamp[i]
          looking4New <- TRUE
          
          Participant <- dfTrimmed2$participant[i]
          # Participant <- "moo"
          Trial <- t
          Group <- dfTrimmed2$Experience[i]
          # Group <- "ooop"
          gazeObject <- currentObj
          gazeTime <- endTime - startTime
          gazeStartTime <- startTime
          gazeEndTime <- endTime
          frameEnd <- dfTrimmed2$Frame[i]
          
          
          newRow <- data.frame(Participant, Trial, Group, gazeObject,gazeTime, gazeStartTime, gazeEndTime, frameStart, frameEnd)
          gazeDurationTimes <<- rbind(gazeDurationTimes, newRow)
          
        }
    }
  }
  
  
  
}



# gazeDurationTimes <- gazeDurationTimes %>% dplyr :: filter(gazeTime < 2000)
gazeDurationTimes <- gazeDurationTimes %>% dplyr :: filter(gazeTime > 100)

avg_Time <- aggregate(gazeTime ~ gazeObject, data = gazeDurationTimes, FUN = mean)

p <- gazeDurationTimes %>%
  group_by(gazeObject)%>%
  summarize(
    MeanPercent = mean(gazeTime),
    sd = sd(gazeTime),
    CI_lower = MeanPercent - 1.96 * sd / sqrt(n()),
    CI_upper = MeanPercent + 1.96 * sd / sqrt(n())) %>%
  ggplot(aes(gazeObject, gazeTime, fill = gazeObject))+
  geom_bar(stat = "identity")+
  labs(title = "Build Wall", x = "", y = "")+
  theme_bw()
p

# p <- gazeDurationTimes %>%
#   group_by(gazeObject)%>%
#   ggplot(aes(gazeObject, gazeTime, fill = gazeObject))+
#   geom_bar(stat = "identity")+
#   labs(title = "Build Wall", x = "", y = "")+
#   theme_bw()
# p




# avg_Time <- aggregate(gazeTime ~ gazeObject + Trial, data = gazeDurationTimes, FUN = mean)
# 
# p <- avg_Time %>%
#   group_by(gazeObject, Trial)%>%
#   #summarise(mATT = mean(P2B), sATT = sd(P2B))%>%
#   ggplot(aes(gazeObject, gazeTime, fill = Trial))+
#   geom_bar(stat = "identity", position = "dodge")+
#   # geom_boxplot(outlier.shape = NA) +
#   labs(title = "", x = "", y = "")+
#   #theme(legend.position = "none")+
#   theme_bw()
# p





#__________________________________________________________________________________________________________



# p <- gazeDurationTimes %>%
#   group_by(gazeObject, Trial)%>%
#   #summarise(mATT = mean(P2B), sATT = sd(P2B))%>%
#   ggplot(aes(gazeObject,gazeTime, fill = reorder(Trial,gazeTime)))+
#   geom_boxplot() +
#   # geom_boxplot(outlier.shape = NA) +
#   labs(title = "Build Wall", x = "", y = "")+
#   #theme(legend.position = "none")+
#   theme_bw()
# p
# 
# 
# p <- gazeDurationTimes %>%
#   group_by(gazeObject)%>%
#   #summarise(mATT = mean(P2B), sATT = sd(P2B))%>%
#   ggplot(aes(gazeObject,gazeTime, fill = gazeObject))+
#   geom_boxplot() +
#   # geom_boxplot(outlier.shape = NA) +
#   labs(title = "Build Wall", x = "", y = "")+
#   #theme(legend.position = "none")+
#   theme_bw()
# 
# 
# p <- p + scale_y_continuous(limits = c(90, 200))
# p
# 
# 
# p <- avg_Time %>%
#   #summarise(mATT = mean(P2B), sATT = sd(P2B))%>%
#   ggplot(aes(gazeObject,gazeTime))+
#   geom_bar(stat = "summary", fun.y = "mean", fill = "skyblue")+
#   
#   # geom_boxplot(outlier.shape = NA) +
#   labs(title = "Build Wall", x = "", y = "")+
#   #theme(legend.position = "none")+
#   theme_bw()
# 
# 
# p











