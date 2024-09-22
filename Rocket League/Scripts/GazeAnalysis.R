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

gazeTransferTimes <- data.frame(Participant = factor(),
                                Trial = numeric(),
                                Group = factor(),
                                gazeTransfer = factor(),
                                transferTime = numeric(),
                                transferStartTime = numeric(),
                                transferEndTime = numeric(),
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
filteredGazeDurationTimes <- gazeDurationTimes %>% dplyr :: filter(gazeTime > 100)

# avg_Time <- aggregate(gazeTime ~ gazeObject, data = gazeDurationTimes, FUN = mean)
# 
# testDF <- gazeDurationTimes %>%
#   group_by(gazeObject)%>%
#   summarize(
#     MeanPercent = mean(gazeTime),
#     sd = sd(gazeTime),
#     CI_lower = MeanPercent - 1.96 * sd / sqrt(n()),
#     CI_upper = MeanPercent + 1.96 * sd / sqrt(n()))

p <- filteredGazeDurationTimes %>%
  group_by(Group, gazeObject)%>%
  summarize(
    avgGazeTime = mean(gazeTime),
    sd = sd(gazeTime),
    CI_lower = avgGazeTime - 1.96 * sd / sqrt(n()),
    CI_upper = avgGazeTime + 1.96 * sd / sqrt(n())) %>%
  ggplot(aes(reorder(gazeObject,avgGazeTime),avgGazeTime, fill = reorder(Group,avgGazeTime)))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(mapping = aes(ymin = CI_lower, ymax = CI_upper),
                width = .3, position = position_dodge(width = 1))+
  labs(title = "Gaze Duration Time", x = "", y = "Time (ms)")+
  theme_bw()
p
p <- p + guides(fill=guide_legend(title="Group"))
p
ggsave("Gaze Duration Time.pdf")

objectDF <- gazeDurationTimes %>% dplyr:: filter(gazeObject == "Teammate")
result <- t.test(gazeTime ~ Group, data = objectDF, var.equal = FALSE)
print(result)




#finding the gaze duration times
participantList <- unique(gazeDurationTimes$Participant)
for(f in 1:length(participantList)){
  print(participantList[f])
  dfTrimmed <- gazeDurationTimes %>% dplyr:: filter(Participant == participantList[f])
  print(nrow(dfTrimmed))
  
  trialNumbers <- unique(dfTrimmed$Trial)
  for (t in 1:length(trialNumbers)){
    print(length(trialNumbers))
    transferStartTime <- 0
    transferEndTime <- 0
    totalTime <- 0
    
    endObj <- ""
    firstObj <- ""
    looking4New <- TRUE
    frameStart <- 0
    frameEnd <- 0
    
    dfTrimmed2 <- dfTrimmed %>% dplyr:: filter(Trial == trialNumbers[t])
    if(nrow(dfTrimmed) == 0){
      break
    }
    for (i in 1:nrow(dfTrimmed2)){
      if(looking4New){
        firstObj <- dfTrimmed2$gazeObject[i]
        transferStartTime <- dfTrimmed2$gazeEndTime[i]
        frameStart <- dfTrimmed2$frameEnd[i]
        
        looking4New <- FALSE
      }else{
        if(firstObj != dfTrimmed2$gazeObject[i]){
          
          endObj <- dfTrimmed2$gazeObject[i]
          gazeTransfer <- paste(firstObj, endObj, sep = "-")
          transferEndTime<- dfTrimmed2$gazeStartTime[i]
          transferTime <- transferEndTime - transferStartTime
          
          Participant <- dfTrimmed2$Participant[i]
          Trial <- t
          Group <- dfTrimmed2$Group[i]
          frameEnd <- dfTrimmed2$frameStart[i]
          
          newRow <- data.frame(Participant, Trial, Group, gazeTransfer,transferTime, 
                               transferStartTime, transferEndTime, frameStart, frameEnd)
          gazeTransferTimes <<- rbind(gazeTransferTimes, newRow)
          
          
          firstObj <- endObj
          transferStartTime <- dfTrimmed2$gazeEndTime[i]
          frameStart <- dfTrimmed2$frameEnd[i]
          
          
        }else{
          looking4New <- TRUE
        }

      }
      
    }
  }
}


filteredgazeTransferTimes <- gazeTransferTimes %>% dplyr :: filter(transferTime > 20)
filteredgazeTransferTimes <- filteredgazeTransferTimes %>% dplyr :: filter(transferTime < 2000)

filteredgazeTransferTimes <- filteredgazeTransferTimes %>% mutate(gazeTransfer = ifelse(gazeTransfer == "Self-Ball", "Ball-Self", gazeTransfer))
filteredgazeTransferTimes <- filteredgazeTransferTimes %>% mutate(gazeTransfer = ifelse(gazeTransfer == "Goal Post-Ball", "Ball-Goal Post", gazeTransfer))
filteredgazeTransferTimes <- filteredgazeTransferTimes %>% mutate(gazeTransfer = ifelse(gazeTransfer == "Teammate-Ball", "Ball-Teammate", gazeTransfer))
filteredgazeTransferTimes <- filteredgazeTransferTimes %>% mutate(gazeTransfer = ifelse(gazeTransfer == "Opponent-Ball", "Ball-Opponent", gazeTransfer))

filteredgazeTransferTimes <- filteredgazeTransferTimes %>% mutate(gazeTransfer = ifelse(gazeTransfer == "Goal Post-Self", "Self-Goal Post", gazeTransfer))
filteredgazeTransferTimes <- filteredgazeTransferTimes %>% mutate(gazeTransfer = ifelse(gazeTransfer == "Teammate-Self", "Self-Teammate", gazeTransfer))
filteredgazeTransferTimes <- filteredgazeTransferTimes %>% mutate(gazeTransfer = ifelse(gazeTransfer == "Opponent-Self", "Self-Opponent", gazeTransfer))

filteredgazeTransferTimes <- filteredgazeTransferTimes %>% mutate(gazeTransfer = ifelse(gazeTransfer == "Teammate-Goal Post", "Goal Post-Teammate", gazeTransfer))
filteredgazeTransferTimes <- filteredgazeTransferTimes %>% mutate(gazeTransfer = ifelse(gazeTransfer == "Opponent-Goal Post", "Goal Post-Opponent", gazeTransfer))

filteredgazeTransferTimes <- filteredgazeTransferTimes %>%
  filter(!grepl("Boost", filteredgazeTransferTimes$gazeTransfer))

filteredgazeTransferTimes <- filteredgazeTransferTimes %>% dplyr :: filter(gazeTransfer == "Ball-Opponent" | gazeTransfer == "Self-Opponent" |
                                                                             gazeTransfer == "Self-Teammate" |gazeTransfer == "Ball-Goal Post" )


p <- filteredgazeTransferTimes %>%
  group_by(Group, gazeTransfer)%>%
  summarize(
    avgGazeTime = mean(transferTime),
    sd = sd(transferTime),
    CI_lower = avgGazeTime - 1.96 * sd / sqrt(n()),
    CI_upper = avgGazeTime + 1.96 * sd / sqrt(n())) %>%
  ggplot(aes(reorder(gazeTransfer,avgGazeTime),avgGazeTime, fill = reorder(Group,avgGazeTime)))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(mapping = aes(ymin = CI_lower, ymax = CI_upper),
                width = .3, position = position_dodge(width = 1))+
  labs(title = "Gaze Transfer Between Gameobjects", x = "", y = "Time (ms)")+
  theme_bw()
p <- p + guides(fill=guide_legend(title="Group"))
p
ggsave("Object Transfer Time.pdf")


p <- filteredgazeTransferTimes %>%
  group_by(Group)%>%
  summarize(
    avgGazeTime = mean(transferTime),
    sd = sd(transferTime),
    CI_lower = avgGazeTime - 1.96 * sd / sqrt(n()),
    CI_upper = avgGazeTime + 1.96 * sd / sqrt(n())) %>%
  ggplot(aes(Group ,avgGazeTime, fill = Group))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(mapping = aes(ymin = CI_lower, ymax = CI_upper),
                width = .3, position = position_dodge(width = 1))+
  labs(title = "Overall Gaze Transfer Time", x = "", y = "Time (ms)")+
  theme_bw()
p <- p + guides(fill=guide_legend(title="Group"))
p
ggsave("Overall Gaze Transfer Time.pdf")

objectDF <- filteredgazeTransferTimes %>% dplyr:: filter(gazeObject == "Teammate")
result <- t.test(transfert ~ Group, data = filteredgazeTransferTimes, var.equal = FALSE)
print(result)




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

tempDF <- QE.100px
tempDF <- QE.50px
tempDF <- tempDF %>% filter(Duration != 0)

p <- tempDF %>%
  group_by(Group)%>%
  summarize(
    Mean = mean(Onset),
    sd = sd(Onset),
    CI_lower = Mean - 1.96 * sd / sqrt(n()),
    CI_upper = Mean + 1.96 * sd / sqrt(n())) %>%
  ggplot(aes(Group,Mean, fill = Group))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(mapping = aes(ymin = CI_lower, ymax = CI_upper),
                width = .3, position = position_dodge(width = .9))+
  labs(title = "View Wall Gaze Duration Time", x = "", y = "Time (s)")+
  theme_bw()

p <- p + guides(fill=guide_legend(title="Group"))
p




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











