library(plot3D)
library(rgl)
#library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(ggsci)
library(ggpubr)


#dataFile <- "AllSubjectGazeData2-25-23_FullTime_Trimmed.csv"
#dataFile <- "AllSubjectGazeData2-25-23_FullTime.csv"
#dataFile <- "pacMoving_2-25-23_FullTime.csv"
#dataFile <- "pacMoving_2-25-23_FullTime_Trimmed.csv"
dataFile <- "AllSubjectGazeData2-25-23_OneMin.csv"
#dataFile <- "AllSubjectGazeData2-25-23_OneMin_Trimmed.csv"
#dataFile <- "pacMoving_2-25-23_OneMin.csv"
#dataFile <- "pacMoving_2-25-23_OneMin_Trimmed.csv"


df <- read.csv(dataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)

df$group[df$group == "e"] <- 'AUT'
df$group[df$group == "c"] <- 'Non-AUT'

df$condition[df$condition == "s"] <- 'Solo'
df$condition[df$condition == "co"] <- 'Cooperative'

soloDF <- df[df$condition == "Solo",]
coDF <- df[df$condition == "Cooperative",]

cDF <- df[df$group == "AUT",]
eDF <- df[df$group == "Non-AUT",]

CsoloDF <- df[df$condition == "Solo" & df$group == "Non-AUT",]
EsoloDF <- df[df$condition == "Solo" & df$group == "AUT",]

CcoDF <- df[df$condition == "Cooperative" & df$group == "Non-AUT",]
EcoDF <- df[df$condition == "Cooperative" & df$group == "AUT",]


# #
# df$group[df$partGroup == "e"] <- 'AUT'
# df$group[df$partGroup == "c"] <- 'Non-AUT'
# 
# df$condition[df$condition == "s"] <- 'Solo'
# df$condition[df$condition == "co"] <- 'Cooperative'
# 
# soloDF <- df[df$condition == "s",]
# coDF <- df[df$condition == "co",]
# 
# cDF <- df[df$group == "c",]
# eDF <- df[df$group == "e",]




#-------------------------------------------------------------------------------------------------------------------------------
#View wall analysis
use <- df$pacMove
sd(use)
boxplot(use)

#-------------------------------------------------------------------------------------------------------------------------------
#avg total Gaze Transfer Time analysis
gazePlot <- df%>%
  group_by(condition, group)%>%
  summarise(mATT = mean(avgTotalTransferTime), sATT = sd(avgTotalTransferTime))%>%
  ggplot(aes(reorder(condition,mATT),mATT, fill = reorder(group,mATT)))+
  geom_bar(stat = "identity", position = "dodge")+
  #geom_text(mapping=aes(label=round(mATT,2)), position = position_dodge(width = 0.9),
  #        cex= 2.5, vjust=-2)+
  labs(title = "PAC Cube Placement Sequence",
       subtitle = "",
       x = "Trial Condition", y = "Time (ms)",
       #caption = "moo",
       fill = "")+
  geom_errorbar(mapping = aes(ymin = mATT-sATT, ymax = mATT + sATT),
                width = 0.2, position = position_dodge(width = 0.9))+
  theme_pubclean()+scale_fill_startrek() 

gazePlot

#------------------------------------------------------------------------------------------

gazePlot <- df%>%
  group_by(condition, group)%>%
  summarise(mATT = mean(avgPlay2View), sATT = sd(avgPlay2View))%>%
  ggplot(aes(reorder(condition,mATT),mATT, fill = reorder(group,mATT)))+
  geom_bar(stat = "identity", position = "dodge")+
  #geom_text(mapping=aes(label=round(mATT,2)), position = position_dodge(width = 0.9),
  #          cex= 2.5, vjust=-2)+
  labs(title = "",
       #subtitle = "kitty",
       x = "", y = "",
       #caption = "moo",
       fill = "")+
  geom_errorbar(mapping = aes(ymin = mATT-sATT, ymax = mATT + sATT),
                width = 0.2, position = position_dodge(width = 0.9))+
  theme_pubclean()+scale_fill_startrek() 


gazePlot

#-------------------------------------------------------------------------------------------------------------------------------
#TESTING CODE
gazePlot <- df%>%
  group_by(condition, group)%>%
  summarise(mATT = mean(pacStay), sATT = sd(pacStay))%>%
  ggplot(aes(reorder(condition,mATT),mATT, fill = reorder(group,mATT)))+
  geom_bar(stat = "identity", position = "dodge")+
  #geom_text(mapping=aes(label=round(mATT,2)), position = position_dodge(width = 0.9),
  #        cex= 2.5, vjust=-2)+
  labs(title = "PAC Cube Placement Sequence",
       subtitle = "",
       x = "Trial Condition", y = "Time (ms)",
       #caption = "moo",
       fill = "")+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  theme_pubclean()+scale_fill_startrek() 

gazePlot