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

#dataFile <- "singleGazeTransferDF.csv"
dataFile <- "singleGrab2PlaceMTdf.csv"



df <- read.csv(dataFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
df <- read.csv(dataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)


# for(i in 1:nrow(df))
# {
#   #if(df$Group[i] == "1" | df$Group[i] == "f"| is.na(df$Group[i]) | df$Participant[i] == "nuP20"| df$Participant[i] == "nuP17"){
#   if(df$Group[i] == "1" | df$Group[i] == "f"| is.na(df$Group[i])){
#     
#     df$Group[i] <- "e"
#   }
#   if(df$Participant[i] == "sdP13"| df$Participant[i] == "sdP2"){
#     df$Group[i] <- "c"
#     
#   }
# }

dfAut <- df %>% filter(df$Group == "e")
dfNonAut <- df %>% filter(df$Group != "e")
#df <- df %>% filter(df$Condition != "comp")
#-------------------------------------------------------------------------------------------------------------------------------------------------------------


# tempdf <- df %>% filter(!is.na(B2P))
#tempdf <- df %>% filter(P2V > 0)

sd <-sd(df$MovementTime)
tempdf <- df %>% filter(MovementTime < sd*3)
# tempdf <- tempdf %>% filter(Participant != "P22")
#tempdf <- tempdf %>% filter(currentColor == "red")

# 
# tempdf <- tempdf %>% filter(avgPlayerDurration > 100)
# tempdf <- tempdf %>% filter(Condition != "solo")
#tempdf <- tempdf %>% filter(Group != "solo")
#tempdf <- tempdf %>%   mutate(pointColor = ifelse(Group == "e", "orange", "purple"))

standardPlot <- tempdf %>%
  group_by(Condition, Group)%>%
  mutate(pointColor = ifelse(Group == "c", "orange", "purple"))%>%
  summarise(individualPoints = MovementTime,
            pointColor = pointColor,
            mATT = mean(MovementTime), sATT = sd(MovementTime),
            CI_lower = mATT - 1.96 * sATT / sqrt(n()),
            CI_upper = mATT + 1.96 * sATT / sqrt(n()))%>%
  ggplot(aes(Condition,mATT, fill = reorder(Group,-mATT)))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_point(aes(x = Condition, y = individualPoints, color = pointColor),
             position = position_dodge(width = 1.2), size = 3) +
  labs(title = "Partner Gaze Duration",
       x = "", y = "Time (ms)",
       #caption = "moo",
       fill = "")+
  #scale_y_continuous(limits = c(0, 1000))+  # Set y-axis limits
  geom_errorbar(mapping = aes(ymin = CI_lower, ymax = CI_upper),
                width = 0.2, position = position_dodge(width = 0.9), size = 1)+
  theme_pubclean()+scale_fill_startrek()

# standardPlot <- standardPlot + scale_y_continuous(limits = c(0, 1750))
standardPlot
#standardPlot+ theme(legend.position = "none")

#tempdf <- tempdf %>% filter(Group == "c")
tempdf <- tempdf %>% filter(Condition == "co")
t_test_result <- t.test(MovementTime ~ Group, data = tempdf , var.equal = FALSE)
print(t_test_result)


# tempdf <- tempdf %>% filter(Condition == "solo")
# twoANOVA <- aov(tempdf$avgPlayWallDurration ~ factor(tempdf$Group) , data = tempdf)
# summary(twoANOVA)