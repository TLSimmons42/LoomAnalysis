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



#dataFile <- "singleGazeTransferDF.csv"
dataFile <- "singleBlinkDF.csv"
#dataFile <- "singleGazeDurationDF.csv"

df <- read.csv(dataFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)

df <- df %>% mutate(Group = ifelse(Group == "e", "Aut",
                                   ifelse(Group == "c", "Non-Aut", "whatever")))

df <- df %>% mutate(Condition = ifelse(Condition == "s","Solo",
                                       ifelse(Condition == "co", "Cooperative", "whatever")))                                      

df <- df %>% mutate(Group = ifelse(Group == "whatever","Non-Aut", Group))


# df <- read.csv(dataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)


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

tempdf <- df %>% filter(!is.na(BlinkCount))
sd <-sd(tempdf$BlinkCount)
upperLimit <- mean(tempdf$BlinkCount + (sd*3))
lowerLimit <- mean(tempdf$BlinkCount - (sd*3))



tempdf <- tempdf %>% filter(BlinkCount > lowerLimit & BlinkCount < upperLimit)


# tempdf <- tempdf %>% filter(Participant != "P22")
#tempdf <- tempdf %>% filter(currentColor == "red")

# 
# tempdf <- tempdf %>% filter(avgPlayerDurration > 100)
# tempdf <- tempdf %>% filter(Condition != "solo")
#tempdf <- tempdf %>% filter(Group != "solo")
#tempdf <- tempdf %>%   mutate(pointColor = ifelse(Group == "e", "orange", "purple"))

standardPlot <- tempdf %>%
  group_by(Condition, Group)%>%
  mutate(pointColor = ifelse(Group == "Aut", "orange", "purple"))%>%
  summarise(individualPoints = BlinkCount,
            pointColor = pointColor,
            mATT = mean(BlinkCount), sATT = sd(BlinkCount),
            CI_lower = mATT - 1.96 * sATT / sqrt(n()),
            CI_upper = mATT + 1.96 * sATT / sqrt(n()))%>%
  ggplot(aes(Condition,mATT, fill = reorder(Group,mATT)))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_point(aes(x = Condition, y = individualPoints, color = pointColor),
             position = position_dodge(width = 1.2), size = 3, show.legend = FALSE) +
  labs(title = "Gaze Transfer: Build to Play",
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
tempdf <- tempdf %>% filter(Condition == "s")
t_test_result <- t.test(BlinkCount ~ Group, data = tempdf , var.equal = FALSE)
print(t_test_result)


# tempdf <- tempdf %>% filter(Condition == "solo")
# twoANOVA <- aov(tempdf$avgPlayWallDurration ~ factor(tempdf$Group) , data = tempdf)
# summary(twoANOVA)





tempdf <- df %>% filter(!is.na(BlinkCount))
sd <-sd(tempdf$BlinkCount)
upperLimit <- mean(tempdf$BlinkCount + (sd*3))
lowerLimit <- mean(tempdf$BlinkCount - (sd*3))



tempdf <- tempdf %>% filter(BlinkCount > lowerLimit & BlinkCount < upperLimit)


# tempdf <- tempdf %>% filter(Participant != "P22")
#tempdf <- tempdf %>% filter(currentColor == "red")

# 
# tempdf <- tempdf %>% filter(avgPlayerDurration > 100)
# tempdf <- tempdf %>% filter(Condition != "solo")
#tempdf <- tempdf %>% filter(Group != "solo")
#tempdf <- tempdf %>%   mutate(pointColor = ifelse(Group == "e", "orange", "purple"))

blinkDF <- df %>% filter(BlinkTime >= 100 & BlinkTime <= 400)
blinkDF <- blinkDF %>% filter(Condition != "tut") 


blinkDF <- blinkDF  %>%
  group_by(Participant, Group, Condition, Trial) %>%
  summarize(BlinkCount = sum(df$Participant == Participant & df$Condition == Condition & df$Trial == Trial))

blinkDF <- blinkDF %>% filter(Participant != "P9")

# blinkDF <- blinkDF %>% filter(Participant != "P9")


tempdf <- blinkDF %>% filter(!is.na(BlinkCount)) 
sd <-sd(tempdf$BlinkCount)
upperLimit <- mean(tempdf$BlinkCount + (sd*3))
lowerLimit <- mean(tempdf$BlinkCount - (sd*3))
tempdf <- tempdf %>% filter(BlinkCount > lowerLimit & BlinkCount < upperLimit)


sd <-sd(tempdf$BlinkCount)
upperLimit <- mean(tempdf$BlinkCount + (sd*3))
lowerLimit <- mean(tempdf$BlinkCount - (sd*3))
tempdf <- tempdf %>% filter(BlinkCount > lowerLimit & BlinkCount < upperLimit)

viewPlot <- tempdf %>%
  group_by(Condition, Group, Participant)%>%
  #summarise(mATT = mean(P2B), sATT = sd(P2B))%>%
  ggplot(aes(Condition,BlinkCount, fill = reorder(Group,-BlinkCount)))+
  geom_boxplot() +
  # geom_boxplot(outlier.shape = NA) +
  labs(title = "Blink Rate", x = "", y = "Blinks per Minute", color = "Group")+
  theme_bw()

viewPlot <- viewPlot + scale_y_continuous(limits = c(0, 100))
viewPlot <- viewPlot + guides(fill=guide_legend(title="Group"))
viewPlot



resultDF <- tempdf %>% filter(Group == "Aut")
result <- t.test(resultDF$BlinkCount ~ resultDF$Condition, var.equal = FALSE)
print(result)

resultDF <- tempdf %>% filter(Group == "Non-Aut")
result <- t.test(resultDF$BlinkCount ~ resultDF$Condition, var.equal = FALSE)
print(result)

resultDF <- tempdf %>% filter(Condition == "Solo")
result <- t.test(resultDF$BlinkCount ~ resultDF$Group, var.equal = FALSE)
print(result)

resultDF <- tempdf %>% filter(Condition == "Cooperative")
result <- t.test(resultDF$BlinkCount ~ resultDF$Group, var.equal = FALSE)
print(result)


model <- aov(tempdf$BlinkCount ~ tempdf$Group * tempdf$Condition, data = tempdf)
summary(model)
#viewPlot+ theme(legend.position = "none")

#Gaze Duration
#--------------------------------------------------------------------------------------
tempdf <- df %>% filter(DurationEvent == "View") 
sd <-sd(tempdf$DurationTime)
upperLimit <- mean(tempdf$DurationTime + (sd*3))
lowerLimit <- mean(tempdf$DurationTime - (sd*3))
tempdf <- tempdf %>% filter(DurationTime > lowerLimit & DurationTime < upperLimit)

standardPlot <- tempdf %>%
  group_by(Condition, Group)%>%
  #summarise(mATT = mean(P2B), sATT = sd(P2B))%>%
  ggplot(aes(Condition,DurationTime, fill = reorder(Group,DurationTime)))+
  geom_boxplot() +
  # geom_boxplot(outlier.shape = NA) +
  labs(title = "Blink Rate", x = "Game Conditions", y = "# of Blinks")+
  theme_bw()

standardPlot <- standardPlot + scale_y_continuous(limits = c(0, 1750))

# Add significance bars
#standardPlot + geom_signif(comparisons = list(c("4", "6"), c("4", "8"), c("6", "8")), map_signif_level = TRUE)
standardPlot

#tempdf <- tempdf %>% filter(Group == "c")


# tempdf <- tempdf %>% filter(Condition == "solo")
# twoANOVA <- aov(tempdf$avgPlayWallDurration ~ factor(tempdf$Group) , data = tempdf)
# summary(twoANOVA)