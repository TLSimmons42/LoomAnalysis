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

#dataFile <- "gazeTimesReal.csv"
dataFile <- "PACsfnREAL.csv"


df <- read.csv(dataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
df <- read.csv(dataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)


for(i in 1:nrow(df))
{
  #if(df$Group[i] == "1" | df$Group[i] == "f"| is.na(df$Group[i]) | df$Participant[i] == "nuP20"| df$Participant[i] == "nuP17"){
  if(df$Group[i] == "1" | df$Group[i] == "f"| is.na(df$Group[i])){
      
    df$Group[i] <- "e"
  }
  if(df$Participant[i] == "sdP13"| df$Participant[i] == "sdP2"){
    df$Group[i] <- "c"

  }
}

dfAut <- df %>% filter(df$Group == "e")
dfNonAut <- df %>% filter(df$Group != "e")
#df <- df %>% filter(df$Condition != "comp")
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

tempdf <- df %>% filter(!is.na(avgPlayerDurration))
tempdf <- tempdf %>% filter(avgPlayerDurration > 100)
tempdf <- tempdf %>% filter(Condition != "solo")
#tempdf <- tempdf %>% filter(Group != "solo")
#tempdf <- tempdf %>%   mutate(pointColor = ifelse(Group == "e", "orange", "purple"))

standardPlot <- tempdf %>%
  group_by(Condition, Group)%>%
  mutate(pointColor = ifelse(Group == "c", "orange", "purple"))%>%
  summarise(individualPoints = avgPlayerDurration,
            pointColor = pointColor,
            mATT = mean(avgPlayerDurration), sATT = sd(avgPlayerDurration),
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

#standardPlot <- standardPlot + scale_y_continuous(limits = c(0, 1750))
standardPlot
standardPlot+ theme(legend.position = "none")

tempdf <- tempdf %>% filter(Condition == "solo")
twoANOVA <- aov(tempdf$avgPlayWallDurration ~ factor(tempdf$Group) , data = tempdf)
summary(twoANOVA)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

df <- df %>% filter(!is.na(df$avgGoldCubeGrab))
df <- df %>% filter(!is.na(df$avgRedCubeGrab))
df <- df %>% filter(!is.na(df$avgBlueCubeGrab))
df <- df %>% filter(!is.na(df$avgWhiteCubeGrab))


newDF <-df%>% group_by(Group)%>%
  filter(Condition == "co") %>%
  group_by(Group)%>%
  summarise(Gold = mean(avgGoldCubeGrab),
            Red = mean(avgRedCubeGrab),
            Blue = mean(avgBlueCubeGrab),
            White = mean(avgWhiteCubeGrab),
            gold_lower = Gold - 1.96 * sd(avgGoldCubeGrab) / sqrt(n()),
            gold_upper = Gold + 1.96 * sd(avgGoldCubeGrab) / sqrt(n()),
            red_lower = Red - 1.96 * sd(avgRedCubeGrab) / sqrt(n()),
            red_upper = Red + 1.96 * sd(avgRedCubeGrab) / sqrt(n()),
            blue_lower = Blue - 1.96 * sd(avgBlueCubeGrab) / sqrt(n()),
            blue_upper = Blue + 1.96 * sd(avgBlueCubeGrab) / sqrt(n()),
            white_lower = White - 1.96 * sd(avgWhiteCubeGrab) / sqrt(n()),
            white_upper = White + 1.96 * sd(avgWhiteCubeGrab) / sqrt(n()))

colorDF <- data.frame(Color =c("Gold", "Gold","White","White","Red","Red","Blue","Blue"),
                      Means = c(newDF$Gold[1],newDF$Gold[2],newDF$White[1],newDF$White[2],newDF$Red[1],newDF$Red[2],newDF$Blue[1],newDF$Blue[2]),
                      ci_upper = c(newDF$gold_upper[1],newDF$gold_upper[2],newDF$white_upper[1],newDF$white_upper[2],newDF$red_upper[1],newDF$red_upper[2],newDF$blue_upper[1],newDF$blue_upper[2]),
                      ci_lower = c(newDF$gold_lower[1],newDF$gold_lower[2],newDF$white_lower[1],newDF$white_lower[2],newDF$red_lower[1],newDF$red_lower[2],newDF$blue_lower[1],newDF$blue_lower[2]),
                      Group = c(newDF$Group[1],newDF$Group[2],newDF$Group[1],newDF$Group[2],newDF$Group[1],newDF$Group[2],newDF$Group[1],newDF$Group[2]))

gazePlot <- colorDF%>%
  group_by(Group,Color)%>%
  ggplot(aes(Color,Means, fill = reorder(Group,Means)))+
  geom_bar(stat = "identity", position = "dodge")+
  #geom_text(mapping=aes(label=round(mATT,2)), position = position_dodge(width = 0.9),
  #        cex= 2.5, vjust=-2)+
  labs(title = "Solo Cube Grab",
       subtitle = "",
       x = "Trial Condition", y = "Time (ms)",
       #caption = "moo",
       fill = "")+
  geom_errorbar(mapping = aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, position = position_dodge(width = 0.9))+
  theme_pubclean()+scale_fill_startrek()

gazePlot
#-------------------------------------------------------------------------------------------------------------------------------------------------------------


newDF <-df%>% group_by(Group)%>%
  filter(Condition == "comp") %>%
  filter(!is.na(avgWhiteCubeGrab))%>%
  group_by(Group)%>%
  summarise(Gold = mean(avgGoldCubeGrab),
            Red = mean(avgRedCubeGrab),
            Blue = mean(avgBlueCubeGrab),
            White = mean(avgWhiteCubeGrab),
            gold_lower = Gold - 1.96 * sd(avgGoldCubeGrab) / sqrt(n()),
            gold_upper = Gold + 1.96 * sd(avgGoldCubeGrab) / sqrt(n()),
            red_lower = Red - 1.96 * sd(avgRedCubeGrab) / sqrt(n()),
            red_upper = Red + 1.96 * sd(avgRedCubeGrab) / sqrt(n()),
            blue_lower = Blue - 1.96 * sd(avgBlueCubeGrab) / sqrt(n()),
            blue_upper = Blue + 1.96 * sd(avgBlueCubeGrab) / sqrt(n()),
            white_lower = White - 1.96 * sd(avgWhiteCubeGrab) / sqrt(n()),
            white_upper = White + 1.96 * sd(avgWhiteCubeGrab) / sqrt(n()))

colorDF <- data.frame(Color =c("Gold", "Gold","White","White","Red","Red","Blue","Blue"),
                      Means = c(newDF$Gold[1],newDF$Gold[2],newDF$White[1],newDF$White[2],newDF$Red[1],newDF$Red[2],newDF$Blue[1],newDF$Blue[2]),
                      ci_upper = c(newDF$gold_upper[1],newDF$gold_upper[2],newDF$white_upper[1],newDF$white_upper[2],newDF$red_upper[1],newDF$red_upper[2],newDF$blue_upper[1],newDF$blue_upper[2]),
                      ci_lower = c(newDF$gold_lower[1],newDF$gold_lower[2],newDF$white_lower[1],newDF$white_lower[2],newDF$red_lower[1],newDF$red_lower[2],newDF$blue_lower[1],newDF$blue_lower[2]),
                      Group = c(newDF$Group[1],newDF$Group[2],newDF$Group[1],newDF$Group[2],newDF$Group[1],newDF$Group[2],newDF$Group[1],newDF$Group[2]))

gazePlot <- colorDF%>%
  group_by(Group,Color)%>%
  ggplot(aes(Color,Means, fill = reorder(Group,Means)))+
  geom_bar(stat = "identity", position = "dodge")+
  #geom_text(mapping=aes(label=round(mATT,2)), position = position_dodge(width = 0.9),
  #        cex= 2.5, vjust=-2)+
  labs(title = "PAC Cube Placement Sequence",
       subtitle = "",
       x = "Trial Condition", y = "Time (ms)",
       #caption = "moo",
       fill = "")+
  geom_errorbar(mapping = aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, position = position_dodge(width = 0.9))+
  theme_pubclean()+scale_fill_startrek()

gazePlot


twoANOVA <- aov(newDF$avgWhiteCubeGrab ~ factor(newDF$Group) , data = newDF)
summary(twoANOVA)
#-------------------------------------------------------------------------------------------------------------------- 
dfBlue <- df %>% mutate(newBlue = ifelse(is.na(df$avgBlueCubeGrab),avgRedCubeGrab,avgBlueCubeGrab))
dfBlue <- dfBlue %>% filter(Condition == "solo")
twoANOVA <- aov(dfBlue$newBlue ~ factor(dfBlue$Group) , data = dfBlue)
summary(twoANOVA)

dfWhite <- df %>% filter(!is.na(df$avgWhiteCubeGrab))
dfWhite <- dfWhite %>% filter(Condition == "comp")
twoANOVA <- aov(dfWhite$avgWhiteCubeGrab ~ factor(dfWhite$Group) , data = dfWhite)
summary(twoANOVA)

dfGold <- df %>% filter(!is.na(df$avgGoldCubeGrab))
dfGold <- dfGold %>% filter(Condition == "solo")
twoANOVA <- aov(dfGold$avgGoldCubeGrab ~ factor(dfGold$Group) , data = dfGold)
summary(twoANOVA)


# dfBlue <- df %>% filter(!is.na(df$avgBlueCubeGrab))
dfRed <- df %>% filter(!is.na(df$avgRedCubeGrab))
dfGold <- df %>% filter(!is.na(df$avgGoldCubeGrab))
dfWhite <- df %>% filter(!is.na(df$avgWhiteCubeGrab))

dfBlue <- df %>% mutate(newBlue = ifelse(is.na(df$avgBlueCubeGrab),avgRedCubeGrab,avgBlueCubeGrab))



bluePACplot <- dfBlue %>%
  group_by(Condition, Group)%>%
  mutate(pointColor = ifelse(Group == "c", "orange", "purple"))%>%
  summarise(pointColor = pointColor,
            newPoints = (newBlue + avgRedCubeGrab)/2,
            individualPoints = newPoints,
            mATT = mean(newPoints), sATT = sd(newPoints),
            CI_lower = mATT - 1.96 * sATT / sqrt(n()),
            CI_upper = mATT + 1.96 * sATT / sqrt(n()))%>%
  ggplot(aes(Condition,mATT, fill = reorder(Group,mATT)))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_point(aes(x = Condition, y = individualPoints, color = pointColor),
             position = position_dodge(width = 1.2), size = 2) +
  labs(title = "Blue and Red Cube Grab Times",
       x = "", y = "",
       #caption = "moo",
       fill = "")+
  #scale_y_continuous(limits = c(0, 1000))+  # Set y-axis limits
  geom_errorbar(mapping = aes(ymin = CI_lower, ymax = CI_upper),
                width = 0.2, position = position_dodge(width = 0.9), size = 1)+
  theme_pubclean()+scale_fill_startrek()

bluePACplot <- bluePACplot + scale_y_continuous(limits = c(0, 1500))+ theme(legend.position = "none")

bluePACplot




whitePACplot <- dfWhite %>%
  group_by(Condition, Group)%>%
  mutate(pointColor = ifelse(Group == "c", "orange", "purple"))%>%
  summarise(individualPoints = avgWhiteCubeGrab,
            pointColor = pointColor,
            mATT = mean(avgWhiteCubeGrab), sATT = sd(avgWhiteCubeGrab),
            CI_lower = mATT - 1.96 * sATT / sqrt(n()),
            CI_upper = mATT + 1.96 * sATT / sqrt(n()))%>%
  ggplot(aes(Condition,mATT, fill = reorder(Group,mATT)))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_point(aes(x = Condition, y = individualPoints, color = pointColor),
             position = position_dodge(width = 1.2), size = 2) +
  labs(title = "White Cube Grab Times",
       x = "", y = "",
       #caption = "moo",
       fill = "")+
  #scale_y_continuous(limits = c(0, 1000))+  # Set y-axis limits
  geom_errorbar(mapping = aes(ymin = CI_lower, ymax = CI_upper),
                width = 0.2, position = position_dodge(width = 0.9), size = 1)+
  theme_pubclean()+scale_fill_startrek()

whitePACplot <- whitePACplot + scale_y_continuous(limits = c(0, 1500))+ theme(legend.position = "none")

whitePACplot


goldPACplot <- dfGold %>%
  group_by(Condition, Group)%>%
  mutate(pointColor = ifelse(Group == "c", "orange", "purple"))%>%
  summarise(individualPoints = avgGoldCubeGrab,
            pointColor = pointColor,
            mATT = mean(avgGoldCubeGrab), sATT = sd(avgGoldCubeGrab),
            CI_lower = mATT - 1.96 * sATT / sqrt(n()),
            CI_upper = mATT + 1.96 * sATT / sqrt(n()))%>%
  ggplot(aes(Condition,mATT, fill = reorder(Group,mATT)))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_point(aes(x = Condition, y = individualPoints, color = pointColor),
             position = position_dodge(width = 1.2), size = 2) +
  labs(title = "Gold Cube Grab Times",
       x = "", y = "Time (ms)",
       #caption = "moo",
       fill = "")+
  #scale_y_continuous(limits = c(0, 1000))+  # Set y-axis limits
  geom_errorbar(mapping = aes(ymin = CI_lower, ymax = CI_upper),
                width = 0.2, position = position_dodge(width = 0.9), size = 1)+
  theme_pubclean()+scale_fill_startrek()

goldPACplot <- goldPACplot + scale_y_continuous(limits = c(0, 1500))+ theme(legend.position = "none")

goldPACplot


combined_plot <- plot_grid(goldPACplot, bluePACplot, whitePACplot, nrow = 1, ncol = 3)

combined_plot