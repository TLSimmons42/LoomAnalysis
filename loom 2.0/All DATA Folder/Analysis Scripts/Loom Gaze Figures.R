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

#dataFile <- "gazeDurationTimes 10-4-23.csv"
dataFile <- "PACdf 10-5-23.csv"


df <- read.csv(dataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
df <- read.csv(dataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)

for(i in 1:nrow(df))
{
  if(df$Group[i] == "1" | df$Group[i] == "f"| is.na(df$Group[i])){
    df$Group[i] <- "e"
  }
}
#df <- df %>% filter(df$Condition != "comp")
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

# 
# ggplot(df, aes(x = Condition, y = avgPlaceAreaPAC)) +
#   geom_boxplot() +
#   geom_jitter(width = 0.2, alpha = 0.5) +  # Add jittered points
#   labs(title = "Box Plot with Individual Points for Three Conditions",
#        x = "Condition",
#        y = "Value")
# 
# 
# #data_excluded <- subset(df, Condition != "solo")
# 
# 
# anova_result <- aov(avgPlaceAreaPAC ~ Condition, data = df)
# summary(anova_result)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

df <- df %>% filter(!is.na(df$avgGoldCubeGrab))
df <- df %>% filter(!is.na(df$avgRedCubeGrab))
df <- df %>% filter(!is.na(df$avgBlueCubeGrab))
df <- df %>% filter(!is.na(df$avgWhiteCubeGrab))


newDF <-df%>% group_by(Group)%>%
  filter(Condition == "comp") %>%
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
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

newDF <-df%>% group_by(Group)%>%
  filter(Condition == "comp") %>%
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


twoANOVA <- aov(df$avgBlueCubeGrab ~ factor(df$Condition) * factor(df$Group) , data = df)
summary(twoANOVA)
#-------------------------------------------------------------------------------------------------------------------- 
dfBlue <- df %>% filter(!is.na(df$avgBlueCubeGrab))
dfRed <- df %>% filter(!is.na(df$avgRedCubeGrab))
dfGold <- df %>% filter(!is.na(df$avgGoldCubeGrab))
dfWhite <- df %>% filter(!is.na(df$avgWhiteCubeGrab))


bluePACplot <- dfBlue%>%
  group_by(Condition, Group)%>%
  summarise(mATT = mean(avgBlueCubeGrab), sATT = sd(avgBlueCubeGrab))%>%
  ggplot(aes(reorder(Condition,mATT),mATT, fill = reorder(Group,mATT)))+
  geom_bar(stat = "identity", position = "dodge")+
  #geom_text(mapping=aes(label=round(mATT,2)), position = position_dodge(width = 0.9),
  #        cex= 2.5, vjust=-2)+
  labs(title = "Blue",
       subtitle = "",
       x = "Trial Condition", y = "Time (ms)",
       #caption = "moo",
       fill = "")+
  scale_y_continuous(limits = c(0, 1000))+  # Set y-axis limits 
  geom_errorbar(mapping = aes(ymin = mATT-sATT, ymax = mATT + sATT),
                width = 0.2, position = position_dodge(width = 0.9))+
  theme_pubclean()+scale_fill_startrek() 

bluePACplot

redPACplot <- dfRed%>%
  group_by(Condition, Group)%>%
  summarise(mATT = mean(avgRedCubeGrab), sATT = sd(avgRedCubeGrab))%>%
  ggplot(aes(reorder(Condition,mATT),mATT, fill = reorder(Group,mATT)))+
  geom_bar(stat = "identity", position = "dodge")+
  #geom_text(mapping=aes(label=round(mATT,2)), position = position_dodge(width = 0.9),
  #        cex= 2.5, vjust=-2)+
  labs(title = "Red",
       subtitle = "",
       x = "Trial Condition", y = "Time (ms)",
       #caption = "moo",
       fill = "")+
  scale_y_continuous(limits = c(0, 1000))+  # Set y-axis limits
  geom_errorbar(mapping = aes(ymin = mATT-sATT, ymax = mATT + sATT),
                width = 0.2, position = position_dodge(width = 0.9))+
  theme_pubclean()+scale_fill_startrek()

redPACplot

whitePACplot <- dfWhite%>%
  group_by(Condition, Group)%>%
  summarise(mATT = mean(avgWhiteCubeGrab), sATT = sd(avgWhiteCubeGrab))%>%
  ggplot(aes(reorder(Condition,mATT),mATT, fill = reorder(Group,mATT)))+
  geom_bar(stat = "identity", position = "dodge")+
  #geom_text(mapping=aes(label=round(mATT,2)), position = position_dodge(width = 0.9),
  #        cex= 2.5, vjust=-2)+
  labs(title = "White",
       subtitle = "",
       x = "Trial Condition", y = "Time (ms)",
       #caption = "moo",
       fill = "")+
  scale_y_continuous(limits = c(0, 1000))+  # Set y-axis limits
  geom_errorbar(mapping = aes(ymin = mATT-sATT, ymax = mATT + sATT),
                width = 0.2, position = position_dodge(width = 0.9))+
  theme_pubclean()+scale_fill_startrek()

goldPACplot <- dfGold%>%
  group_by(Condition, Group)%>%
  summarise(mATT = mean(avgGoldCubeGrab), sATT = sd(avgGoldCubeGrab))%>%
  ggplot(aes(reorder(Condition,mATT),mATT, fill = reorder(Group,mATT)))+
  geom_bar(stat = "identity", position = "dodge")+
  #geom_text(mapping=aes(label=round(mATT,2)), position = position_dodge(width = 0.9),
  #        cex= 2.5, vjust=-2)+
  labs(title = "Gold",
       subtitle = "",
       x = "Trial Condition", y = "Time (ms)",
       #caption = "moo",
       fill = "")+
  scale_y_continuous(limits = c(0, 1000))+  # Set y-axis limits
  geom_errorbar(mapping = aes(ymin = mATT-sATT, ymax = mATT + sATT),
                width = 0.2, position = position_dodge(width = 0.9))+
  theme_pubclean()+scale_fill_startrek()

combined_plot <- plot_grid(goldPACplot, bluePACplot, redPACplot, whitePACplot, nrow = 1, ncol = 4)

combined_plot