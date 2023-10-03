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

#dataFile <- "gazeDurationTimes 9-6.csv"
#dataFile <- "gazeDurationTimes 9-27-23.csv"
dataFile <- "PACdf 10-3-23.csv"


df <- read.csv(dataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
df <- read.csv(dataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)

for(i in 1:nrow(df))
{
  if(df$Group[i] == "1" | df$Group[i] == "f"){
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

df <- df %>% filter(!is.na(df$avgGrabPAC))
  

gazePlot <- df%>%
  group_by(Condition, Group)%>%
  summarise(mATT = mean(avgGrabPAC), sATT = sd(avgGrabPAC))%>%
  ggplot(aes(reorder(Condition,mATT),mATT, fill = reorder(Group,mATT)))+
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

twoANOVA <- aov(df$avgGrabPAC ~ factor(df$Condition) * factor(df$Group) , data = df)
summary(twoANOVA)
  
# gazePlot <- df%>%
#   group_by(Condition, group)%>%
#   summarise(mATT = mean(avgBuild2Play), sATT = sd(avgBuild2Play))%>%
#   ggplot(aes(reorder(condition,mATT),mATT, fill = reorder(group,mATT)))+
#   geom_bar(stat = "identity", position = "dodge")+
#   #geom_text(mapping=aes(label=round(mATT,2)), position = position_dodge(width = 0.9),
#   #        cex= 2.5, vjust=-2)+
#   labs(title = "PAC Cube Placement Sequence",
#        subtitle = "",
#        x = "Trial Condition", y = "Time (ms)",
#        #caption = "moo",
#        fill = "")+
#   geom_errorbar(mapping = aes(ymin = mATT-sATT, ymax = mATT + sATT),
#                 width = 0.2, position = position_dodge(width = 0.9))+
#   theme_pubclean()+scale_fill_startrek() 
# 
# gazePlot