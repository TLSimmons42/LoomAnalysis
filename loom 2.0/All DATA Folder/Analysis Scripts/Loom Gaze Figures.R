library(plot3D)
library(rgl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(ggpubr)
library(ez)
library(cowplot)

#dataFile <- "gazeDurationTimes 9-6.csv"
dataFile <- "PACdf 9-20-23.csv"



#df <- read.csv(dataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
df <- read.csv(dataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)


ggplot(df, aes(x = Condition, y = avgPlaceAreaPAC)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +  # Add jittered points
  labs(title = "Box Plot with Individual Points for Three Conditions",
       x = "Condition",
       y = "Value")


data_excluded <- subset(df, Condition != "solo")


anova_result <- aov(avgPlaceAreaPAC ~ Condition, data = data_excluded)
summary(anova_result)
  
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