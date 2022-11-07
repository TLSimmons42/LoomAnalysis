library(plot3D)
library(rgl)
#library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(ggsci)
library(ggpubr)

dataFile <- "pacMoving.csv"


df <- read.csv(dataFile, header = TRUE, sep = ",")
df$partGroup[df$partGroup == "e"] <- 'AUT'
df$partGroup[df$partGroup == "c"] <- 'Non-AUT'

df$condition[df$condition == "s"] <- 'Solo'
df$condition[df$condition == "co"] <- 'Cooperative'



gazePlot <- df%>%
  group_by(condition, partGroup)%>%
  summarise(mATT = mean(pacStay), sATT = sd(pacStay))%>%
  ggplot(aes(reorder(condition,mATT),mATT, fill = reorder(partGroup,mATT)))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(mapping=aes(label=round(mATT,2)), position = position_dodge(width = 0.9),
            cex= 2.5, vjust=-2)+
  labs(title = "PAC2",
       #subtitle = "kitty",
       x = "Conditions", y = "Time (ms)",
       #caption = "moo",
       fill = "Groups")+
  geom_errorbar(mapping = aes(ymin = mATT-sATT, ymax = mATT + sATT),
                width = 0.2, position = position_dodge(width = 0.9))+
  theme_bw()+scale_fill_aaas() 


gazePlot

#----------------------------------------------------------------------------------------------------------------

#df$partGroup <- reorder(x = df$partGroup, X = )

gazePlot <- df%>%
  group_by(condition, partGroup)%>%
  summarise(mATT = mean(pacMove), sATT = sd(pacMove))%>%
  ggplot(aes(reorder(condition,mATT),mATT, fill = reorder(partGroup, -mATT)))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(mapping=aes(label=round(mATT,2)), position = position_dodge(width = 0.9),
            cex= 2.5, vjust=-2)+
  labs(title = "PAC1",
       #subtitle = "kitty",
       x = "Conditions", y = "Time (ms)",
       #caption = "moo",
       fill = "Groups")+
  geom_errorbar(mapping = aes(ymin = mATT-sATT, ymax = mATT + sATT),
                width = 0.2, position = position_dodge(width = 0.9))+
  theme_bw()+scale_fill_aaas() 


gazePlot