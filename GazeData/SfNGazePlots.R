library(plot3D)
library(rgl)
#library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(ggsci)
library(ggpubr)
library(ez)
library(ggpubr)



#dataFile <- "AllSubjectGazeData2-25-23_OneMin.csv"
#dataFile <- "pacMoving_2-25-23_OneMin.csv"

dataFile <- "AllSubjectGazeData3-11-23_OneMin.csv"



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
  summarise(mATT = mean(avgBuild2Play), sATT = sd(avgBuild2Play))%>%
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
dat.avg <-
  read.table("AllSubjectGazeData3-11-23_OneMin.csv",
             header=T, sep=",") %>%
  dplyr::mutate(Participant = as.factor(Participant)) %>%
  dplyr::mutate(Condition = factor(condition, levels=c("s", "co")) %>%
                  fct_recode(Solo="s", Cooperative="co")) %>%
  dplyr::mutate(Group = as.factor(group) %>%
                  fct_recode(ASD="e", Ctrl="c"))

write.csv(coDF, "coDF.csv")
write.csv(soloDF, "solooDF.csv")

dat.avg2 <-
  read.table("coDF.csv",
             header=T, sep=",") %>%
  dplyr::mutate(Participant = as.factor(Participant)) %>%
  dplyr::mutate(Group = as.factor(group,levels=c("AUT", "Non-AUT")) %>%
                  fct_recode(ASD="AUT", Ctrl="Non-AUT"))%>%
  dplyr::mutate(Condition = as.factor(condition) %>%
                  fct_recode( Cooperative="Cooperative"))



ezANOVA(dat.avg, dv = avgViewFix, wid = Participant, 
        within = c(Condition), between = c(Group), type=2)


ezANOVA(dat.avg2, dv = avgViewFix, wid = Participant, 
        within = c(Condition), type=2)


ezANOVA(data = coDF, dv = coDF$avgBuildFix, wid = coDF$Participant, 
        within = c(coDF$group), type=2)




v <- dat.avg %>%
  ggplot(aes(x=Condition, y= avgViewFix, color=Group, fill=Group)) +
  geom_boxplot() +
  scale_color_brewer(palette = "Set1") +
  xlab("") +
  ylab("View Wall Fixation Time (ms)") +
  ylim(0, 2250)+
  theme_pubr()

ggsave("View Wall .PNG", width = 9, height = 6, units = "cm", dpi = 320 )  

p <- dat.avg %>%
  ggplot(aes(x=Condition, y= avgPlayFix, color=Group, fill=Group)) +
  geom_boxplot() +
  scale_color_brewer(palette = "Set1") +
  xlab("") +
  ylab("Play Wall Fixation Time (ms)") +
  ylim(0, 2250)+
  theme_pubr()
ggsave("Play Wall Fixation.PNG", width = 9, height = 6, units = "cm", dpi = 320 )  
p

b <- dat.avg %>%
  ggplot(aes(x=Condition, y= avgBuildFix, color=Group, fill=Group)) +
  geom_boxplot() +
  scale_color_brewer(palette = "Set1") +
  xlab("") +
  ylab("Build Wall Fixation Time (ms)") +
  ylim(0, 2250)+
  theme_pubr()
b

ggsave("Build Wall Fixation.PNG", width = 9, height = 6, units = "cm", dpi = 320 )  




ggarrange(v,p,b,
          labels = c("View Wall Fixation Time (ms)", "Play Wall Fixation Time (ms)", "Build Wall Fixation Time (ms)"),
          ncol(3), nrow(1))

ggsave("Gaze Durration Figure.PNG", width = 15, height = 8, units = "cm", dpi = 320 )  



