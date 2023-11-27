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


dataFile <- "singleGazeTransferDF.csv"
#dataFile <- "singleGrab2PlaceMTdf.csv"

df <- read.csv(dataFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)

df <- df %>% mutate(Group = ifelse(Group == "e", "Aut",
                                   ifelse(Group == "c", "Non-Aut", "nothing")))
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

tempdf <- df %>% filter(!is.na(B2P))
sd <-sd(tempdf$B2P)
upperLimit <- mean(tempdf$B2P + (sd*3))
lowerLimit <- mean(tempdf$B2P - (sd*3))



tempdf <- tempdf %>% filter(B2P > lowerLimit & B2P < upperLimit)


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
  summarise(individualPoints = B2P,
            pointColor = pointColor,
            mATT = mean(B2P), sATT = sd(B2P),
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
t_test_result <- t.test(B2P ~ Group, data = tempdf , var.equal = FALSE)
print(t_test_result)


# tempdf <- tempdf %>% filter(Condition == "solo")
# twoANOVA <- aov(tempdf$avgPlayWallDurration ~ factor(tempdf$Group) , data = tempdf)
# summary(twoANOVA)





tempdf <- df %>% filter(!is.na(B2P))
sd <-sd(tempdf$B2P)
upperLimit <- mean(tempdf$B2P + (sd*3))
lowerLimit <- mean(tempdf$B2P - (sd*3))



tempdf <- tempdf %>% filter(B2P > lowerLimit & B2P < upperLimit)


# tempdf <- tempdf %>% filter(Participant != "P22")
#tempdf <- tempdf %>% filter(currentColor == "red")

# 
# tempdf <- tempdf %>% filter(avgPlayerDurration > 100)
# tempdf <- tempdf %>% filter(Condition != "solo")
#tempdf <- tempdf %>% filter(Group != "solo")
#tempdf <- tempdf %>%   mutate(pointColor = ifelse(Group == "e", "orange", "purple"))
tempdf <- df %>% filter(!is.na(B2P))
sd <-sd(tempdf$B2P)
upperLimit <- mean(tempdf$B2P + (sd*3))
lowerLimit <- mean(tempdf$B2P - (sd*3))



tempdf <- tempdf %>% filter(B2P > lowerLimit & B2P < upperLimit)

standardPlot <- tempdf %>%
  group_by(Condition, Group, Participant)%>%
  mutate(pointColor = ifelse(Group == "Aut", "orange", "purple"))%>%
  summarise(pointColor = pointColor,
            mATT = mean(B2P), sATT = sd(B2P))%>%
  ggplot(aes(Condition,B2P, fill = reorder(Group,B2P)))+
  geom_boxplot() +
  labs(title = "Box-and-Whisker Plot", x = "Number of Cylinders", y = "Miles Per Gallon")

# Add significance bars
standardPlot + geom_signif(comparisons = list(c("4", "6"), c("4", "8"), c("6", "8")), map_signif_level = TRUE)
standardPlot

#tempdf <- tempdf %>% filter(Group == "c")
tempdf <- tempdf %>% filter(Condition == "s")
t_test_result <- t.test(B2P ~ Group, data = tempdf , var.equal = FALSE)
print(t_test_result)


# tempdf <- tempdf %>% filter(Condition == "solo")
# twoANOVA <- aov(tempdf$avgPlayWallDurration ~ factor(tempdf$Group) , data = tempdf)
# summary(twoANOVA)