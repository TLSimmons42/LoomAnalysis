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




dataFile <- "singleGrabDF.csv"
#dataFile <-"singlePlaceDF.csv"

df <- read.csv(dataFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)

df <- df %>% mutate(Group = ifelse(Group == "e", "Aut",
                                   ifelse(Group == "c", "Non-Aut", 
                                          ifelse(Condition == "s","Solo",
                                                 ifelse(Condition == "co", "Cooperative", "whatever")))))
tempdf <- df
# tempdf <- tempdf %>% filter(Color == "gray")
sd <-sd(tempdf$MovementTime)
upperLimit <- mean(tempdf$MovementTime + (sd*3))
lowerLimit <- mean(tempdf$MovementTime - (sd*3))
tempdf <- tempdf %>% filter(MovementTime > lowerLimit & MovementTime < upperLimit)

standardPlot <- tempdf %>%
  group_by(Condition, Group)%>%
  #summarise(mATT = mean(P2B), sATT = sd(P2B))%>%
  ggplot(aes(Condition,MovementTime, fill = reorder(Group,MovementTime)))+
  geom_boxplot() +
  # geom_boxplot(outlier.shape = NA) +
  labs(title = "Blink Rate", x = "Game Conditions", y = "# of Blinks")+
  theme_bw()

standardPlot <- standardPlot + scale_y_continuous(limits = c(0, 1000))

# Add significance bars
#standardPlot + geom_signif(comparisons = list(c("4", "6"), c("4", "8"), c("6", "8")), map_signif_level = TRUE)
standardPlot


resultDF <- tempdf %>% filter(Group == "Aut")
result <- t.test(resultDF$MovementTime ~ resultDF$Condition, var.equal = FALSE)
print(result)

resultDF <- tempdf %>% filter(Group == "Non-Aut")
result <- t.test(resultDF$MovementTime ~ resultDF$Condition, var.equal = FALSE)
print(result)

resultDF <- tempdf %>% filter(Condition == "s")
result <- t.test(resultDF$MovementTime ~ resultDF$Group, var.equal = FALSE)
print(result)

resultDF <- tempdf %>% filter(Condition == "co")
result <- t.test(resultDF$MovementTime ~ resultDF$Group, var.equal = FALSE)
print(result)






