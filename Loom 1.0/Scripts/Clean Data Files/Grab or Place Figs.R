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
dataFile <-"singlePlaceDF.csv"
dataFile <- "singleGrab2PlaceMTdf.csv"

df <- read.csv(dataFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)

placeDF <- df %>% mutate(Group = ifelse(Group == "e", "Aut",
                                   ifelse(Group == "c", "Non-Aut", 
                                          ifelse(Condition == "s","Solo",
                                                ifelse(Condition == "co", "Cooperative", "whatever")))))

placeDF <- placeDF %>% mutate(Condition = ifelse(Condition == "s","Solo",
                                       ifelse(Condition == "co", "Cooperative", "whatever")))   



resultDF <- tempdf %>% filter(Group == "Aut")
result <- t.test(resultDF$MovementTime ~ resultDF$Condition, var.equal = FALSE)
print(result)

resultDF <- tempdf %>% filter(Group == "Non-Aut")
result <- t.test(resultDF$MovementTime ~ resultDF$Condition, var.equal = FALSE)
print(result)

resultDF <- tempdf %>% filter(Condition == "Solo")
result <- t.test(resultDF$MovementTime ~ resultDF$Group, var.equal = FALSE)
print(result)

resultDF <- tempdf %>% filter(Condition == "Cooperative")
result <- t.test(resultDF$MovementTime ~ resultDF$Group, var.equal = FALSE)
print(result)


model <- aov(tempdf$MovementTime ~ tempdf$Group * tempdf$Condition, data = tempdf)
summary(model)


tempdf <- grabDF
# tempdf <- tempdf %>% filter(DurationEvent == "View")
sd <-sd(tempdf$MovementTime)
upperLimit <- mean(tempdf$MovementTime + (sd*3))
lowerLimit <- mean(tempdf$MovementTime - (sd*3))
tempdf <- tempdf %>% filter(MovementTime > lowerLimit & MovementTime < upperLimit)

grabFig <- tempdf %>%
  group_by(Condition, Group)%>%
  #summarise(mATT = mean(P2B), sATT = sd(P2B))%>%
  ggplot(aes(Condition,MovementTime, fill = reorder(Group,-MovementTime)))+
  geom_boxplot() +
  # geom_boxplot(outlier.shape = NA) +
  labs(title = "Grab Movement Time", x = "", y = "", color = "Group")+
  theme_bw()

grabFig <- grabFig + scale_y_continuous(limits = c(0, 2000))
grabFig <- grabFig + guides(fill=guide_legend(title="Group"))
grabFig <- grabFig + theme(legend.position = "none")


tempdf <- placeDF
# tempdf <- tempdf %>% filter(DurationEvent == "View")
sd <-sd(tempdf$MovementTime)
upperLimit <- mean(tempdf$MovementTime + (sd*3))
lowerLimit <- mean(tempdf$MovementTime - (sd*3))
tempdf <- tempdf %>% filter(MovementTime > lowerLimit & MovementTime < upperLimit)

placeFig <- tempdf %>%
  group_by(Condition, Group)%>%
  #summarise(mATT = mean(P2B), sATT = sd(P2B))%>%
  ggplot(aes(Condition,MovementTime, fill = reorder(Group,MovementTime)))+
  geom_boxplot() +
  # geom_boxplot(outlier.shape = NA) +
  labs(title = "Place Movement Time", x = "", y = "", color = "Group")+
  theme_bw()

placeFig <- placeFig + scale_y_continuous(limits = c(0, 2000))
placeFig <- placeFig + guides(fill=guide_legend(title="Group"))
placeFig <- placeFig + theme(legend.position = "none")



tempdf <- grab2placeDF
# tempdf <- tempdf %>% filter(DurationEvent == "View")
sd <-sd(tempdf$MovementTime)
upperLimit <- mean(tempdf$MovementTime + (sd*3))
lowerLimit <- mean(tempdf$MovementTime - (sd*3))
tempdf <- tempdf %>% filter(MovementTime > lowerLimit & MovementTime < upperLimit)

grab2placeFig <- tempdf %>%
  group_by(Condition, Group)%>%
  #summarise(mATT = mean(P2B), sATT = sd(P2B))%>%
  ggplot(aes(Condition,MovementTime, fill = reorder(Group,MovementTime)))+
  geom_boxplot() +
  # geom_boxplot(outlier.shape = NA) +
  labs(title = "Grab to Place Movement Time", x = "", y = "", color = "Group")+
  theme_bw()

grab2placeFig <- grab2placeFig + scale_y_continuous(limits = c(0, 2000))
grab2placeFig <- grab2placeFig + guides(fill=guide_legend(title="Group"))
# placeFig <- placeFig + theme(legend.position = "none")

combined_plot <- plot_grid(
  grabFig, placeFig, grab2placeFig,
  labels = c('A', 'B', 'C'),  # Optional labels for each plot
  nrow = 1,
  rel_widths = c(1,1,1.28)
)
print(combined_plot)

