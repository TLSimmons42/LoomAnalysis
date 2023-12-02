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


dataFile <- "singleGazeTransferDF.csv"
#dataFile <-"singleGazeDurationDF.csv"

#dataFile <- "singleGazeTransferDFold.csv"
#dataFile <-"singleGazeDurationDFold.csv"

df <- read.csv(dataFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)

df <- df %>% mutate(Group = ifelse(Group == "e", "Aut",
                                   ifelse(Group == "c", "Non-Aut", "whatever")))
                                          
df <- df %>% mutate(Condition = ifelse(Condition == "s","Solo",
                                   ifelse(Condition == "co", "Cooperative", "whatever")))                                      
                                          
df <- df %>% mutate(Group = ifelse(Group == "whatever","Non-Aut", Group))



# STATS
#__________________________________________________________________________________________________________________
resultDF <- tempdf %>% filter(Group == "Aut")
result <- t.test(resultDF$P2V ~ resultDF$Condition, var.equal = FALSE)
print(result)

resultDF <- tempdf %>% filter(Group == "Non-Aut")
result <- t.test(resultDF$P2V ~ resultDF$Condition, var.equal = FALSE)
print(result)

resultDF <- tempdf %>% filter(Condition == "Solo")
result <- t.test(resultDF$P2V ~ resultDF$Group, var.equal = FALSE)
print(result)

resultDF <- tempdf %>% filter(Condition == "Cooperative")
result <- t.test(resultDF$P2V ~ resultDF$Group, var.equal = FALSE)
print(result)


model <- aov(tempdf$P2V ~ tempdf$Group * tempdf$Condition, data = tempdf)
summary(model)



# Transfer Figs
#________________________________________________________________________________________________________________

tempdf <- df
tempdf <- tempdf %>% filter(!is.na(P2B))
sd <-sd(tempdf$P2B)
upperLimit <- mean(tempdf$P2B + (sd*3))
lowerLimit <- mean(tempdf$P2B - (sd*3))
tempdf <- tempdf %>% filter(P2B > lowerLimit & P2B < upperLimit)

P2BPlot <- tempdf %>%
  group_by(Condition, Group)%>%
  #summarise(mATT = mean(P2B), sATT = sd(P2B))%>%
  ggplot(aes(Condition,P2B, fill = reorder(Group,P2B)))+
  geom_boxplot() +
  # geom_boxplot(outlier.shape = NA) +
  labs(title = "Play - Build", x = "", y = "Time (ms)", color = "Group")+
  theme_bw()

P2BPlot <- P2BPlot + scale_y_continuous(limits = c(0, 1000))
P2BPlot <- P2BPlot + guides(fill=guide_legend(title="Group"))
P2BPlot <- P2BPlot+ theme(legend.position = "none")

# Add significance bars
#standardPlot + geom_signif(comparisons = list(c("4", "6"), c("4", "8"), c("6", "8")), map_signif_level = TRUE)


tempdf <- df
tempdf <- tempdf %>% filter(!is.na(B2P))
sd <-sd(tempdf$B2P)
upperLimit <- mean(tempdf$B2P + (sd*3))
lowerLimit <- mean(tempdf$B2P - (sd*3))
tempdf <- tempdf %>% filter(B2P > lowerLimit & B2P < upperLimit)

B2PPlot <- tempdf %>%
  group_by(Condition, Group)%>%
  #summarise(mATT = mean(P2B), sATT = sd(P2B))%>%
  ggplot(aes(Condition,B2P, fill = reorder(Group,B2P)))+
  geom_boxplot() +
  # geom_boxplot(outlier.shape = NA) +
  labs(title = "Build - Play", x = "", y = "", color = "Group")+
  theme_bw()

B2PPlot <- B2PPlot + scale_y_continuous(limits = c(0, 1000))
B2PPlot <- B2PPlot + guides(fill=guide_legend(title="Group"))
#B2PPlot+ theme(legend.position = "none")

tempdf <- df
tempdf <- tempdf %>% filter(!is.na(P2V))
sd <-sd(tempdf$P2V)
upperLimit <- mean(tempdf$P2V + (sd*3))
lowerLimit <- mean(tempdf$P2V - (sd*3))
tempdf <- tempdf %>% filter(P2V > lowerLimit & P2V < upperLimit)

P2VPlot <- tempdf %>%
  group_by(Condition, Group)%>%
  #summarise(mATT = mean(P2B), sATT = sd(P2B))%>%
  ggplot(aes(Condition,P2V, fill = reorder(Group,P2V)))+
  geom_boxplot() +
  # geom_boxplot(outlier.shape = NA) +
  labs(title = "Play - View", x = "", y = "Time (ms)", color = "Group")+
  theme_bw()

P2VPlot <- P2VPlot + scale_y_continuous(limits = c(0, 300))
P2VPlot <- P2VPlot + guides(fill=guide_legend(title="Group"))
P2VPlot <- P2VPlot+ theme(legend.position = "none")

tempdf <- df
tempdf <- tempdf %>% filter(!is.na(V2P))
sd <-sd(tempdf$V2P)
upperLimit <- mean(tempdf$V2P + (sd*3))
lowerLimit <- mean(tempdf$V2P - (sd*3))
tempdf <- tempdf %>% filter(V2P > lowerLimit & V2P < upperLimit)

V2PPlot <- tempdf %>%
  group_by(Condition, Group)%>%
  #summarise(mATT = mean(P2B), sATT = sd(P2B))%>%
  ggplot(aes(Condition,V2P, fill = reorder(Group,V2P)))+
  geom_boxplot() +
  # geom_boxplot(outlier.shape = NA) +
  labs(title = "View - Play", x = "", y = "", color = "Group")+
  theme_bw()

V2PPlot <- V2PPlot + scale_y_continuous(limits = c(0, 300))
V2PPlot <- V2PPlot + guides(fill=guide_legend(title="Group"))
V2PPlot+ theme(legend.position = "none")

combined_plot <- plot_grid(
  P2BPlot, B2PPlot, P2VPlot,V2PPlot,
  labels = c('A', 'B', 'C','D'),  # Optional labels for each plot
  nrow = 2,
  rel_widths = c(1,1.33,1,1.33)
)
print(combined_plot)


combined_plot_no_legend <- combined_plot + theme(legend.position = "none") +
  theme(legend.position = "none") +
  guides(fill = FALSE) 
# Display the combined plot


# Durration Figs
#________________________________________________________________________________________________________________

tempdf <- df
tempdf <- tempdf %>% filter(DurationEvent == "View")
sd <-sd(tempdf$DurationTime)
upperLimit <- mean(tempdf$DurationTime + (sd*3))
lowerLimit <- mean(tempdf$DurationTime - (sd*3))
tempdf <- tempdf %>% filter(DurationTime > lowerLimit & DurationTime < upperLimit)

viewPlot <- tempdf %>%
  group_by(Condition, Group)%>%
  #summarise(mATT = mean(P2B), sATT = sd(P2B))%>%
  ggplot(aes(Condition,DurationTime, fill = reorder(Group,-DurationTime)))+
  geom_boxplot() +
  # geom_boxplot(outlier.shape = NA) +
  labs(title = "View Wall", x = "", y = "", color = "Group")+
  theme_bw()

viewPlot <- viewPlot + scale_y_continuous(limits = c(0, 3000))
viewPlot <- viewPlot + guides(fill=guide_legend(title="Group"))
#viewPlot+ theme(legend.position = "none")

# Add significance bars
#standardPlot + geom_signif(comparisons = list(c("4", "6"), c("4", "8"), c("6", "8")), map_signif_level = TRUE)


tempdf <- df
tempdf <- tempdf %>% filter(DurationEvent == "Build")
sd <-sd(tempdf$DurationTime)
upperLimit <- mean(tempdf$DurationTime + (sd*3))
lowerLimit <- mean(tempdf$DurationTime - (sd*3))
tempdf <- tempdf %>% filter(DurationTime > lowerLimit & DurationTime < upperLimit)

buildPlot <- tempdf %>%
  group_by(Condition, Group)%>%
  #summarise(mATT = mean(P2B), sATT = sd(P2B))%>%
  ggplot(aes(Condition,DurationTime, fill = reorder(Group,DurationTime)))+
  geom_boxplot() +
  # geom_boxplot(outlier.shape = NA) +
  labs(title = "Build Wall", x = "", y = "")+
  theme(legend.position = "none")+
  theme_bw()

buildPlot <- buildPlot + scale_y_continuous(limits = c(0, 3000))
buildPlot <-buildPlot+ theme(legend.position = "none")

# Add significance bars
#standardPlot + geom_signif(comparisons = list(c("4", "6"), c("4", "8"), c("6", "8")), map_signif_level = TRUE)



tempdf <- df
tempdf <- tempdf %>% filter(DurationEvent == "Play")
sd <-sd(tempdf$DurationTime)
upperLimit <- mean(tempdf$DurationTime + (sd*3))
lowerLimit <- mean(tempdf$DurationTime - (sd*3))
tempdf <- tempdf %>% filter(DurationTime > lowerLimit & DurationTime < upperLimit)

playPlot <- tempdf %>%
  group_by(Condition, Group)%>%
  #summarise(mATT = mean(P2B), sATT = sd(P2B))%>%
  ggplot(aes(Condition,DurationTime, fill = reorder(Group,DurationTime)))+
  geom_boxplot() +
  # geom_boxplot(outlier.shape = NA) +
  labs(title = "Play Wall", x = "", y = "Time (ms)")+
  theme_bw()
playPlot <- playPlot + scale_y_continuous(limits = c(0, 3000))
playPlot <- playPlot+ theme(legend.position = "none")
playPlot
# Add significance bars
#standardPlot + geom_signif(comparisons = list(c("4", "6"), c("4", "8"), c("6", "8")), map_signif_level = TRUE)



combined_plot <- plot_grid(playPlot, buildPlot, viewPlot, ncol = 3, align = 'hv', rel_widths = c(1, 1, 1.5))
print(combined_plot)


combined_plot <- plot_grid(
  playPlot, buildPlot, viewPlot,
  labels = c('A', 'B', 'C'),  # Optional labels for each plot
  nrow = 1,
  rel_widths = c(1,1,1.33)
)
print(combined_plot)


combined_plot_no_legend <- combined_plot + theme(legend.position = "none") +
  theme(legend.position = "none") +
  guides(fill = FALSE) 
# Display the combined plot





