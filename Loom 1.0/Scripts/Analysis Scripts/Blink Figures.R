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

# grabDataFile <- "grabArousalSFNreal2.csv"
DataFile <- "singleBlinkDF.csv"


df <- read.csv(DataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)

df <- df %>% filter(BlinkTime >= 100 & BlinkTime <= 400)

df <- df %>% mutate(Group = ifelse(Group == "e", "Aut",
                                   ifelse(Group == "c", "Non-Aut", "nothing")))

# Specify the value you want to count
# value_to_count <- "P8"
# 
# # Count the number of rows with the specified value in the 'Value' column
# count <- sum(df$Participant == value_to_count)
# count

blinkDF <- df  %>%
  group_by(Participant, Group, Condition, Trial) %>%
  summarize(BlinkCount = sum(df$Participant == Participant & df$Condition == Condition & df$Trial == Trial))

blinkDF <- blinkDF %>% filter(Participant != "P9")

standardPlot <- blinkDF %>%
  group_by(Condition, Group)%>%
  mutate(pointColor = ifelse(Group == "Aut", "orange", "purple"))%>%
  summarise(individualPoints = BlinkCount,
            pointColor = pointColor,
            mATT = mean(BlinkCount), sATT = sd(BlinkCount),
            CI_lower = mATT - 1.96 * sATT / sqrt(n()),
            CI_upper = mATT + 1.96 * sATT / sqrt(n()))%>%
  ggplot(aes(Condition,mATT, fill = reorder(Group,-mATT)))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_point(aes(x = Condition, y = individualPoints, color = pointColor),
             position = position_dodge(width = 1.2), size = 3, show.legend = FALSE) +
  labs(title = "Total Blinks per Trial",
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
tempdf <- blinkDF %>% filter(Group == "c")
t_test_result <- t.test(BlinkCount ~ Condition, data = tempdf , var.equal = FALSE)
print(t_test_result)



