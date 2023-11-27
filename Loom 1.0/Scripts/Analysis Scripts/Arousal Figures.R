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
DataFile <- "arousalGrab2PlaceDF.csv"



df <- read.csv(DataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)

df <- df %>% mutate(Group = ifelse(participant == "P2" | participant == "P4" |participant == "P5" | participant == "P6" |
                                     participant == "P7" | participant == "P8" | participant == "P10" | participant == "P12" |
                                     participant == "P14" | participant == "P16", "e", "c" ))

df <- df %>% mutate(Group = ifelse(Group == "e", "Aut",
                                   ifelse(Group == "c", "Non-Aut", "nothing")))

placeDFmean <- df  %>%
  #filter(condition == "co")%>%
  #group_by(TimeEpoch, group, condition) %>%
  group_by(TimeEpoch, Group, Condition) %>%
  summarize(
    MeanPercent = mean(PercentChange),
    sd = sd(PercentChange),
    CI_lower = MeanPercent - 1.96 * sd / sqrt(n()),
    CI_upper = MeanPercent + 1.96 * sd / sqrt(n()))



# placeDFmean <- placeDFmean %>%
#   mutate(GroupColor = ifelse(group == "e" & condition == "solo", "e-solo",
#                              ifelse(group == "e" & condition == "co", "e-co",
#                                     ifelse(group == "e" & condition == "comp", "e-comp",
#                                            ifelse(group == "c" & condition == "solo", "c-solo",
#                                                   ifelse(group == "c" & condition == "co", "c-co",
#                                                          ifelse(group == "c" & condition == "comp", "c-comp",NA)))))))



p <- ggplot(placeDFmean, aes(x=TimeEpoch, y=MeanPercent,  color=Group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(mapping = aes(ymin = CI_lower, ymax = CI_upper),
                width = .1, position = position_dodge(width = 0))+
  labs(
    #title = ,
    x = "Percent of Movement Compleated",
    y = "Pupil Response (% Change)",
    color = "Group"
  )+
  facet_wrap(~ Condition
             )+
  theme(strip.text = element_text(face = "bold"), 
        strip.background = element_rect(fill = "gray", color = "black"))+
  theme_bw()

#+theme_pubclean()+scale_fill_startrek()

print(p)