 # this is a script to measure the CGP combo count %'s

library(plot3D)
library(rgl)
library(dplyr)
library(bit64)
library(stringr)
library(tidyr)
library(ggplot2)



dfAllData <- data.frame(Participant = factor(),
                              Group = factor(),
                              Trial = factor(),
                              Trial = numeric(),
                              ZeroObj = numeric(),
                              OneObj = numeric(),
                              TwoObj = numeric(),
                              ThreeObj = numeric(),
                              FourObj = numeric(),
                              FiveObj = numeric(),
                              stringsAsFactors = FALSE)

data_files <- list.files(pattern = ".csv")
for(f in 1:length(data_files))
{
  participantDataFile <- data_files[f]
  print(participantDataFile)
  
  df <- read.csv(participantDataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]
  
  df <- df %>%
    filter(Gaze_Strat != "Missing Gaze Data") %>% 
    filter(Gaze_Strat != "Missing Yolo Data") %>%
    filter(Distance < 100)
  
  
  dfSum <- df %>% group_by(Object_Count)%>%
    summarise(count = n(), 
              percentage = (count / nrow(df)) * 100)
  
  dfWide <- dfSum %>% 
    select(-count) %>% 
    pivot_wider(names_from = Object_Count, values_from = percentage)
  
  Participant <- df$Participant[5]
  Group <- df$Group[5]
  Trial <- df$Trial_Num[5]
  
  ZeroObj <- dfWide$`0`[1]
  OneObj <- dfWide$`1`[1]
  TwoObj <- dfWide$`2`[1]
  ThreeObj <- dfWide$`3`[1]
  FourObj <- dfWide$`4`[1]
  FiveObj <- dfWide$`5`[1]

  
  if(is.null(ZeroObj)){
    ZeroObj <- 0
  }
  if(is.null(OneObj)){
    OneObj <- 0
  }  
  if(is.null(ThreeObj)){
    ThreeObj <- 0
  }  
  if(is.null(TwoObj)){
    TwoObj <- 0
  }
  if(is.null(FourObj)){
    FourObj <- 0
  }
  if(is.null(FiveObj)){
    FiveObj <- 0
  }

  
  newPartRow <- data.frame(Participant, Group, Trial,ZeroObj, OneObj, TwoObj, ThreeObj, FourObj, FiveObj)
  dfAllData <- rbind(dfAllData, newPartRow)
  

}


dfSelect <- dfAllData %>%
  select(ZeroObj, OneObj,TwoObj,ThreeObj,FourObj,FiveObj) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")


ggplot(dfSelect, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "combo %", x = "Variable", y = "Value") +
  theme(legend.position = "none")

