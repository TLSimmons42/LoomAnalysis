library(plot3D)
library(rgl)
library(dplyr)
library(bit64)
library(stringr)


data_files <- list.files(pattern = ".csv")

PACdf <- data.frame(Time = numeric(),
                    Participant = factor(),
                    Condition = factor(),
                    Trial = numeric(),
                    Group = factor(),
                    grabPACcount = numeric(),
                    placePACcount = numeric(),
                    avgGrabPAC = numeric(),
                    avgPlacePAC = numeric(),
                    grabAreaPACcount = numeric(),
                    placeAreaPACcount = numeric(),
                    avgGrabAreaPAC = numeric(),
                    avgPlaceAreaPAC = numeric(),
                    stringsAsFactors = FALSE)


individualPACdf <- data.frame(Time = numeric(),
                              Participant = factor(),
                              Condition = factor(),
                              Trial = numeric(),
                              Group = factor(),
                              PACtype = factor(),
                              PACtime = numeric(),
                              PACstartTime = numeric(),
                              PACendTime = numeric(),
                              Event = factor(),
                              stringsAsFactors = FALSE)


combined_Arousaldf <- data.frame()
rawCombined_Arousaldf <- data.frame()

combined_ArousaldfPlace <- data.frame()
rawCombined_ArousaldfPlace <- data.frame()

for(f in 1:length(data_files))
{
  participantDataFile <- data_files[f]
  print(participantDataFile)
  
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]
  df$Event <- gsub("\\(|\\)", "", df$Event)
  df$CurrentGazeTarget <- gsub("\\(|\\)", "", df$CurrentGazeTarget)
  gameOverTime <- df[df$Event == "Game Over",]
  df <- df %>% filter(df$Time <= gameOverTime[1,1])
  
  
  # Find all of the Picked events
  trimedGrabDF <- df[grep("picked", df$Event), ]
  #trimedGrabDF <- trimedGrabDF %>% filter(CurrentGazeArea == "play_wall")
  trimedGrabDF <- trimedGrabDF %>% filter(!grepl("by", Event))
  
  
  trimedPlaceDF <- df[grep("placed", df$Event), ]
  trimedPlaceDF <- trimedPlaceDF %>% filter(!grepl("Hit", Event))
  trimedPlaceDF <- trimedPlaceDF %>% filter(!grepl("HB", Event))
  trimedPlaceDF <- trimedPlaceDF[!duplicated(trimedPlaceDF$Event), ]
  #trimedPlaceDF <- trimedPlaceDF %>% filter(CurrentGazeArea == "build_wall")
  
  
  combined_df <- data.frame()
  combined_dfPlace <- data.frame()
  
  
  if(df[1,8] != "solo")
  {
    trimedP2GrabDF <- df[grep("picked", df$Event), ]
    trimedP2GrabDF <- trimedP2GrabDF %>% filter(grepl("P2", Event))
    
    for(b in 1:nrow(trimedP2GrabDF))
    {
      input_string <- trimedP2GrabDF[b,10]
      
      # Find the position of " was picked up"
      pos <- regexpr(" was picked up by P2", input_string)
      
      # Extract the substring before the target text
      result <- substr(input_string, 1, pos - 1)
      result <- paste(result, "was", sep = "")
      #result <- gsub("(Clone)", "Clone", result)
      
      
      trimedPlaceDF <- trimedPlaceDF %>% filter(!grepl(result, Event))
      
    }
  }

  for(i in 1:nrow(trimedGrabDF))
  {
    i <- 1
    trimedGrabDF <- trimedGrabDF %>% filter(LeftPupil != -1 & RightPupil != -1)
    
    currentTime <- trimedGrabDF[i,1]
    currentGazeArea <- trimedGrabDF[i,12]
    currentEvent <- trimedGrabDF[i,10]
    currentArousal <- (trimedGrabDF[i,55] + trimedGrabDF[i,56])/2
    


    subDFpre <- df %>% filter(Time > (currentTime-20000000) & Time <= currentTime)
    subDFpost <- df %>% filter(Time < (currentTime+20000000) & Time >= currentTime)
    subDFFull <- df %>% filter(Time < (currentTime+20000000) & Time > (currentTime-20000000))
    subDFFull <- subDFFull %>% filter(LeftPupil != -1 & RightPupil != -1)
    
    firstArousal <- (subDFFull[1,55] + subDFFull[1,56])/2
    
    
    if(nrow(subDFFull) != 0){
      subDFFull <- subDFFull %>%
        mutate(pupilAverage = (RightPupil + LeftPupil) / 2)
      
      subDFFull <- subDFFull %>%
        #mutate(PercentChange = (pupilAverage - currentArousal) / currentArousal * 100)
        mutate(PercentChange = (pupilAverage - firstArousal) / firstArousal * 100)
      
      
      subDFFull <- subDFFull %>%
        mutate(TimeEpoch = round((Time/10000000 - currentTime/10000000),1))
      
      #group_mean <- aggregate(subDFFull$PercentChange, list(subDFFull$TimeEpoch), mean)
      
      group_mean <- subDFFull  %>%
        group_by(TimeEpoch) %>%
        summarize(
          MeanPercentChange = mean(PercentChange),
          Meanpupil = mean(pupilAverage))
      
      
      combined_df <- rbind(combined_df, group_mean)
    }
  }
  
  if(nrow(trimedPlaceDF) != 0){
    for(i in 1:nrow(trimedPlaceDF))
    {
      trimedPlaceDF <- trimedPlaceDF %>% filter(LeftPupil != -1 & RightPupil != -1)
      
      currentTime <- trimedPlaceDF[i,1]
      currentGazeArea <- trimedPlaceDF[i,12]
      currentEvent <- trimedPlaceDF[i,10]
      currentArousal <- (trimedPlaceDF[i,55] + trimedPlaceDF[i,56])/2
      
      
      
      subDFpre <- df %>% filter(Time > (currentTime-20000000) & Time <= currentTime)
      subDFpost <- df %>% filter(Time < (currentTime+20000000) & Time >= currentTime)
      subDFFull <- df %>% filter(Time < (currentTime+20000000) & Time > (currentTime-20000000))
      subDFFull <- subDFFull %>% filter(LeftPupil != -1 & RightPupil != -1)
      
      firstArousal <- (subDFFull[1,55] + subDFFull[1,56])/2
      
      
      if(nrow(subDFFull) != 0){
        subDFFull <- subDFFull %>%
          mutate(pupilAverage = (RightPupil + LeftPupil) / 2)
        
        subDFFull <- subDFFull %>%
          #mutate(PercentChange = (pupilAverage - currentArousal) / currentArousal * 100)
          mutate(PercentChange = (pupilAverage - firstArousal) / firstArousal * 100)
        
        
        subDFFull <- subDFFull %>%
          mutate(TimeEpoch = round((Time/10000000 - currentTime/10000000),1))
        
        #group_mean <- aggregate(subDFFull$PercentChange, list(subDFFull$TimeEpoch), mean)
        
        group_mean <- subDFFull  %>%
          group_by(TimeEpoch) %>%
          summarize(
            MeanPercentChange = mean(PercentChange),
            Meanpupil = mean(pupilAverage))
        
        
        combined_dfPlace <- rbind(combined_dfPlace, group_mean)
      }
    }
  }
  #total_group_mean <- aggregate(combined_df$x, list(combined_df$Group.1), mean)
  
  total_group_mean <- combined_df  %>%
    group_by(TimeEpoch) %>%
    summarize(
      MeanPercentChange = mean(MeanPercentChange),
      Meanpupil = mean(Meanpupil))
  
  total_group_mean <- total_group_mean %>%
    mutate(condition = trimedPlaceDF$Condition[1])
  total_group_mean <- total_group_mean %>%
    mutate(group = trimedPlaceDF$Group[1])
  total_group_mean <- total_group_mean %>%
    mutate(Participant = trimedPlaceDF$Participant[1])
  
  rawCombined_Arousaldf <- rbind(rawCombined_Arousaldf, total_group_mean)
  
  
  if(nrow(trimedPlaceDF) != 0){
    
    total_group_mean <- combined_dfPlace  %>%
      group_by(TimeEpoch) %>%
      summarize(
        MeanPercentChange = mean(MeanPercentChange),
        Meanpupil = mean(Meanpupil))
    
    total_group_mean <- total_group_mean %>%
      mutate(condition = trimedPlaceDF$Condition[1])
    total_group_mean <- total_group_mean %>%
      mutate(group = trimedPlaceDF$Group[1])
    total_group_mean <- total_group_mean %>%
      mutate(Participant = trimedPlaceDF$Participant[1])
    
      
    
    rawCombined_ArousaldfPlace <- rbind(rawCombined_ArousaldfPlace, total_group_mean)
  }
  
}


# dataFile <- "combined_arousaldfmean 1-27-23.csv"
# #dataFile <- "PACdf 10-5-23.csv"
# 
# 
# df <- read.csv(dataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
# df <- read.csv(dataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)

#combined_Arousaldf <- aggregate(combined_Arousaldf$x, list(combined_Arousaldf$Group.1), mean)

combined_Arousaldf <- rawCombined_Arousaldf  %>%
       group_by(TimeEpoch, condition, Participant, group) %>%
       summarize(
             MeanPercent = mean(MeanPercentChange),
             MeanPupilSize = mean(Meanpupil))

combined_Arousaldfmean <- combined_Arousaldf  %>%
  group_by(TimeEpoch, condition, group) %>%
  summarize(
    MeanPercent = mean(MeanPercent),
    MeanPupilSize = mean(MeanPupilSize))


for(i in 1:nrow(combined_Arousaldf))
{
  print(combined_Arousaldf$Participant[i])
  if(combined_Arousaldf$group[i] == "1" | combined_Arousaldf$group[i] == "f" | is.na(combined_Arousaldf$group[i]))
  {
    combined_Arousaldf$group[i] <- "e"
  }
  if(combined_Arousaldf$Participant[i] == "sdP13" & !is.na(combined_Arousaldf$Participant[i])){
    combined_Arousaldf$group[i] <- "c"

  }

}

combined_ArousaldfPlace <- rawCombined_ArousaldfPlace  %>%
  group_by(TimeEpoch, condition, Participant, group) %>%
  summarize(
    MeanPercent = mean(MeanPercentChange),
    MeanPupilSize = mean(Meanpupil))

combined_ArousaldfmeanPlace <- combined_ArousaldfPlace  %>%
  group_by(TimeEpoch, condition, group) %>%
  summarize(
    MeanPercent = mean(MeanPercent),
    MeanPupilSize = mean(MeanPupilSize))



for(i in 1:nrow(combined_ArousaldfPlace))
{
  print(combined_ArousaldfPlace$Participant[i])
  if(combined_ArousaldfPlace$group[i] == "1" | combined_ArousaldfPlace$group[i] == "f" | is.na(combined_ArousaldfPlace$group[i]))
  {
    combined_ArousaldfPlace$group[i] <- "e"
  }
  if(combined_ArousaldfPlace$Participant[i] == "sdP13" & !is.na(combined_ArousaldfPlace$Participant[i])){
    combined_ArousaldfPlace$group[i] <- "c"
    
  }
  
}



combined_Arousaldfmean <- combined_Arousaldfmean %>%
  mutate(GroupColor = ifelse(group == "e" & condition == "solo", "red",
                             ifelse(group == "e" & condition == "co", "blue",
                                    ifelse(group == "e" & condition == "comp", "green",
                                           ifelse(group == "c" & condition == "solo", "yellow",
                                                  ifelse(group == "c" & condition == "co", "orange",
                                                         ifelse(group == "c" & condition == "comp", "purple",NA)))))))


combined_ArousaldfmeanPlace <- combined_ArousaldfmeanPlace %>%
  mutate(GroupColor = ifelse(group == "e" & condition == "solo", "red",
                             ifelse(group == "e" & condition == "co", "blue",
                                    ifelse(group == "e" & condition == "comp", "green",
                                           ifelse(group == "c" & condition == "solo", "yellow",
                                                  ifelse(group == "c" & condition == "co", "orange",
                                                         ifelse(group == "c" & condition == "comp", "purple",NA)))))))


#plot(combined_Arousaldf$TimeEpoch,combined_Arousaldf$MeanPercent, col = as.factor(combined_Arousaldf$condition))

 plot(combined_Arousaldfmean$TimeEpoch,combined_Arousaldfmean$MeanPercent, col = as.factor(combined_Arousaldfmean$condition))
 plot(combined_Arousaldfmean$TimeEpoch,combined_Arousaldfmean$MeanPercent, col = combined_Arousaldfmean$GroupColor)
 
 plot(combined_ArousaldfmeanPlace$TimeEpoch,combined_ArousaldfmeanPlace$MeanPercent, col = combined_ArousaldfmeanPlace$GroupColor)
 
 
 combined_ArousaldfmeanPlace <- combined_ArousaldfPlace  %>%
   group_by(group, TimeEpoch) %>%
   summarize(
     MeanPercent = mean(MeanPercent),
     MeanPupilSize = mean(MeanPupilSize))
 
 plot(combined_ArousaldfmeanPlace$TimeEpoch,combined_ArousaldfmeanPlace$MeanPercent, col = as.factor(combined_ArousaldfmeanPlace$group))
 
 
 
 combined_Arousaldfmean <- combined_Arousaldf  %>%
   group_by(group, TimeEpoch) %>%
   summarize(
     MeanPercent = mean(MeanPercent),
     MeanPupilSize = mean(MeanPupilSize))
 
 plot(combined_Arousaldfmean$TimeEpoch,combined_Arousaldfmean$MeanPercent, col = as.factor(combined_Arousaldfmean$group))
 
 
 

