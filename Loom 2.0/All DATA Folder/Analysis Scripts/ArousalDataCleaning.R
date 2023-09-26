library(plot3D)
library(rgl)
library(dplyr)
library(bit64)
library(stringr)


data_files <- list.files(pattern = "nuP9_old3")

PACdf <- data.frame(Time = numeric(),
                    Participant = factor(),
                    Condition = factor(),
                    Trial = numeric(),
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
                              PACtype = factor(),
                              PACtime = numeric(),
                              PACstartTime = numeric(),
                              PACendTime = numeric(),
                              Event = factor(),
                              stringsAsFactors = FALSE)


combined_Arousaldf <- data.frame()

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
  combined_df <- data.frame()
  
  for(i in 1:nrow(trimedGrabDF))
  {
    trimedGrabDF <- trimedGrabDF %>% filter(LeftPupil != -1 & RightPupil != -1)
    
    currentTime <- trimedGrabDF[i,1]
    currentGazeArea <- trimedGrabDF[i,12]
    currentEvent <- trimedGrabDF[i,10]
    currentArousal <- (trimedGrabDF[i,55] + trimedGrabDF[i,56])/2
    
    print(currentArousal)
    

    subDFpre <- df %>% filter(Time > (currentTime-20000000) & Time <= currentTime)
    subDFpost <- df %>% filter(Time < (currentTime+20000000) & Time >= currentTime)
    subDFFull <- df %>% filter(Time < (currentTime+20000000) & Time > (currentTime-20000000))
    subDFFull <- subDFFull %>% filter(LeftPupil != -1 & RightPupil != -1)
    
    if(nrow(subDFFull) != 0){
      subDFFull <- subDFFull %>%
        mutate(pupilAverage = (RightPupil + LeftPupil) / 2)
      
      subDFFull <- subDFFull %>%
        mutate(PercentChange = (pupilAverage - currentArousal) / currentArousal * 100)
      
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
  
  #total_group_mean <- aggregate(combined_df$x, list(combined_df$Group.1), mean)
  
  total_group_mean <- combined_df  %>%
    group_by(TimeEpoch) %>%
    summarize(
      MeanPercentChange = mean(MeanPercentChange),
      Meanpupil = mean(Meanpupil))
  
  total_group_mean <- total_group_mean %>%
    mutate(condition = trimedGrabDF$Condition[1])
  total_group_mean <- total_group_mean %>%
    mutate(group = trimedGrabDF$Group[1])
  total_group_mean <- total_group_mean %>%
    mutate(Participant = trimedGrabDF$Participant[1])
  
    
    
  
  combined_Arousaldf <- rbind(combined_Arousaldf, total_group_mean)
  
  
}

#combined_Arousaldf <- aggregate(combined_Arousaldf$x, list(combined_Arousaldf$Group.1), mean)

combined_Arousaldf <- combined_Arousaldf  %>%
       group_by(TimeEpoch, condition, Participant) %>%
       summarize(
             MeanPercent = mean(MeanPercentChange),
             MeanPupilSize = mean(Meanpupil))

plot(combined_Arousaldf$TimeEpoch,combined_Arousaldf$MeanPercent, col = as.factor(combined_Arousaldf$condition))


