# This scrip is designed to calculate the arousal for timeing around grabing and droping
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(bit64)

setwd("C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Processed Data")



#data_files <- list.files(pattern = "nuP15(\\D|$)")
data_files <- list.files(pattern = ".csv")
combined_df_grab <- data.frame()
combined_df_drop <- data.frame()

for(f in 1:length(data_files))
{
  # grab_df <- data.frame(Participant = factor(),
  #                       Condition = factor(),
  #                       Trial = numeric(),
  #                       Group = factor(),
  #                       StartTime = factor(),
  #                       EndTime = numeric(),
  #                       stringsAsFactors = FALSE)
  
  
  participantDataFile <- data_files[f]
  print(participantDataFile)
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  
  
  #  output_file <- "C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Gaze Merged Data/sdP15_Gaze_Events.csv"  # Change this to your desired output file
  trimDF <- df %>% mutate(Phase = ifelse(ActionEvent == "DropStart","Place Start", Phase))
  
  trimDF <- trimDF %>%  dplyr::filter(Phase != "none")
  trimDF <- trimDF %>% filter(LeftPupil != -1 & RightPupil != -1)
  
  
  #Get the movement Times------------------------------------------------------------------------------------------
  grab_df <- trimDF %>% filter(Phase == "Phase 3")
  drop_df <- trimDF %>% filter(Phase == "Phase 5")
  
  # temp <- nrow(trimDF) -2
  # for (i in 1:temp){
  #   if(trimDF$Phase[i] == "Phase 2"){
  #     if(trimDF$Phase[i+1] == "Phase 3"){
  #       Participant <- trimDF$Participant[i]
  #       Condition <- trimDF$Condition[i]
  #       Trial <- trimDF$Trial[i]
  #       Group <- trimDF$Group[i]
  #       
  #       StartTime <- trimDF$Time[i]
  #       EndTime <- trimDF$Time[i+1]
  #       newPartRow <- data.frame(Participant, Condition, Trial, Group, StartTime, EndTime)
  #       grab_df <- rbind(grab_df, newPartRow)
  #       
  #     }
  #   }
  # }
  
  #Calculate the baseline----------------------------------------------------------------------------------------
  # df_baseLine <- data.frame()
  # 
  # for (i in 1:nrow(grab_df)){
  #   index <- which(df$Time == grab_df$StartTime[i])
  #   if (length(index) > 0) {
  #     # Determine the row range (2 rows before, 2 rows after)
  #     row_range <- max(1, index - 2):min(nrow(df), index + 2)
  #     # Append these rows to the result DataFrame
  #     df_baseLine <- rbind(df_baseLine, df[row_range, ])
  #   }
  # } 
  # 
  # df_baseLine <- df_baseLine %>%
  #   mutate(pupilAverage = (LeftPupil + LeftPupil) / 2)
  # 
  # 
  # df_baseLine_solo <- df_baseLine %>%
  #   filter(Condition == "solo")
  # pupil_Baseline_Solo <- mean(df_baseLine_solo$pupilAverage)
  # 
  # df_baseLine_co <- df_baseLine %>%
  #   filter(Condition == "co")
  # pupil_Baseline_Co <- mean(df_baseLine_co$pupilAverage)
  # 
  # df_baseLine_comp <- df_baseLine %>%
  #   filter(Condition == "comp")
  # pupil_Baseline_Comp <- mean(df_baseLine_comp$pupilAverage)
  # 
  # 
  # 
  # print(pupil_Baseline)
  # 
  
  
  for(i in 1:nrow(grab_df))
  {
    
    filtDF <- df %>% dplyr::filter(Time >= grab_df$Time[i] & Time <= grab_df$Time[i]+ 10000000)
    
    
    # for(i in 1:nrow(filtDF))
    # {
    # 
    # }
    
    
    # currentTime <- filtDF[i,1]
    # currentGazeArea <- filtDF[i,12]
    # currentEvent <- filtDF[i,10]
    # currentArousal <- (filtDF[i,55] + filtDF[i,56])/2
    
    
    
    # subDFpre <- df %>% filter(Time > (currentTime-20000000) & Time <= currentTime)
    # subDFpost <- df %>% filter(Time < (currentTime+20000000) & Time >= currentTime)
    # subDFFull <- df %>% filter(Time < (currentTime+20000000) & Time > (currentTime-20000000))
    # subDFFull <- subDFFull %>% filter(LeftPupil != -1 & RightPupil != -1)
    #
    # baseline <- (mean(subDFpre$RightPupil) + mean(subDFpre$LeftPupil))/2
    
    # subDFpre <- df %>% filter(Time > (currentTime-10000000) & Time <= currentTime)
    
    
    subDFFull <- df %>% dplyr::filter(Time >= grab_df$Time[i] & Time <= grab_df$Time[i]+ 10000000)
    subDFFull$PercentChange <- 0
    
    
    subDFFull <- subDFFull %>% filter(LeftPupil != -1)
    subDFFull <- subDFFull %>%
      mutate(pupilAverage = (LeftPupil + LeftPupil) / 2)
    
    currentTime <- subDFFull$Time[1]
    
    
    df_baseLine <- data.frame()
    index <- which(df$Time == grab_df$Time[i])
    if (length(index) > 0) {
      # Determine the row range (2 rows before, 2 rows after)
      row_range <- max(1, index - 2):min(nrow(df), index + 2)
      # Append these rows to the result DataFrame
      df_baseLine <- rbind(df_baseLine, df[row_range, ])
    }
    
    df_baseLine <- df_baseLine %>%
      mutate(pupilAverage = (LeftPupil + LeftPupil) / 2)
    
    
    baseline <- mean(df_baseLine$pupilAverage)
    
    
    
    #firstArousal <- (subDFFull[1,55] + subDFFull[1,56])/2
    #print((subDFFull$Time[nrow(subDFFull)]  - subDFFull$Time[1])/10000000)
    #print(range($))
    
    if(nrow(subDFFull) != 0){
      
      df_baseLine <- df_baseLine %>%
        mutate(pupilAverage = (LeftPupil + LeftPupil) / 2)
      
      
      baseline <- mean(df_baseLine$pupilAverage)
      
      subDFFull <- subDFFull %>%
        mutate(PercentChange = (pupilAverage - baseline) / baseline * 100)
      #   mutate(pupilAverage = (LeftPupil + RightPupil) / 2)
      # 
      # subDFFull <- subDFFull %>%
      #   mutate(PercentChange = ifelse(Condition == "solo", 
      #                                 (pupilAverage - pupil_Baseline_Solo) / pupil_Baseline_Solo * 100, 
      #                                 PercentChange))
      # 
      # subDFFull <- subDFFull %>%
      #   mutate(PercentChange = ifelse(Condition == "co", 
      #                                 (pupilAverage - pupil_Baseline_Co) / pupil_Baseline_Co * 100, 
      #                                 PercentChange))
      # 
      # subDFFull <- subDFFull %>%
      #   mutate(PercentChange = ifelse(Condition == "comp", 
      #                                 (pupilAverage - pupil_Baseline_Comp) / pupil_Baseline_Comp * 100, 
      #                                 PercentChange))
      
      
      subDFFull <- subDFFull %>%
        mutate(TimeEpoch = round((Time/10000000 - currentTime/10000000),1))
      
      #group_mean <- aggregate(subDFFull$PercentChange, list(subDFFull$TimeEpoch), mean)
      
      group_mean <- subDFFull  %>%
        group_by(Participant, Group, Condition, TimeEpoch) %>%
        summarize(
          MeanPercentChange = mean(PercentChange),
          Meanpupil = mean(pupilAverage))
      
      
      combined_df_grab <- rbind(combined_df_grab, group_mean)
    }
  }
  
  for(i in 1:nrow(drop_df))
  {
    
    filtDF <- df %>% dplyr::filter(Time >= drop_df$Time[i] & Time <= drop_df$Time[i]+ 10000000)
    
    
    # for(i in 1:nrow(filtDF))
    # {
    # 
    # }
    
    
    # currentTime <- filtDF[i,1]
    # currentGazeArea <- filtDF[i,12]
    # currentEvent <- filtDF[i,10]
    # currentArousal <- (filtDF[i,55] + filtDF[i,56])/2
    
    
    
    # subDFpre <- df %>% filter(Time > (currentTime-20000000) & Time <= currentTime)
    # subDFpost <- df %>% filter(Time < (currentTime+20000000) & Time >= currentTime)
    # subDFFull <- df %>% filter(Time < (currentTime+20000000) & Time > (currentTime-20000000))
    # subDFFull <- subDFFull %>% filter(LeftPupil != -1 & RightPupil != -1)
    #
    # baseline <- (mean(subDFpre$RightPupil) + mean(subDFpre$LeftPupil))/2
    
    # subDFpre <- df %>% filter(Time > (currentTime-10000000) & Time <= currentTime)
    
    
    subDFFull <- df %>% dplyr::filter(Time >= drop_df$Time[i] & Time <= drop_df$Time[i]+ 10000000)
    subDFFull$PercentChange <- 0
    
    
    subDFFull <- subDFFull %>% filter(LeftPupil != -1)
    subDFFull <- subDFFull %>%
      mutate(pupilAverage = (LeftPupil + LeftPupil) / 2)
    
    currentTime <- subDFFull$Time[1]
    
    
    df_baseLine <- data.frame()
    index <- which(df$Time == drop_df$Time[i])
    if (length(index) > 0) {
      # Determine the row range (2 rows before, 2 rows after)
      row_range <- max(1, index - 2):min(nrow(df), index + 2)
      # Append these rows to the result DataFrame
      df_baseLine <- rbind(df_baseLine, df[row_range, ])
    }
    
    df_baseLine <- df_baseLine %>%
      mutate(pupilAverage = (LeftPupil + LeftPupil) / 2)
    
    
    baseline <- mean(df_baseLine$pupilAverage)
    
    
    
    #firstArousal <- (subDFFull[1,55] + subDFFull[1,56])/2
    #print((subDFFull$Time[nrow(subDFFull)]  - subDFFull$Time[1])/10000000)
    #print(range($))
    
    if(nrow(subDFFull) != 0){
      
      df_baseLine <- df_baseLine %>%
        mutate(pupilAverage = (LeftPupil + LeftPupil) / 2)
      
      
      baseline <- mean(df_baseLine$pupilAverage)
      
      subDFFull <- subDFFull %>%
        mutate(PercentChange = (pupilAverage - baseline) / baseline * 100)
      #   mutate(pupilAverage = (LeftPupil + RightPupil) / 2)
      # 
      # subDFFull <- subDFFull %>%
      #   mutate(PercentChange = ifelse(Condition == "solo", 
      #                                 (pupilAverage - pupil_Baseline_Solo) / pupil_Baseline_Solo * 100, 
      #                                 PercentChange))
      # 
      # subDFFull <- subDFFull %>%
      #   mutate(PercentChange = ifelse(Condition == "co", 
      #                                 (pupilAverage - pupil_Baseline_Co) / pupil_Baseline_Co * 100, 
      #                                 PercentChange))
      # 
      # subDFFull <- subDFFull %>%
      #   mutate(PercentChange = ifelse(Condition == "comp", 
      #                                 (pupilAverage - pupil_Baseline_Comp) / pupil_Baseline_Comp * 100, 
      #                                 PercentChange))
      
      
      subDFFull <- subDFFull %>%
        mutate(TimeEpoch = round((Time/10000000 - currentTime/10000000),1))
      
      #group_mean <- aggregate(subDFFull$PercentChange, list(subDFFull$TimeEpoch), mean)
      
      group_mean <- subDFFull  %>%
        group_by(Participant, Group, Condition, TimeEpoch) %>%
        summarize(
          MeanPercentChange = mean(PercentChange),
          Meanpupil = mean(pupilAverage))
      
      
      combined_df_drop <- rbind(combined_df_drop, group_mean)
    }
  }
  
  
  
}


#write.csv(combined_df, file = "C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/SICK DATA FRAMES/Cube_PAC.csv", row.names = FALSE)


#Figures ---------------------------------------------------------------------------------------------------------------------------

trim_combined_df <- combined_df_grab %>% filter(Condition != "")
trim_combined_df <- trim_combined_df %>% filter(Group != "")

# trim_combined_df <- trim_combined_df %>% filter(Participant != "nuP10" |Participant != "sdP2" |Participant != "P5" |Participant != "sdP11" |
#                                                   Participant != "nuP19" |Participant != "nuP13" |Participant != "nuP20" |Participant != "nuP15")
# trim_combined_df <- trim_combined_df %>% mutate(Group = ifelse(Participant == "nuP33","e",Group))


trim_combined_df <- trim_combined_df %>% filter(TimeEpoch <= 1)
trim_combined_df <- trim_combined_df %>% filter(MeanPercentChange > -100)



# 1. Compute a mean for each participant first
subject_means <- trim_combined_df %>% 
  group_by(TimeEpoch, Participant, Condition, Group) %>% 
  summarise(participant_mean  = mean(MeanPercentChange, na.rm = TRUE),
            .groups = "drop")

# 2. Now compute SEM (or CI) across participants
summary_df <- subject_means %>% 
  group_by(TimeEpoch, Condition, Group) %>% 
  summarise(
    mean_percent = mean(participant_mean, na.rm = TRUE),
    sd  = sd(participant_mean,  na.rm = TRUE),
    n   = n(),                              # participants, not samples
    sem = sd / sqrt(n),
    ci95 = sem * qt(.975, df = n - 1),      # optional: 95 % CI
    .groups = "drop"
  )


# # Calculate Mean and SEM (Standard Error of the Mean) for each Condition and Time Epoch
# summary_df <- trim_combined_df %>%
#   group_by(TimeEpoch, Condition, Group) %>%
#   summarize(
#     mean_percent = mean(MeanPercentChange, na.rm = TRUE),
#     sem = sd(MeanPercentChange, na.rm = TRUE) / sqrt(n())
#   )

# summary_dfTemp <- trim_combined_df %>%
#   group_by(TimeEpoch, Participant, Condition, Group) %>%
#   dplyr::summarise(
#     mean_percent = mean(MeanPercentChange, na.rm = TRUE)
#   )
# 
# 
# 
# summary_df <- summary_dfTemp %>%
#   group_by(TimeEpoch, Condition, Group) %>%
#   dplyr::summarise(
#     mean_percent = mean(mean_percent, na.rm = TRUE),
#     sem = sd(mean_percent, na.rm = TRUE) / sqrt(n()),
#     .groups = "drop"
#   )
#summary_df <- summary_df %>% filter(Group != "")


summary_df <- summary_df %>% filter(Condition == "co")
#summary_df <- summary_df %>% filter(Group == "c")


# Plotting the Line Plot with Error Bars for Multiple Conditions and Groups
p3 <- ggplot(summary_df, aes(x = TimeEpoch, y = mean_percent, color = Group, linetype = Condition, group = interaction(Group, Condition))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(
    ymin = mean_percent - ci95,
    ymax = mean_percent + ci95,
    fill = Group), alpha = 0.2, color = NA)  +
  labs(
    title = "Compeditive",
    x = "Time (s)",
    #y = "Percentage Change"
    y = ""
  ) +
  theme_minimal(base_size = 15) +
  #ylim(-4.4, 2)+
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )
p3

library(patchwork)

combined_plot <- p1 + p2 + p3
combined_plot


#Stats -------------------------------------------------------------------------------------------------------------------------------


leveneTest(participant_mean ~ Group, data = subject_means)


library(nlme)

model_var <- lme(participant_mean ~ Group * Condition * TimeEpoch,
                 random = ~1 | Participant,
                 weights = varIdent(form = ~1 | Group),
                 data = subject_means)

summary(model_var)


model_equal <- lme(participant_mean ~ Group * Condition * TimeEpoch,
                   random = ~1 | Participant,
                   data = subject_means)

anova(model_equal, model_var) 

# library(lme4)
# library(lmerTest)
# 
# model <- lmer(participant_mean ~ Group * Condition * TimeEpoch + (1 | Participant), data = subject_means)
# summary(model)

emm <- emmeans(model, ~ Group * Condition * TimeEpoch)
pairs(emm, by = c("TimeEpoch", "Condition"))




subset_df <- subject_means %>%
  filter(TimeEpoch == 1)

# Group difference per condition
library(broom)

subset_df %>%
  group_by(Condition) %>%
  do(tidy(t.test(participant_mean ~ Group, data = .)))


last_epoch <- max(subject_means$TimeEpoch)

emm <- emmeans(model, ~ Group | Condition, at = list(TimeEpoch = last_epoch))


combined_df_grab <- combined_df_grab %>% filter(Group != "")

ggplot(combined_df_grab, aes(x = Condition, y = Meanpupil, fill = Group)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = NA) +
  #geom_jitter(position = position_jitter(width = 0.2), size = 1, alpha = 0.6) +
  labs(x = "Condition", y = "Size (mm)",
       title = "Pupil size")+
  #subtitle = "Comparing two groups across five categories") +
  scale_fill_manual(values = c( "salmon", "skyblue"), name = "Group") +
  theme_minimal(base_size = 15) + 
  ylim(0, 10)+
  # Use a minimal theme with a larger base font size
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "top") # Move legend to the top


