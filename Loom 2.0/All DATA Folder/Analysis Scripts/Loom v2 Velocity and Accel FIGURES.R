# Load required packages
library(dplyr)
library(ggplot2)
library(signal)  # For Butterworth dplyr::filter
library(plotly)
library(bit64)
library(pracma)  # For rad2deg function



moveDF <- data.frame(Participant = factor(),
                          Condition = factor(),
                          Trial = numeric(),
                          Group = factor(),
                          Duration_Time = numeric(),
                          avgVel = numeric(),
                          peak_vel = numeric(),
                          avg_accel = numeric(),
                          peak_accel = numeric(),
                          stringsAsFactors = FALSE)



data_files <- list.files(pattern = "nuP24(\\D|$)")
data_files <- list.files(pattern = ".csv")

for(f in 1:length(data_files))
{
  Grab2DropDF <- data.frame(Participant = factor(),
                            Condition = factor(),
                            Trial = numeric(),
                            Group = factor(),
                            StartTime = factor(),
                            EndTime = numeric(),
                            stringsAsFactors = FALSE)
  
  
  participantDataFile <- data_files[f]
  print(participantDataFile)
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  
  output_file <- "C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Gaze Merged Data/sdP15_Gaze_Events.csv"  # Change this to your desired output file
  
  trimDF <- df %>%  dplyr::filter(Phase != "none")
  
  Participant <- ""
  Condition <- ""
  Trial <- ""
  Group <- ""
  StartTime <- 0
  EndTime <- 0
  
  temp <- nrow(trimDF) -2
  for (i in 1:temp){
    if(trimDF$Phase[i] == "Phase 3"){
      if(trimDF$Phase[i+1] == "Phase 4" & trimDF$Phase[i+2] == "Phase 5"){
        Participant <- trimDF$Participant[i]
        Condition <- trimDF$Condition[i]
        Trial <- trimDF$Trial[i]
        Group <- trimDF$Group[i]
        
        StartTime <- trimDF$Time[i]
        EndTime <- trimDF$Time[i+2]
        newPartRow <- data.frame(Participant, Condition, Trial, Group, StartTime, EndTime)
        Grab2DropDF <- rbind(Grab2DropDF, newPartRow)
        
      }
    }
    
  }
  
  
  
  
  for (j in 1:nrow(Grab2DropDF)) {
    filtDF <- df %>% dplyr::filter(Time >= Grab2DropDF$StartTime[j] & Time <= Grab2DropDF$EndTime[j])
    Participant <- Grab2DropDF$Participant[j]
    Condition <- Grab2DropDF$Condition[j]
    Trial <- Grab2DropDF$Trial[j]
    Group <- Grab2DropDF$Group[j]
    
    Duration_Time <- (Grab2DropDF$EndTime[j] - Grab2DropDF$StartTime[j])/10000
    print(Duration_Time)
    #dplyr::filter TIME--------------------------------------------------
    
    fs <- 90   # Sampling frequency (Hz)
    fc <- 6    # Cutoff frequency (Hz)
    n <- 2     # 2nd-order dplyr::filter (applied twice = 4th order total)
    bf <- butter(n, fc / (fs / 2), type = "low")
    
    # Calculate Hand Velocity  and Acceleration --------------------------------------------------------------------------
    filtDF <- filtDF %>%
      arrange(Time) %>%
      mutate(
        delta_t = ModTime - lag(ModTime),
        v_x = (HandPos_X - lag(HandPos_X)) / delta_t,
        v_y = (HandPos_Y - lag(HandPos_Y)) / delta_t,
        v_z = (HandPos_Z - lag(HandPos_Z)) / delta_t,
        hand_velocity = sqrt(v_x^2 + v_y^2 + v_z^2)  # Magnitude of velocity
      )
    
    filtDF <- filtDF %>%
      # group_by(Condition, Trial) %>%
      mutate(
        v_x_smooth = filtfilt(bf, v_x),
        v_y_smooth = filtfilt(bf, v_y),
        v_z_smooth = filtfilt(bf, v_z),
        hand_velocity_smooth = filtfilt(bf, hand_velocity)  # Smoothed velocity magnitude
      )
    
    avgVel <- mean(filtDF$hand_velocity_smooth)
    peak_vel <- max(filtDF$hand_velocity_smooth)
    
    filtDF <- filtDF %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      mutate(
        a_x = (v_x_smooth - lag(v_x_smooth)) / delta_t,
        a_y = (v_y_smooth - lag(v_y_smooth)) / delta_t,
        a_z = (v_z_smooth - lag(v_z_smooth)) / delta_t,
        acceleration = sqrt(a_x^2 + a_y^2 + a_z^2)  # Magnitude of acceleration
      )
    filtDF <- filtDF %>%
      # group_by(Condition, Trial) %>%
      mutate(
        a_x_smooth = filtfilt(bf, a_x),
        a_y_smooth = filtfilt(bf, a_x),
        a_z_smooth = filtfilt(bf, a_x),
        acceleration_smooth = filtfilt(bf, acceleration)  # Smoothed velocity magnitude
      )
    
    avg_accel <- mean(filtDF$acceleration_smooth)
    peak_accel <-max(filtDF$acceleration_smooth)
    print(Duration_Time)
    
    newPartRow <- data.frame(Participant, Condition, Trial, Group, Duration_Time, avgVel, peak_vel, avg_accel, peak_accel)
    moveDF <- rbind(moveDF, newPartRow)
    
  }
  
  
  
}
moveDF <- moveDF %>% mutate(Group = ifelse(Group == "c","Non-Aut",Group))
moveDF <- moveDF %>% mutate(Group = ifelse(Group == "e","Aut",Group))


moveDF$Condition <- as.factor(moveDF$Condition)
moveDF$Group <- as.factor(moveDF$Group)
# Create the box plot
ggplot(moveDF, aes(x = Condition, y = Duration_Time, fill = Group)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.6) +
  labs(x = "Category", y = "Value", 
       title = "Box Plot of Values by Category and Group") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightcoral"), name = "Group") +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))




ggplot(moveDF, aes(x = Condition, y = Duration_Time, fill = Group)) +
  geom_boxplot() +
  #geom_jitter(position = position_jitter(width = 0.2), size = 1, alpha = 0.6) +
  labs(x = "Condition", y = "Duration (ms)",
       title = "Movement Duration")+
  #subtitle = "Comparing two groups across five categories") +
  scale_fill_manual(values = c("skyblue", "salmon"), name = "Group") +
  theme_minimal(base_size = 15) + 
  ylim(0, 2500)+
  # Use a minimal theme with a larger base font size
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "top") # Move legend to the top


ggplot(moveDF, aes(x = Condition, y = avgVel, fill = Group)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = NA) +
  #geom_jitter(position = position_jitter(width = 0.2), size = 1, alpha = 0.6) +
  labs(x = "Condition", y = " (m/s)",
       title = "Avg Velocity of Hand: Grab to Place")+
  #subtitle = "Comparing two groups across five categories") +
  scale_fill_manual(values = c("skyblue", "salmon"), name = "Group") +
  theme_minimal(base_size = 15) + 
  ylim(0, 1)+
  # Use a minimal theme with a larger base font size
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "top") # Move legend to the top

ggplot(moveDF, aes(x = Condition, y = peak_vel, fill = Group)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = NA) +
  #geom_jitter(position = position_jitter(width = 0.2), size = 1, alpha = 0.6) +
  labs(x = "Condition", y = "speed",
       title = "Peak Velocity of Hand: Grab to Place")+
  #subtitle = "Comparing two groups across five categories") +
  scale_fill_manual(values = c("skyblue", "salmon"), name = "Group") +
  theme_minimal(base_size = 15) + 
  ylim(0, 4)+
  # Use a minimal theme with a larger base font size
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "top") # Move legend to the top

ggplot(moveDF, aes(x = Condition, y = avg_accel, fill = Group)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = NA) +
  #geom_jitter(position = position_jitter(width = 0.2), size = 1, alpha = 0.6) +
  labs(x = "Condition", y = "speed",
       title = "Avg Accel of Hand: Grab to Place")+
  #subtitle = "Comparing two groups across five categories") +
  scale_fill_manual(values = c("skyblue", "salmon"), name = "Group") +
  theme_minimal(base_size = 15) + 
  ylim(0, 5)+
  # Use a minimal theme with a larger base font size
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "top") # Move legend to the top

ggplot(moveDF, aes(x = Condition, y = peak_accel, fill = Group)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = NA) +
  #geom_jitter(position = position_jitter(width = 0.2), size = 1, alpha = 0.6) +
  labs(x = "Condition", y = "speed",
       title = "Peak Accel of Hand: Grab to Place")+
  #subtitle = "Comparing two groups across five categories") +
  scale_fill_manual(values = c("skyblue", "salmon"), name = "Group") +
  theme_minimal(base_size = 15) + 
  ylim(0, 10)+
  # Use a minimal theme with a larger base font size
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "top") # Move legend to the top

