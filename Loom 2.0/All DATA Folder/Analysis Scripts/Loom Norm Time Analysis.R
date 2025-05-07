# Load necessary libraries
library(dplyr)
library(ggplot2)
library(bit64)
library(signal)  # For Butterworth dplyr::filter


setwd("C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Processed Data")

timeNormalizedDF <- data.frame()
filePath <- "C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/SICK DATA FRAMES/Vel_and_Accel_df.csv"
grab2DropDF <- read.csv(filePath, colClasses=c("startTime" = "integer64", "endTime" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)

#data_files <- list.files(pattern = "nuP15(\\D|$)")
data_files <- list.files(pattern = ".csv")

for(f in 1:length(data_files))
{
  
  participantDataFile <- data_files[f]
  print(participantDataFile)
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  grab2DropDF_trim <- grab2DropDF %>% dplyr::filter(Participant == df$Participant[1])
  
  for (j in 1:nrow(grab2DropDF_trim)) {
    filt_df <- df %>% dplyr::filter(Time >= grab2DropDF_trim$startTime[j] & Time <= grab2DropDF_trim$endTime[j])
    filt_df <- filt_df %>% mutate(NormalizedTime = (Time - min(Time)) / (max(Time) - min(Time)) * 100) 

    
    #FILTER TIME--------------------------------------------------
    
    fs <- 90   # Sampling frequency (Hz)
    fc <- 6    # Cutoff frequency (Hz)
    n <- 2     # 2nd-order dplyr::filter (applied twice = 4th order total)
    bf <- butter(n, fc / (fs / 2), type = "low")
    
    # Calculate Hand Velocity  and Acceleration --------------------------------------------------------------------------
    # filt_df <- filt_df %>%
    #   arrange(Time) %>%
    #   mutate(
    #     delta_t = ModTime - lag(ModTime),
    #     v_x = (HandPos_X - lag(HandPos_X)) / delta_t,
    #     v_y = (HandPos_Y - lag(HandPos_Y)) / delta_t,
    #     v_z = (HandPos_Z - lag(HandPos_Z)) / delta_t,
    #     hand_velocity = sqrt(v_x^2 + v_y^2 + v_z^2)  # Magnitude of velocity
    #   )
    # 
    # filt_df <- filt_df %>%
    #   # group_by(Condition, Trial) %>%
    #   mutate(
    #     v_x_smooth = filtfilt(bf, v_x),
    #     v_y_smooth = filtfilt(bf, v_y),
    #     v_z_smooth = filtfilt(bf, v_z),
    #     hand_velocity_smooth = filtfilt(bf, hand_velocity)  # Smoothed velocity magnitude
    #   )
    # 
    # 
    # 
    # filt_df <- filt_df %>%
    #   group_by(Condition, Trial) %>%
    #   arrange(Time) %>%
    #   mutate(
    #     a_x = (v_x_smooth - lag(v_x_smooth)) / delta_t,
    #     a_y = (v_y_smooth - lag(v_y_smooth)) / delta_t,
    #     a_z = (v_z_smooth - lag(v_z_smooth)) / delta_t,
    #     acceleration = sqrt(a_x^2 + a_y^2 + a_z^2)  # Magnitude of acceleration
    #   )
    # filt_df <- filt_df %>%
    #   # group_by(Condition, Trial) %>%
    #   mutate(
    #     a_x_smooth = filtfilt(bf, a_x),
    #     a_y_smooth = filtfilt(bf, a_x),
    #     a_z_smooth = filtfilt(bf, a_x),
    #     acceleration_smooth = filtfilt(bf, acceleration)  # Smoothed velocity magnitude
    #   )
    # 
    filt_df <- filt_df %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      # Ensure correct order
      mutate(
        dt = ModTime - lag(ModTime),  # Time difference
        
        dx = xHeadRot - lag(xHeadRot),  # Change in X
        dy = yHeadRot - lag(yHeadRot),  # Change in Y
        dz = zHeadRot - lag(zHeadRot),  # Change in Z
        
        # Handle angle wraparound (-180 to 180 case)
        dx = ifelse(dx > 180, dx - 360, ifelse(dx < -180, dx + 360, dx)),
        dy = ifelse(dy > 180, dy - 360, ifelse(dy < -180, dy + 360, dy)),
        dz = ifelse(dz > 180, dz - 360, ifelse(dz < -180, dz + 360, dz)),
        
        # Compute angular velocity (deg/s)
        head_rot_velocity_x = dx / dt,
        head_rot_velocity_y = dy / dt,
        head_rot_velocity_z = dz / dt,
        
        # Compute total angular velocity magnitude
        head_rot_velocity = sqrt(head_rot_velocity_x^2 + head_rot_velocity_y^2 + head_rot_velocity_z^2)
      )
    filt_df <- filt_df %>%
      mutate(head_rot_velocity_x = ifelse(is.na(head_rot_velocity_x), 0, head_rot_velocity_x),
             head_rot_velocity_y = ifelse(is.na(head_rot_velocity_y), 0, head_rot_velocity_y),
             head_rot_velocity_z = ifelse(is.na(head_rot_velocity_z), 0, head_rot_velocity_z),
             head_rot_velocity = ifelse(is.na(head_rot_velocity), 0, head_rot_velocity))
    
    
    filt_df <- filt_df %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      mutate(
        head_rot_smooth_x = filtfilt(bf, head_rot_velocity_x),
        head_rot_smooth_y = filtfilt(bf, head_rot_velocity_y),
        head_rot_smooth_z = filtfilt(bf, head_rot_velocity_z),
        head_rot_Vel_smooth = filtfilt(bf, head_rot_velocity)  # Smoothed velocity magnitude
      )
    
    timeNormalizedDF <- rbind(timeNormalizedDF, filt_df)
    
    
    
  }
  
  
}

df_binned <- timeNormalizedDF %>%
  mutate(TimeBin = cut(NormalizedTime, breaks = seq(0, 100, by = 2), include.lowest = TRUE)) %>%
  group_by(Group, TimeBin) %>%
  summarize(
    AvgVelocity = mean(head_rot_Vel_smooth, na.rm = TRUE),
    SDVelocity = sd(head_rot_Vel_smooth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    TimeBin = as.numeric(sub("\\((.+),.*", "\\1", TimeBin)) + 1 # Correctly converting bin labels to numeric
  )

df_binned$TimeBin[is.na(df_binned$TimeBin)] <- 0

# Plotting the Average Velocity with Shaded SD Area
ggplot(df_binned, aes(x = TimeBin, y = AvgVelocity, group = Group, linetype = Group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = AvgVelocity - SDVelocity, ymax = AvgVelocity + SDVelocity, fill = Group), alpha = 0.2) +
  labs(
    title = "Hand Velocity Over Binned Normalized Time (2% Intervals)",
    x = "Normalized Time (%)",
    y = "Velocity (m/s)"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())