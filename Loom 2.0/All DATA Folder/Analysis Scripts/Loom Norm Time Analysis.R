# Load necessary libraries
library(dplyr)
library(ggplot2)
library(bit64)
library(signal)  # For Butterworth dplyr::filter
library(pracma)  # For rad2deg function



setwd("C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Processed Data")

timeNormalizedDF <- data.frame()
filePath <- "C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/SICK DATA FRAMES/Vel_and_Accel_df_6-10.csv"
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
    filt_df <- filt_df %>%
      arrange(Time) %>%
      mutate(
        delta_t = ModTime - lag(ModTime),
        v_x = (HandPos_X - lag(HandPos_X)) / delta_t,
        v_y = (HandPos_Y - lag(HandPos_Y)) / delta_t,
        v_z = (HandPos_Z - lag(HandPos_Z)) / delta_t,
        hand_velocity = sqrt(v_x^2 + v_y^2 + v_z^2)  # Magnitude of velocity
      )
    
    filt_df <- filt_df %>%
      # group_by(Condition, Trial) %>%
      mutate(
        v_x_smooth = filtfilt(bf, v_x),
        v_y_smooth = filtfilt(bf, v_y),
        v_z_smooth = filtfilt(bf, v_z),
        hand_velocity_smooth = filtfilt(bf, hand_velocity)  # Smoothed velocity magnitude
      )
    
    
    
    filt_df <- filt_df %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      mutate(
        a_x = (v_x_smooth - lag(v_x_smooth)) / delta_t,
        a_y = (v_y_smooth - lag(v_y_smooth)) / delta_t,
        a_z = (v_z_smooth - lag(v_z_smooth)) / delta_t,
        acceleration = sqrt(a_x^2 + a_y^2 + a_z^2)  # Magnitude of acceleration
      )
    filt_df <- filt_df %>%
      # group_by(Condition, Trial) %>%
      mutate(
        a_x_smooth = filtfilt(bf, a_x),
        a_y_smooth = filtfilt(bf, a_x),
        a_z_smooth = filtfilt(bf, a_x),
        acceleration_smooth = filtfilt(bf, acceleration)  # Smoothed velocity magnitude
      )

    # Calculate Hand Rotation Velocity and Acceleration --------------------------------------------------------------------------
    
    filt_df <- filt_df %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      # Ensure correct order
      mutate(
        handDt = ModTime - lag(ModTime),  # Time difference
        
        handDx = xHandRot - lag(xHandRot),  # Change in X
        handDy = yHandRot - lag(yHandRot),  # Change in Y
        handDz = zHandRot - lag(zHandRot),  # Change in Z
        
        # Handle angle wraparound (-180 to 180 case)
        handDx = ifelse(handDx > 180, handDx - 360, ifelse(handDx < -180, handDx + 360, handDx)),
        handDy = ifelse(handDy > 180, handDy - 360, ifelse(handDy < -180, handDy + 360, handDy)),
        handDz = ifelse(handDz > 180, handDz - 360, ifelse(handDz < -180, handDz + 360, handDz)),
        
        # Compute angular velocity (deg/s)
        hand_rot_velocity_x = handDx / handDt,
        hand_rot_velocity_y = handDy / handDt,
        hand_rot_velocity_z = handDz / handDt,
        
        # Compute total angular velocity magnitude
        hand_rot_velocity = sqrt(hand_rot_velocity_x^2 + hand_rot_velocity_y^2 + hand_rot_velocity_z^2)
      )
    filt_df <- filt_df %>%
      mutate(hand_rot_velocity_x = ifelse(is.na(hand_rot_velocity_x), 0, hand_rot_velocity_x),
             hand_rot_velocity_y = ifelse(is.na(hand_rot_velocity_y), 0, hand_rot_velocity_y),
             hand_rot_velocity_z = ifelse(is.na(hand_rot_velocity_z), 0, hand_rot_velocity_z),
             hand_rot_velocity = ifelse(is.na(hand_rot_velocity), 0, hand_rot_velocity))
    
    filt_df <- filt_df %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      mutate(
        hand_rot_smooth_x = filtfilt(bf, hand_rot_velocity_x),
        hand_rot_smooth_y = filtfilt(bf, hand_rot_velocity_y),
        hand_rot_smooth_z = filtfilt(bf, hand_rot_velocity_z),
        hand_rot_Vel_smooth = filtfilt(bf, hand_rot_velocity)  # Smoothed velocity magnitude
      )
    
    
    
    filt_df <- filt_df %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      mutate(
        hand_a_x = (hand_rot_smooth_x - lag(hand_rot_smooth_x)) / delta_t,
        hand_a_y = (hand_rot_smooth_y - lag(hand_rot_smooth_y)) / delta_t,
        hand_a_z = (hand_rot_smooth_z - lag(hand_rot_smooth_z)) / delta_t,
        hand_ROT_acceleration = sqrt(hand_a_x^2 + hand_a_y^2 + hand_a_z^2)  # Magnitude of acceleration
      )
    filt_df <- filt_df %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      mutate(
        hand_a_x_smooth = filtfilt(bf, hand_a_x),
        hand_a_y_smooth = filtfilt(bf, hand_a_y),
        hand_a_z_smooth = filtfilt(bf, hand_a_z),
        hand_ROT_acceleration_smooth = filtfilt(bf, hand_ROT_acceleration)  # Smoothed velocity magnitude
      )

    # Head Rotation Velocity --------------------------------------------------------------------
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
    
    
    
    filt_df <- filt_df %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      mutate(
        head_a_x = (head_rot_smooth_x - lag(head_rot_smooth_x)) / dt,
        head_a_y = (head_rot_smooth_y - lag(head_rot_smooth_y)) / dt,
        head_a_z = (head_rot_smooth_z - lag(head_rot_smooth_z)) / dt,
        head_ROT_acceleration = sqrt(head_a_x^2 + head_a_y^2 + head_a_z^2)  # Magnitude of acceleration
      )
    filt_df <- filt_df %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      mutate(
        hand_a_x_smooth = filtfilt(bf, head_a_x),
        hand_a_y_smooth = filtfilt(bf, head_a_y),
        hand_a_z_smooth = filtfilt(bf, head_a_z),
        head_ROT_acceleration_smooth = filtfilt(bf, head_ROT_acceleration)  # Smoothed velocity magnitude
      )
    
    
    # Gaze Velocity and Accel--------------------------------------------------------------------
    
    filt_df <- filt_df %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      mutate(
        gazeDirX = EyePos_X - HeadPos_X,
        gazeDirY = EyePos_Y - HeadPos_Y,
        gazeDirZ = EyePos_Z - HeadPos_Z,
        # Normalize current and previous vectors
        dir_mag = sqrt(gazeDirX^2 + gazeDirY^2 + gazeDirZ^2),
        
        normX = gazeDirX / dir_mag,
        normY = gazeDirY / dir_mag,
        normZ = gazeDirZ / dir_mag,
        
        # Compute dot product using normalized vectors
        dotProd = (lag(normX) * normX) + (lag(normY) * normY) + (lag(normZ) * normZ),
        
        # Compute angle in radians and convert to degrees
        theta_radians = acos(dotProd),
        degrees = rad2deg(theta_radians)
      )
    
    filt_df <- filt_df %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      mutate(
        delta_t = ModTime - lag(ModTime),
        gaze_velocity = degrees / delta_t,
      )
    
    filt_df <- filt_df %>% dplyr::filter(gaze_velocity < 800)
    filt_df <- filt_df %>% mutate(gaze_velocity_filt_bf =  filtfilt(bf, gaze_velocity))
    #filt_df$gaze_velocity_filt <- runmed(filt_df$gaze_velocity, k = 7, endrule = "keep")
    
    filt_df <- filt_df %>%
      arrange(Time) %>%  # Ensure order
      mutate(
        gaze_vel_dt = ModTime - lag(ModTime),  # Time difference
        gaze_vel_dv = gaze_velocity_filt_bf - lag(gaze_velocity_filt_bf),        # Change in velocity
        gaze_accel =  abs(gaze_vel_dv / gaze_vel_dt)         # Acceleration (°/s²)
      )
    filt_df <- filt_df %>% mutate(gaze_accel_filt_bf =  filtfilt(bf, gaze_accel))
    #filt_df$gaze_accel_filt <- runmed(filt_df$gaze_accel, k = 7, endrule = "keep")
    
    

    timeNormalizedDF <- rbind(timeNormalizedDF, filt_df)
  #   
  #   
  #   
   }
  
  
}

df_binned <- timeNormalizedDF %>%
  mutate(TimeBin = cut(NormalizedTime, breaks = seq(0, 100, by = 2), include.lowest = TRUE)) %>%
  group_by(Group, TimeBin) %>%
  summarize(
    AvgVelocity = mean(head_ROT_acceleration_smooth, na.rm = TRUE),
    SDVelocity = sd(head_ROT_acceleration_smooth, na.rm = TRUE),
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
    title = "Gaze Velocity Over Binned Normalized Time (2% Intervals)",
    x = "Normalized Time (%)",
    y = "Velocity (degrees/s)"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())