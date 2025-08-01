# Load required packages
library(dplyr)
library(ggplot2)
library(signal)  # For Butterworth dplyr::filter
library(plotly)
library(bit64)
library(pracma)  # For rad2deg function
setwd("C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Processed Data")


DropmoveDF <- data.frame(Participant = factor(),
                         Condition = factor(),
                         Trial = numeric(),
                         Group = factor(),
                         startTime = numeric(),
                         endTime = numeric(),
                         cubeColor = factor(),
                         Duration_Time = numeric(),
                         hand_avgVel = numeric(),
                         hand_peak_vel = numeric(),
                         hand_avg_accel = numeric(),
                         hand_peak_accel = numeric(),
                         hand_time2PeakVel = numeric(),
                         hand_time2PeakAccel = numeric(),
                         handRot_avgVel = numeric(),
                         handRot_peak_vel = numeric(),
                         handRot_avg_accel = numeric(),
                         handRot_peak_accel = numeric(),
                         handRot_time2PeakVel = numeric(),
                         handRot_time2PeakAccel = numeric(),
                         headRot_avgVel = numeric(),
                         headRot_peak_vel = numeric(),
                         headRot_avg_accel = numeric(),
                         headRot_peak_accel = numeric(),
                         headRot_time2PeakVel = numeric(),
                         headRot_time2PeakAccel = numeric(),
                         headRot_time2PeakAccel = numeric(),
                         gaze_avgVel = numeric(),
                         gaze_peak_vel = numeric(),
                         gaze_avg_accel = numeric(),
                         gaze_peak_accel = numeric(),
                         gaze_time2PeakVel = numeric(),
                         gaze_time2PeakAccel = numeric(),
                         hand_time2Initiation = numeric(),
                         handRot_time2Initiation = numeric(),
                         headRot_time2Initiation = numeric(),
                         gaze_time2Initiation = numeric(),
                         hand_endVel = numeric(),
                         handRot_endVel = numeric(),
                         headRot_endVel = numeric(),
                         gaze_endVel = numeric(),
                         hand_endAccel = numeric(),
                         handRot_endAccel = numeric(),
                         headRot_endAccel = numeric(),
                         gaze_endAccel = numeric(),
                         stringsAsFactors = FALSE)


#data_files <- list.files(pattern = "nuP15(\\D|$)")
 data_files <- list.files(pattern = ".csv")

for(f in 1:length(data_files))
{
  Grab2DropDF <- data.frame(Participant = factor(),
                            Condition = factor(),
                            Trial = numeric(),
                            Group = factor(),
                            StartTime = factor(),
                            EndTime = numeric(),
                            color = factor(),
                            stringsAsFactors = FALSE)
  
  
  participantDataFile <- data_files[f]
  print(participantDataFile)
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]
  
  
  #  output_file <- "C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Gaze Merged Data/sdP15_Gaze_Events.csv"  # Change this to your desired output file
  trimDF <- df %>% mutate(Phase = ifelse(ActionEvent == "DropStart","Place Start", Phase))
  
  trimDF <- trimDF %>%  dplyr::filter(Phase != "none")
  
  Participant <- ""
  Condition <- ""
  Trial <- ""
  Group <- ""
  StartTime <- 0
  EndTime <- 0
  
  temp <- nrow(trimDF) -1
  for (i in 1:temp){
    if(trimDF$Phase[i] == "Phase 4"){
      if(trimDF$Phase[i+1] == "Phase 5" | trimDF$Phase[i+1] == "Place Start"){
        Participant <- trimDF$Participant[i]
        Condition <- trimDF$Condition[i]
        Trial <- trimDF$Trial[i]
        Group <- trimDF$Group[i]
        
        StartTime <- trimDF$Time[i]
        EndTime <- trimDF$Time[i+1]
        
        color <- trimDF$cubeColor[i]
        if(color == "none"){
          if(grepl("Blue", trimDF$Event[i])){
            color = "Blue"
          }
          if(grepl("Red", trimDF$Event[i])){
            color = "Red"
          }
          if(grepl("Neutral ", trimDF$Event[i])){
            color = "White"
          }
          if(grepl("Gold", trimDF$Event[i])){
            color = "Gold"
          }
          
        }
        
        
        newPartRow <- data.frame(Participant, Condition, Trial, Group, StartTime, EndTime, color)
        Grab2DropDF <- rbind(Grab2DropDF, newPartRow)
        
      }
    }
    
  }
  
  
  g2pData <- data.frame()
  g2pDataFirst <- data.frame()
  
  for (j in 1:nrow(Grab2DropDF)) {
    filtDF <- df %>% dplyr::filter(Time >= Grab2DropDF$StartTime[j] & Time <= Grab2DropDF$EndTime[j])
    
    
    Participant <- Grab2DropDF$Participant[j]
    Condition <- Grab2DropDF$Condition[j]
    Trial <- Grab2DropDF$Trial[j]
    Group <- Grab2DropDF$Group[j]
    cubeColor <- Grab2DropDF$color[j]
    
    
    startTime <- first(filtDF$Time)
    endTime <- last(filtDF$Time)
    
    
    Duration_Time <- (Grab2DropDF$EndTime[j] - Grab2DropDF$StartTime[j])/10000
    
    
    #FILTER TIME--------------------------------------------------
    
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
    
    hand_avgVel <- mean(filtDF$hand_velocity_smooth)
    hand_peak_vel <- max(filtDF$hand_velocity_smooth)
    hand_time2PeakVel <- filtDF$ModTime[which.max(filtDF$hand_velocity_smooth)] - first(filtDF$ModTime)
    
    hand_avg_accel <- mean(filtDF$acceleration_smooth)
    hand_peak_accel <- max(filtDF$acceleration_smooth)
    hand_time2PeakAccel <- filtDF$ModTime[which.max(filtDF$acceleration_smooth)] - first(filtDF$ModTime)
    
    hand_endVel = last(filtDF$hand_velocity_smooth)
    hand_endAccel = last(filtDF$acceleration_smooth)
    
    
    
    # Calculate Hand Rotation Velocity and Acceleration --------------------------------------------------------------------------
    
    filtDF <- filtDF %>%
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
    filtDF <- filtDF %>%
      mutate(hand_rot_velocity_x = ifelse(is.na(hand_rot_velocity_x), 0, hand_rot_velocity_x),
             hand_rot_velocity_y = ifelse(is.na(hand_rot_velocity_y), 0, hand_rot_velocity_y),
             hand_rot_velocity_z = ifelse(is.na(hand_rot_velocity_z), 0, hand_rot_velocity_z),
             hand_rot_velocity = ifelse(is.na(hand_rot_velocity), 0, hand_rot_velocity))
    
    filtDF <- filtDF %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      mutate(
        hand_rot_smooth_x = filtfilt(bf, hand_rot_velocity_x),
        hand_rot_smooth_y = filtfilt(bf, hand_rot_velocity_y),
        hand_rot_smooth_z = filtfilt(bf, hand_rot_velocity_z),
        hand_rot_Vel_smooth = filtfilt(bf, hand_rot_velocity)  # Smoothed velocity magnitude
      )
    
    
    
    filtDF <- filtDF %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      mutate(
        hand_a_x = (hand_rot_smooth_x - lag(hand_rot_smooth_x)) / delta_t,
        hand_a_y = (hand_rot_smooth_y - lag(hand_rot_smooth_y)) / delta_t,
        hand_a_z = (hand_rot_smooth_z - lag(hand_rot_smooth_z)) / delta_t,
        hand_ROT_acceleration = sqrt(hand_a_x^2 + hand_a_y^2 + hand_a_z^2)  # Magnitude of acceleration
      )
    filtDF <- filtDF %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      mutate(
        hand_a_x_smooth = filtfilt(bf, hand_a_x),
        hand_a_y_smooth = filtfilt(bf, hand_a_y),
        hand_a_z_smooth = filtfilt(bf, hand_a_z),
        hand_ROT_acceleration_smooth = filtfilt(bf, hand_ROT_acceleration)  # Smoothed velocity magnitude
      )
    
    handRot_avgVel <- mean(filtDF$hand_rot_Vel_smooth)
    handRot_peak_vel <- max(filtDF$hand_rot_Vel_smooth)
    handRot_time2PeakVel <- filtDF$ModTime[which.max(filtDF$hand_rot_Vel_smooth)] - first(filtDF$ModTime)
    
    handRot_avg_accel <- mean(filtDF$hand_ROT_acceleration_smooth)
    handRot_peak_accel <- max(filtDF$hand_ROT_acceleration_smooth)
    handRot_time2PeakAccel <- filtDF$ModTime[which.max(filtDF$hand_ROT_acceleration_smooth)] - first(filtDF$ModTime)
    
    handRot_endVel = last(filtDF$hand_rot_Vel_smooth)
    handRot_endAccel = last(filtDF$hand_ROT_acceleration_smooth)
    
    
    # Head Rotation Velocity --------------------------------------------------------------------
    filtDF <- filtDF %>%
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
    filtDF <- filtDF %>%
      mutate(head_rot_velocity_x = ifelse(is.na(head_rot_velocity_x), 0, head_rot_velocity_x),
             head_rot_velocity_y = ifelse(is.na(head_rot_velocity_y), 0, head_rot_velocity_y),
             head_rot_velocity_z = ifelse(is.na(head_rot_velocity_z), 0, head_rot_velocity_z),
             head_rot_velocity = ifelse(is.na(head_rot_velocity), 0, head_rot_velocity))
    
    
    filtDF <- filtDF %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      mutate(
        head_rot_smooth_x = filtfilt(bf, head_rot_velocity_x),
        head_rot_smooth_y = filtfilt(bf, head_rot_velocity_y),
        head_rot_smooth_z = filtfilt(bf, head_rot_velocity_z),
        head_rot_Vel_smooth = filtfilt(bf, head_rot_velocity)  # Smoothed velocity magnitude
      )
    
    
    
    filtDF <- filtDF %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      mutate(
        head_a_x = (head_rot_smooth_x - lag(head_rot_smooth_x)) / dt,
        head_a_y = (head_rot_smooth_y - lag(head_rot_smooth_y)) / dt,
        head_a_z = (head_rot_smooth_z - lag(head_rot_smooth_z)) / dt,
        head_ROT_acceleration = sqrt(head_a_x^2 + head_a_y^2 + head_a_z^2)  # Magnitude of acceleration
      )
    filtDF <- filtDF %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      mutate(
        hand_a_x_smooth = filtfilt(bf, head_a_x),
        hand_a_y_smooth = filtfilt(bf, head_a_y),
        hand_a_z_smooth = filtfilt(bf, head_a_z),
        head_ROT_acceleration_smooth = filtfilt(bf, head_ROT_acceleration)  # Smoothed velocity magnitude
      )
    
    
    headRot_avgVel <- mean(filtDF$head_rot_Vel_smooth)
    headRot_peak_vel <- max(filtDF$head_rot_Vel_smooth)
    headRot_time2PeakVel <- filtDF$ModTime[which.max(filtDF$head_rot_Vel_smooth)] - first(filtDF$ModTime)
    
    headRot_avg_accel <- mean(filtDF$head_ROT_acceleration_smooth)
    headRot_peak_accel <- max(filtDF$head_ROT_acceleration_smooth)
    headRot_time2PeakAccel <- filtDF$ModTime[which.max(filtDF$head_ROT_acceleration_smooth)] - first(filtDF$ModTime)
    
    
    
    headRot_endVel = last(filtDF$head_rot_Vel_smooth)
    headRot_endAccel = last(filtDF$head_rot_Vel_smooth)
    
    
    # Gaze Velocity and Accel--------------------------------------------------------------------
    
    filtDF <- filtDF %>%
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
    
    filtDF <- filtDF %>%
      group_by(Condition, Trial) %>%
      arrange(Time) %>%
      mutate(
        delta_t = ModTime - lag(ModTime),
        gaze_velocity = degrees / delta_t,
      )
    
    filtDF <- filtDF %>% dplyr::filter(gaze_velocity < 800)
    filtDF <- filtDF %>% mutate(gaze_velocity_filt_bf =  filtfilt(bf, gaze_velocity))
    #filtDF$gaze_velocity_filt <- runmed(filtDF$gaze_velocity, k = 7, endrule = "keep")
    
    filtDF <- filtDF %>%
      arrange(Time) %>%  # Ensure order
      mutate(
        gaze_vel_dt = ModTime - lag(ModTime),  # Time difference
        gaze_vel_dv = gaze_velocity_filt_bf - lag(gaze_velocity_filt_bf),        # Change in velocity
        gaze_accel =  abs(gaze_vel_dv / gaze_vel_dt)         # Acceleration (°/s²)
      )
    filtDF <- filtDF %>% mutate(gaze_accel_filt_bf =  filtfilt(bf, gaze_accel))
    #filtDF$gaze_accel_filt <- runmed(filtDF$gaze_accel, k = 7, endrule = "keep")
    
    
    gaze_avgVel <- mean(filtDF$gaze_velocity_filt_bf)
    gaze_peak_vel <- max(filtDF$gaze_velocity_filt_bf)
    gaze_time2PeakVel <- filtDF$ModTime[which.max(filtDF$gaze_velocity_filt_bf)] - first(filtDF$ModTime)
    
    gaze_avg_accel <- mean(filtDF$gaze_accel_filt_bf)
    gaze_peak_accel <- max(filtDF$gaze_accel_filt_bf)
    gaze_time2PeakAccel <- filtDF$ModTime[which.max(filtDF$gaze_accel_filt_bf)] - first(filtDF$ModTime)
    
    
    gaze_endVel = last(filtDF$gaze_velocity_filt_bf)
    gaze_endAccel = last(filtDF$gaze_accel_filt_bf)
    
    
    
    
    g2pData <- rbind(g2pData, filtDF)
    g2pDataFirst <- rbind(g2pDataFirst, filtDF[1,])
    
    hand_time2Initiation = 0
    handRot_time2Initiation = 0
    headRot_time2Initiation = 0
    gaze_time2Initiation = 0
    newPartRow <- data.frame(Participant, Condition, Trial, Group, startTime, endTime, cubeColor, Duration_Time, 
                             hand_avgVel, hand_peak_vel, hand_avg_accel, hand_peak_accel, hand_time2PeakVel, hand_time2PeakAccel,
                             handRot_avgVel, handRot_peak_vel, handRot_avg_accel, handRot_peak_accel, handRot_time2PeakVel, handRot_time2PeakAccel,
                             headRot_avgVel, headRot_peak_vel, headRot_avg_accel, headRot_peak_accel, headRot_time2PeakVel, headRot_time2PeakAccel,
                             gaze_avgVel, gaze_peak_vel, gaze_avg_accel, gaze_peak_accel, gaze_time2PeakVel, gaze_time2PeakAccel,
                             hand_time2Initiation, handRot_time2Initiation, headRot_time2Initiation, gaze_time2Initiation,
                             hand_endVel, handRot_endVel, headRot_endVel, gaze_endVel, hand_endAccel, handRot_endAccel,headRot_endAccel, gaze_endAccel )
    DropmoveDF <- rbind(DropmoveDF, newPartRow)
    
  }
  
  
  moveFiltDF <- DropmoveDF %>% dplyr::filter(Participant == df$Participant[5])
  
  sdThreshold <- 3
  handSD <- sdThreshold*sd(g2pDataFirst$hand_velocity_smooth)
  handRotSD <- sdThreshold*sd(g2pDataFirst$hand_rot_Vel_smooth)
  headRotSD <- sdThreshold*sd(g2pDataFirst$head_rot_Vel_smooth)
  gazeSD <- sdThreshold*sd(g2pDataFirst$gaze_velocity_filt_bf)
  
  
  
  for (j in 1:nrow(moveFiltDF)) {
    filtDF_trim <- g2pData %>% dplyr::filter(Time >= moveFiltDF$startTime[j] & Time <= moveFiltDF$endTime[j])
    
    # Step 1: Find the first row where 'values' is greater than 'a'
    handIndex <- which(filtDF_trim$hand_velocity_smooth > handSD)[1]
    handRotIndex <- which(filtDF_trim$hand_rot_Vel_smooth > handRotSD)[1]
    headIndex <- which(filtDF_trim$head_rot_Vel_smooth > headRotSD)[1]
    gazeIndex <- which(filtDF_trim$gaze_velocity_filt_bf > gazeSD)[1]
    
    # print(handIndex)
    # print(handRotIndex)
    # print(headIndex)
    # print(gazeIndex)
    
    
    handTime <- filtDF_trim$ModTime[handIndex]
    handTime <- handTime - filtDF_trim$ModTime[1]
    
    handRotTime <- filtDF_trim$ModTime[handRotIndex]
    handRotTime <- handRotTime - filtDF_trim$ModTime[1]
    
    headTime <- filtDF_trim$ModTime[headIndex]
    headTime <- headTime - filtDF_trim$ModTime[1]
    
    gazeTime <- filtDF_trim$ModTime[gazeIndex]
    gazeTime <- gazeTime - filtDF_trim$ModTime[1]
    
    # for (f in 1:nrow(DropmoveDF)) {
    #   if(DropmoveDF$startTime)
    # }
    
    DropmoveDF <- DropmoveDF %>% mutate(hand_time2Initiation = ifelse(startTime == moveFiltDF$startTime[j],handTime, hand_time2Initiation))
    DropmoveDF <- DropmoveDF %>% mutate(handRot_time2Initiation = ifelse(startTime == moveFiltDF$startTime[j],handRotTime, handRot_time2Initiation))
    DropmoveDF <- DropmoveDF %>% mutate(headRot_time2Initiation = ifelse(startTime == moveFiltDF$startTime[j],headTime, headRot_time2Initiation))
    DropmoveDF <- DropmoveDF %>% mutate(gaze_time2Initiation = ifelse(startTime == moveFiltDF$startTime[j],gazeTime, gaze_time2Initiation))
    
  }
  
  
  
  
}
DropmoveDF <- DropmoveDF %>% mutate(Group = ifelse(Group == "c","Non-Aut",Group))
DropmoveDF <- DropmoveDF %>% mutate(Group = ifelse(Group == "e","Aut",Group))


#write.csv(DropmoveDF, file = "C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/SICK DATA FRAMES/Vel_and_Accel_df_6-10.csv", row.names = FALSE)



#figures----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#figures----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#figures----------------------------------------------------------------------------------------------------------------------------------------------------------------------

plotDF <- df %>% dplyr::filter(Time >= 638338369655881888  & Time <= 638338369656182231)

plotDF <- plotDF %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  # Ensure correct order
  mutate(
    dt = ModTime - lag(ModTime),    
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
plotDF <- plotDF %>%
  mutate(head_rot_velocity_x = ifelse(is.na(head_rot_velocity_x), 0, head_rot_velocity_x),
         head_rot_velocity_y = ifelse(is.na(head_rot_velocity_y), 0, head_rot_velocity_y),
         head_rot_velocity_z = ifelse(is.na(head_rot_velocity_z), 0, head_rot_velocity_z),
         head_rot_velocity = ifelse(is.na(head_rot_velocity), 0, head_rot_velocity))


plotDF <- plotDF %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  mutate(
    head_rot_smooth_x = filtfilt(bf, head_rot_velocity_x),
    head_rot_smooth_y = filtfilt(bf, head_rot_velocity_y),
    head_rot_smooth_z = filtfilt(bf, head_rot_velocity_z),
    head_rot_Vel_smooth = filtfilt(bf, head_rot_velocity)  # Smoothed velocity magnitude
  )



plotDF <- plotDF %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  mutate(
    head_a_x = (head_rot_smooth_x - lag(head_rot_smooth_x)) / dt,
    head_a_y = (head_rot_smooth_y - lag(head_rot_smooth_y)) / dt,
    head_a_z = (head_rot_smooth_z - lag(head_rot_smooth_z)) / dt,
    head_ROT_acceleration = sqrt(head_a_x^2 + head_a_y^2 + head_a_z^2)  # Magnitude of acceleration
  )
plotDF <- plotDF %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  mutate(
    hand_a_x_smooth = filtfilt(bf, head_a_x),
    hand_a_y_smooth = filtfilt(bf, head_a_y),
    hand_a_z_smooth = filtfilt(bf, head_a_z),
    head_ROT_acceleration_smooth = filtfilt(bf, head_ROT_acceleration)  # Smoothed velocity magnitude
  )


ggplot(plotDF, aes(x = ModTime, y = head_rot_Vel_smooth, color = as.factor(CurrentGazeArea))) +
  geom_line(alpha = 0.8) +
  geom_point()+
  # geom_vline(data = df_Hand_Vel, aes(xintercept = grabPoint), color = "black", linetype = "solid", size = 1) +
  # geom_vline(data = df_Hand_Vel, aes(xintercept = dropPoint), color = "black", linetype = "solid", size = 1) +
  # geom_vline(data = df_Hand_Vel, aes(xintercept = arrivePoint), color = "black", linetype = "solid", size = 1) +
  # geom_vline(data = df_Hand_Vel, aes(xintercept = gazeArrivePoint), color = "black", linetype = "solid", size = 1) +
  # facet_wrap(~Condition) +  # Separate plots for each Condition
  labs(
    title = "Hand Velocity Over Time",
    x = "Time (seconds)",
    y = "Velocity (m/s)",
    color = "Trial"
  ) +
  theme_minimal()




DropmoveDF$Condition <- as.factor(DropmoveDF$Condition)
DropmoveDF$Group <- as.factor(DropmoveDF$Group)


#DropmoveDF <- grab2DropDF
DropmoveDF <- DropmoveDF %>% dplyr::filter(Condition != "")





DropmoveDF <- data.frame(Participant = factor(),
                         Condition = factor(),
                         Trial = numeric(),
                         Group = factor(),
                         startTime = numeric(),
                         endTime = numeric(),
                         cubeColor = factor(),
                         Duration_Time = numeric(),
                         hand_avgVel = numeric(),
                         hand_peak_vel = numeric(),
                         hand_avg_accel = numeric(),
                         hand_peak_accel = numeric(),
                         hand_time2PeakVel = numeric(),
                         hand_time2PeakAccel = numeric(),
                         handRot_avgVel = numeric(),
                         handRot_peak_vel = numeric(),
                         handRot_avg_accel = numeric(),
                         handRot_peak_accel = numeric(),
                         handRot_time2PeakVel = numeric(),
                         handRot_time2PeakAccel = numeric(),
                         headRot_avgVel = numeric(),
                         headRot_peak_vel = numeric(),
                         headRot_avg_accel = numeric(),
                         headRot_peak_accel = numeric(),
                         headRot_time2PeakVel = numeric(),
                         headRot_time2PeakAccel = numeric(),
                         headRot_time2PeakAccel = numeric(),
                         gaze_avgVel = numeric(),
                         gaze_peak_vel = numeric(),
                         gaze_avg_accel = numeric(),
                         gaze_peak_accel = numeric(),
                         gaze_time2PeakVel = numeric(),
                         gaze_time2PeakAccel = numeric(),
                         hand_time2Initiation = numeric(),
                         handRot_time2Initiation = numeric(),
                         headRot_time2Initiation = numeric(),
                         gaze_time2Initiation = numeric(),
                         hand_endVel = numeric(),
                         handRot_endVel = numeric(),
                         headRot_endVel = numeric(),
                         gaze_endVel = numeric(),
                         hand_endAccel = numeric(),
                         handRot_endAccel = numeric(),
                         headRot_endAccel = numeric(),
                         gaze_endAccel = numeric(),
                         stringsAsFactors = FALSE)

#useDF <- DropmoveDF %>% filter(Duration_Time > .5)

useDF <- DropmoveDF %>% filter(hand_endVel <= sd(DropmoveDF$hand_endVel)*3)

summary_df <- DropmoveDF %>%  
  group_by(Participant, Group, Condition) %>%
  summarize(mean_value = mean(hand_avgVel, na.rm = TRUE), .groups = "drop")

#summary_df <- summary_df %>% filter(Condition == "comp")
#summary_df <- summary_df %>% filter(cubeColor == "Gold")


# Create the box plot
ggplot(summary_df, aes(x = Condition, y = mean_value, fill = Group)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.6) +
  labs(x = "Category", y = "Value", 
       title = "Box Plot of Values by Category and Group") +
  theme_minimal() +
  #ylim(250, 2000)+
  scale_fill_manual(values = c("lightblue", "lightcoral"), name = "Group") +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))




ggplot(DropmoveDF, aes(x = Condition, y = Duration_Time, fill = Group)) +
  geom_boxplot() +
  #geom_jitter(position = position_jitter(width = 0.2), size = 1, alpha = 0.6) +
  labs(x = "Condition", y = "Duration (ms)",
       title = "Movement Duration")+
  #subtitle = "Comparing two groups across five categories") +
  # scale_fill_manual(values = c("skyblue", "salmon"), name = "Group") +
  theme_minimal(base_size = 15) + 
  #ylim(0, 2500)+
  # Use a minimal theme with a larger base font size
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "top") # Move legend to the top

# DropmoveDF <- DropmoveDF %>% mutate(hand_time2PeakVel = hand_time2PeakVel/10000)
ggplot(DropmoveDF, aes(x = Condition, y = gaze_avgVel, fill = Group)) +
  geom_boxplot() +
  labs(
    x = "Condition",
    y = "degrees/s",
    title = "gaze avg vel"
  ) +
  scale_fill_manual(values = c("skyblue", "salmon"), name = "Group") +
  theme_minimal(base_size = 15) +
  #ylim(0, 5)+
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.text.x = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

ggplot(DropmoveDF, aes(x = Condition, y = headRot_peak_vel, fill = Group)) +
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

ggplot(DropmoveDF, aes(x = Condition, y = avg_accel, fill = Group)) +
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

ggplot(DropmoveDF, aes(x = Condition, y = gaze, fill = Group)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = NA) +
  #geom_jitter(position = position_jitter(width = 0.2), size = 1, alpha = 0.6) +
  labs(x = "Condition", y = "speed",
       title = "Peak Accel of Hand: Grab to Place")+
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

