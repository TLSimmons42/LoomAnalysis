# Load required packages
library(dplyr)
library(ggplot2)
library(signal)  # For Butterworth filter
#library(plotly)
library(bit64)
library(pracma)  # For rad2deg function





 #dirInput <- "C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Conversion Data/mergedData.csv"
 #df_merged <- read.csv(dirInput, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)


#df_Hand_Vel <- df_merged


data_files <- list.files(pattern = "nuP15")
participantDataFile <- data_files[1]
print(participantDataFile)

df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)


# df_Hand_Vel <- df_Hand_Vel %>%
#   group_by(Condition, Trial) %>%
#   arrange(Time) %>%
#   mutate(ModTime = (Time - Time[1])/10000000) 



df_Hand_Vel <- df %>% dplyr::filter(Condition == "comp" & Trial == 2)

df_Hand_Vel$EyePos_X <- as.numeric(df_Hand_Vel$EyePos_X)
df_Hand_Vel$EyePos_Y <- as.numeric(df_Hand_Vel$EyePos_Y)
df_Hand_Vel$EyePos_Z <- as.numeric(df_Hand_Vel$EyePos_Y)


#FILTER TIME--------------------------------------------------

fs <- 90   # Sampling frequency (Hz)
fc <- 6    # Cutoff frequency (Hz)
n <- 2     # 2nd-order filter (applied twice = 4th order total)
bf <- butter(n, fc / (fs / 2), type = "low")

# Calculate Hand Velocity  and Acceleration --------------------------------------------------------------------------
df_Hand_Vel <- df_Hand_Vel %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  mutate(
    delta_t = ModTime - lag(ModTime),
    v_x = (HandPos_X - lag(HandPos_X)) / delta_t,
    v_y = (HandPos_Y - lag(HandPos_Y)) / delta_t,
    v_z = (HandPos_Z - lag(HandPos_Z)) / delta_t,
    hand_velocity = sqrt(v_x^2 + v_y^2 + v_z^2)  # Magnitude of velocity
  )
# Calculate gaze Distance----------------------------------------------------------------------------------------------------------------

df_Hand_Vel <- df_Hand_Vel %>%
  mutate(gaze_distance = sqrt((EyePos_X - lag(EyePos_X))^2 + (EyePos_Y - lag(EyePos_Y))^2 + (EyePos_Z - lag(EyePos_Z))^2))

# Calculate gaze Velocity----------------------------------------------------------------------------------------------------------------

df_Hand_Vel <- df_Hand_Vel %>%
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

df_Hand_Vel <- df_Hand_Vel %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  mutate(
    delta_t = ModTime - lag(ModTime),
    gaze_velocity = degrees / delta_t,
  )



# Head Rotation Velocity --------------------------------------------------------------------
df_Hand_Vel <- df_Hand_Vel %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  # Ensure correct order
  mutate(
    dt = Time - lag(Time),  # Time difference
    
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
df_Hand_Vel <- df_Hand_Vel %>%
  mutate(head_rot_velocity_x = ifelse(is.na(head_rot_velocity_x), 0, head_rot_velocity_x),
         head_rot_velocity_y = ifelse(is.na(head_rot_velocity_y), 0, head_rot_velocity_y),
         head_rot_velocity_z = ifelse(is.na(head_rot_velocity_z), 0, head_rot_velocity_z),
         head_rot_velocity = ifelse(is.na(head_rot_velocity), 0, head_rot_velocity))


df_Hand_Vel <- df_Hand_Vel %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  mutate(
    head_rot_smooth_x = filtfilt(bf, head_rot_velocity_x),
    head_rot_smooth_y = filtfilt(bf, head_rot_velocity_y),
    head_rot_smooth_z = filtfilt(bf, head_rot_velocity_z),
    head_rot_Vel_smooth = filtfilt(bf, head_rot_velocity)  # Smoothed velocity magnitude
  )



df_Hand_Vel <- df_Hand_Vel %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  mutate(
    head_a_x = (head_rot_smooth_x - lag(head_rot_smooth_x)) / delta_t,
    head_a_y = (head_rot_smooth_y - lag(head_rot_smooth_y)) / delta_t,
    head_a_z = (head_rot_smooth_z - lag(head_rot_smooth_z)) / delta_t,
    head_ROT_acceleration = sqrt(head_a_x^2 + head_a_y^2 + head_a_z^2)  # Magnitude of acceleration
  )
df_Hand_Vel <- df_Hand_Vel %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  mutate(
    hand_a_x_smooth = filtfilt(bf, head_a_x),
    hand_a_y_smooth = filtfilt(bf, head_a_y),
    hand_a_z_smooth = filtfilt(bf, head_a_z),
    head_ROT_acceleration_smooth = filtfilt(bf, head_ROT_acceleration)  # Smoothed velocity magnitude
  )




# Hand Rotation Velocity --------------------------------------------------------------------
df_Hand_Vel <- df_Hand_Vel %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  # Ensure correct order
  mutate(
    handDt = Time - lag(Time),  # Time difference
    
    handDx = xHandRot - lag(xHandRot),  # Change in X
    handDy = yHandRot - lag(yHandRot),  # Change in Y
    handDz = zHandRot - lag(zHandRot),  # Change in Z
    
    # Handle angle wraparound (-180 to 180 case)
    handDx = ifelse(handDx > 180, handDx - 360, ifelse(handDx < -180, handDx + 360, handDx)),
    handDy = ifelse(handDy > 180, handDy - 360, ifelse(handDy < -180, dy + 360, handDy)),
    handDz = ifelse(handDz > 180, handDz - 360, ifelse(handDz < -180, handDz + 360, handDz)),
    
    # Compute angular velocity (deg/s)
    hand_rot_velocity_x = handDx / handDt,
    hand_rot_velocity_y = handDy / handDt,
    hand_rot_velocity_z = handDz / handDt,
    
    # Compute total angular velocity magnitude
    hand_rot_velocity = sqrt(hand_rot_velocity_x^2 + hand_rot_velocity_y^2 + hand_rot_velocity_z^2)
  )
  df_Hand_Vel <- df_Hand_Vel %>%
    mutate(hand_rot_velocity_x = ifelse(is.na(hand_rot_velocity_x), 0, hand_rot_velocity_x),
           hand_rot_velocity_y = ifelse(is.na(hand_rot_velocity_y), 0, hand_rot_velocity_y),
           hand_rot_velocity_z = ifelse(is.na(hand_rot_velocity_z), 0, hand_rot_velocity_z),
           hand_rot_velocity = ifelse(is.na(hand_rot_velocity), 0, hand_rot_velocity))

  df_Hand_Vel <- df_Hand_Vel %>%
    group_by(Condition, Trial) %>%
    arrange(Time) %>%
    mutate(
      hand_rot_smooth_x = filtfilt(bf, hand_rot_velocity_x),
      hand_rot_smooth_y = filtfilt(bf, hand_rot_velocity_y),
      hand_rot_smooth_z = filtfilt(bf, hand_rot_velocity_z),
      hand_rot_Vel_smooth = filtfilt(bf, hand_rot_velocity)  # Smoothed velocity magnitude
    )



  df_Hand_Vel <- df_Hand_Vel %>%
    group_by(Condition, Trial) %>%
    arrange(Time) %>%
    mutate(
      hand_a_x = (hand_rot_smooth_x - lag(hand_rot_smooth_x)) / delta_t,
      hand_a_y = (hand_rot_smooth_y - lag(hand_rot_smooth_y)) / delta_t,
      hand_a_z = (hand_rot_smooth_z - lag(hand_rot_smooth_z)) / delta_t,
      hand_ROT_acceleration = sqrt(hand_a_x^2 + hand_a_y^2 + hand_a_z^2)  # Magnitude of acceleration
    )
  df_Hand_Vel <- df_Hand_Vel %>%
    group_by(Condition, Trial) %>%
    arrange(Time) %>%
    mutate(
      hand_a_x_smooth = filtfilt(bf, hand_a_x),
      hand_a_y_smooth = filtfilt(bf, hand_a_y),
      hand_a_z_smooth = filtfilt(bf, hand_a_z),
      hand_ROT_acceleration_smooth = filtfilt(bf, hand_ROT_acceleration)  # Smoothed velocity magnitude
    )





#GAZE VELOCIRTTY ACCELERATION & FILLTERING------------------------------------------------------------------

df_Hand_Vel <- df_Hand_Vel %>% dplyr::filter(gaze_velocity < 800)
df_Hand_Vel <- df_Hand_Vel %>% mutate(gaze_velocity_filt_bf =  filtfilt(bf, gaze_velocity))
df_Hand_Vel$gaze_velocity_filt <- runmed(df_Hand_Vel$gaze_velocity, k = 7, endrule = "keep")

df_Hand_Vel <- df_Hand_Vel %>%
  arrange(Time) %>%  # Ensure order
  mutate(
    gaze_vel_dt = Time - lag(Time),  # Time difference
    gaze_vel_dv = gaze_velocity_filt - lag(gaze_velocity_filt),        # Change in velocity
    gaze_accel =  abs(gaze_vel_dv / gaze_vel_dt)         # Acceleration (°/s²)
  )
df_Hand_Vel <- df_Hand_Vel %>% mutate(gaze_accel_filt_bf =  filtfilt(bf, gaze_accel))
df_Hand_Vel$gaze_accel_filt <- runmed(df_Hand_Vel$gaze_accel, k = 7, endrule = "keep")



# HAND VELOCITY FILTER AND ACELLERATION + FILTER------------------------------------------------------------------------------
df_Hand_Vel <- df_Hand_Vel %>%
  # group_by(Condition, Trial) %>%
  mutate(
    v_x_smooth = filtfilt(bf, v_x),
    v_y_smooth = filtfilt(bf, v_y),
    v_z_smooth = filtfilt(bf, v_z),
    velocity_smooth = filtfilt(bf, hand_velocity)  # Smoothed velocity magnitude
  )
print(df_Hand_Vel$v_x[2])
print(df_Hand_Vel$velocity_smooth[1])

df_Hand_Vel <- df_Hand_Vel %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  mutate(
    a_x = (v_x_smooth - lag(v_x_smooth)) / delta_t,
    a_y = (v_y_smooth - lag(v_y_smooth)) / delta_t,
    a_z = (v_z_smooth - lag(v_z_smooth)) / delta_t,
    acceleration = sqrt(a_x^2 + a_y^2 + a_z^2)  # Magnitude of acceleration
  )
df_Hand_Vel <- df_Hand_Vel %>%
  # group_by(Condition, Trial) %>%
  mutate(
    a_x_smooth = filtfilt(bf, a_x),
    a_y_smooth = filtfilt(bf, a_x),
    a_z_smooth = filtfilt(bf, a_x),
    acceleration_smooth = filtfilt(bf, acceleration)  # Smoothed velocity magnitude
  )

# Blue cube
# gazeArrivePoint <- 62.75130
# grabPoint <- 62.92226
# dropPoint <- 64.99573
# arrivePoint <- 64.54988

# #white cube
gazeArrivePoint <- 68.53409
grabPoint <- 68.65318
placeLookPoint <- 69.18653
dropPoint <- 69.78529
arrivePoint <- 70.16435


upper <- dropPoint + .6
lower <- grabPoint -.6
df_Hand_Vel <- df_Hand_Vel %>% dplyr::filter(ModTime > lower & ModTime < upper)

#df_Hand_Vel <- df_Hand_Vel %>% dplyr::filter(ModTime > 63.1 & ModTime < 63.5)

ggplot(df_Hand_Vel, aes(x = ModTime, y = gaze_distance, color = as.factor(CurrentGazeArea))) +
  geom_line(alpha = 0.8) +
  geom_point()+
  geom_vline(data = df_Hand_Vel, aes(xintercept = grabPoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_Hand_Vel, aes(xintercept = dropPoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_Hand_Vel, aes(xintercept = arrivePoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_Hand_Vel, aes(xintercept = gazeArrivePoint), color = "black", linetype = "solid", size = 1) +
  facet_wrap(~Condition) +  # Separate plots for each Condition
  labs(
    title = "Hand Velocity Over Time",
    x = "Time (seconds)",
    y = "Velocity (m/s)",
    color = "Trial"
  ) +
  theme_minimal()

ggplot(df_Hand_Vel, aes(x = ModTime)) +
  geom_line(aes(y = gaze_velocity, color = as.factor(Trial)), alpha = 0.8) +
  geom_line(aes(y = gaze_velocity_filt_bf), color = "blue", linetype = "solid") + # New line
  geom_line(aes(y = gaze_velocity_filt), color = "blue", linetype = "solid") + # New line
  geom_point(aes(y = gaze_velocity, color = as.factor(Trial))) +
  geom_point(aes(y = gaze_velocity_filt_bf, color = "blue")) +
  geom_point(aes(y = gaze_velocity_filt, color = "green")) +
  geom_vline(data = df_Hand_Vel, aes(xintercept = grabPoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_Hand_Vel, aes(xintercept = dropPoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_Hand_Vel, aes(xintercept = placeLookPoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_Hand_Vel, aes(xintercept = arrivePoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_Hand_Vel, aes(xintercept = gazeArrivePoint), color = "black", linetype = "solid", size = 1) +
  facet_wrap(~Condition) +  
  labs(
    title = "Head Rotation Velocity - White CUBE",
    x = "Time (seconds)",
    y = "Velocity (deg/s)",
    color = "Trial"
  ) +
  theme_minimal()


df_Hand_Vel$CurrentGazeArea <- factor(df_Hand_Vel$CurrentGazeArea, 
                                      levels = c("background_wall", "build_wall", "play_wall"))

write.csv(df_Hand_Vel, "UnitySim.csv", row.names = FALSE)

plot_ly(df_Hand_Vel,
        x = ~HandPos_X, y = ~HandPos_Y, z = ~HandPos_Z,
        color = ~ CurrentGazeArea,
        #colors = c("red", "blue", "green"),
        type = "scatter3d", mode = "markers")

plot_ly(df_Hand_Vel,
        x = ~EyePos_X, y = ~EyePos_Y, z = ~EyePos_Z,
        color = ~ CurrentGazeArea,
        #colors = c("red", "blue", "green"),
        type = "scatter3d", mode = "markers")


