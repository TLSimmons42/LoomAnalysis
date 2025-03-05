# Load required packages
library(dplyr)
library(ggplot2)
library(signal)  # For Butterworth filter
library(plotly)
library(bit64)
library(pracma)  # For rad2deg function




 #dirInput <- "C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Conversion Data/mergedData.csv"
 #df_merged <- read.csv(dirInput, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)


df_Hand_Vel <- df_merged

df_Hand_Vel <- df_Hand_Vel %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  mutate(ModTime = (Time - Time[1])/10000000) 

df_Hand_Vel <- df_Hand_Vel %>% dplyr::filter(Condition == "comp" & Trial == 2)
df_Hand_Vel <- df_Hand_Vel %>% dplyr::filter(EyePos_X != 0)
df_Hand_Vel <- df_Hand_Vel %>% dplyr::filter(EyePos_X != "N/A")

df_Hand_Vel$EyePos_X <- as.numeric(df_Hand_Vel$EyePos_X)
df_Hand_Vel$EyePos_Y <- as.numeric(df_Hand_Vel$EyePos_Y)
df_Hand_Vel$EyePos_Z <- as.numeric(df_Hand_Vel$EyePos_Y)



# Calculate Hand Velocity
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

# # Calculate gaze Velocity
# df_Hand_Vel <- df_Hand_Vel %>%
#   group_by(Condition, Trial) %>%
#   arrange(Time) %>%
#   mutate(
#     delta_t = ModTime - lag(ModTime),
#     gaze_velocity_x = (eyeAngleX - lag(eyeAngleX)) / delta_t,
#     gaze_velocity_y = (eyeAngleY - lag(eyeAngleY)) / delta_t,
#     gaze_velocity_z = (eyeAngleZ - lag(eyeAngleZ)) / delta_t,
#     gaze_velocity = sqrt(gaze_velocity_x^2 + gaze_velocity_y^2 + gaze_velocity_z^2)  # Magnitude of velocity
#   )

#calculate degree
df_Hand_Vel <- df_Hand_Vel %>%
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
    
    # Ensure dot product is within valid range [-1,1] to avoid NaN issues
    #dotProd = pmin(pmax(dotProd, -1), 1),
    
    # Compute angle in radians and convert to degrees
    theta_radians = acos(dotProd),
    degrees = rad2deg(theta_radians)
  )

# for(i in 2:nrow(df_Hand_Vel)){
#   df_Hand_Vel <- df_Hand_Vel %>% mutate(dotProd = (eyeAngleX[i-1] * eyeAngleX[i]) + (eyeAngleY[i-1] * eyeAngleY[i])+(eyeAngleZ[i-1] * eyeAngleZ[i]))
#   df_Hand_Vel <- df_Hand_Vel %>% mutate(theta_radians = acos(dotProd))
#   df_Hand_Vel <- df_Hand_Vel %>% mutate(degrees = rad2deg(theta_radians))
#   
# }

#calculate velocity of change in degrees
df_Hand_Vel <- df_Hand_Vel %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  mutate(
    delta_t = ModTime - lag(ModTime),
    gaze_velocity = degrees / delta_t,
  )

#FILTER TIME--------------------------------------------------

fs <- 90   # Sampling frequency (Hz)
fc <- 6    # Cutoff frequency (Hz)
n <- 2     # 2nd-order filter (applied twice = 4th order total)
bf <- butter(n, fc / (fs / 2), type = "low")

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
dropPoint <- 69.78529
arrivePoint <- 70.16435


upper <- dropPoint + .6
lower <- grabPoint -.6
df_Hand_Vel <- df_Hand_Vel %>% dplyr::filter(ModTime > lower & ModTime < upper)

#df_Hand_Vel <- df_Hand_Vel %>% dplyr::filter(ModTime > 63.1 & ModTime < 63.5)

df_Hand_Vel <- df_Hand_Vel %>% dplyr::filter(gaze_velocity < 800)
df_Hand_Vel <- df_Hand_Vel %>% mutate(gaze_velocity_filt_bf =  filtfilt(bf, gaze_velocity))
df_Hand_Vel$gaze_velocity_filt <- runmed(df_Hand_Vel$gaze_velocity, k = 7, endrule = "keep")


ggplot(df_Hand_Vel, aes(x = ModTime, y = gaze_velocity_filt, color = as.factor(Trial))) +
  geom_line(alpha = 0.8) +
  geom_point()+
  # geom_vline(data = df_Hand_Vel, aes(xintercept = grabPoint), color = "black", linetype = "solid", size = 1) +
  # geom_vline(data = df_Hand_Vel, aes(xintercept = dropPoint), color = "black", linetype = "solid", size = 1) +
  # geom_vline(data = df_Hand_Vel, aes(xintercept = arrivePoint), color = "black", linetype = "solid", size = 1) +
  # geom_vline(data = df_Hand_Vel, aes(xintercept = gazeArrivePoint), color = "black", linetype = "solid", size = 1) +
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
  geom_line(aes(y = gaze_velocity_filt), color = "blue", linetype = "solid") + # New line
  geom_line(aes(y = gaze_velocity_filt_bf), color = "blue", linetype = "solid") + # New line
  geom_point(aes(y = gaze_velocity, color = as.factor(Trial))) +
  geom_point(aes(y = gaze_velocity_filt, color = "blue")) +
  geom_point(aes(y = gaze_velocity_filt_bf, color = "green")) +
  geom_vline(data = df_Hand_Vel, aes(xintercept = grabPoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_Hand_Vel, aes(xintercept = dropPoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_Hand_Vel, aes(xintercept = arrivePoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_Hand_Vel, aes(xintercept = gazeArrivePoint), color = "black", linetype = "solid", size = 1) +
  facet_wrap(~Condition) +  
  labs(
    title = "Hand Velocity Over Time Blue CUBE",
    x = "Time (seconds)",
    y = "Velocity (deg/s)",
    color = "Trial"
  ) +
  theme_minimal()


df_Hand_Vel$CurrentGazeArea <- factor(df_Hand_Vel$CurrentGazeArea, 
                                      levels = c("background_wall", "build_wall", "play_wall"))

#write.csv(df_Hand_Vel, "gazeVel.csv", row.names = FALSE)

plot_ly(df_Hand_Vel,
        x = ~HandPos_X, y = ~HandPos_Y, z = ~HandPos_Z,
        color = ~ CurrentGazeArea,
        #colors = c("red", "blue", "green"),
        type = "scatter3d", mode = "markers")

#Gaze Velocity ----------------------------------------------------------------------------

df_Gaze_Vel <- df_merged

df_Gaze_Vel <- df_Gaze_Vel %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  mutate(ModTime = (Time - Time[1])/10000000) 

df_Gaze_Vel <- df_Gaze_Vel %>% dplyr::filter(Condition == "comp" & Trial == 2)

df_Gaze_Vel$EyePos_X <- as.numeric(df_Gaze_Vel$EyePos_X)
df_Gaze_Vel$EyePos_Y <- as.numeric(df_Gaze_Vel$EyePos_Y)
df_Gaze_Vel$EyePos_Z <- as.numeric(df_Gaze_Vel$EyePos_Z)
df_Gaze_Vel$HeadPos_X <- as.numeric(df_Gaze_Vel$HeadPos_X)
df_Gaze_Vel$HeadPos_Y <- as.numeric(df_Gaze_Vel$HeadPos_Y)
df_Gaze_Vel$HeadPos_Z <- as.numeric(df_Gaze_Vel$HeadPos_Z)



df_Gaze_Vel <- df_Gaze_Vel %>%
  dplyr::filter(EyePos_X > -5) %>%
  dplyr::filter(EyePos_X != 0) 

df_Gaze_Vel <- df_Gaze_Vel %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "background_wall" & EyePos_X == 1.6 & EyePos_Z >= -15 & EyePos_Z <= 16, "play_wall", CurrentGazeArea))
df_Gaze_Vel <- df_Gaze_Vel %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "background_wall" & EyePos_X <= 15.5 & EyePos_X >= 9.5, "view_wall", CurrentGazeArea))
df_Gaze_Vel <- df_Gaze_Vel %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "background_wall" & EyePos_X <= 15.5 & EyePos_X >= 10, "build_wall", CurrentGazeArea))

df_Gaze_Vel <- df_Gaze_Vel %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "view_wall" & EyePos_Z > 0 & EyePos_Z < 12, "build_wall", CurrentGazeArea ))
df_Gaze_Vel <- df_Gaze_Vel %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "view_wall" & EyePos_Z > 12, "background_wall", CurrentGazeArea ))


#Degrees Calculation--------------------------------------------------------------------------
initialHeadPos <- c(df_Gaze_Vel$HeadPos_X[1],df_Gaze_Vel$HeadPos_Y[1],df_Gaze_Vel$HeadPos_Z[1])
initialGazePos <- c(df_Gaze_Vel$EyePos_X[1],df_Gaze_Vel$EyePos_Y[1],df_Gaze_Vel$EyePos_Z[1])
initialVector <- initialGazePos - initialHeadPos

tempVar <- 2
tempHeadPos <- c(df_Gaze_Vel$HeadPos_X[tempVar],df_Gaze_Vel$HeadPos_Y[tempVar],df_Gaze_Vel$HeadPos_Z[tempVar])
tempGazePos <- c(df_Gaze_Vel$EyePos_X[tempVar],df_Gaze_Vel$EyePos_Y[tempVar],df_Gaze_Vel$EyePos_Z[tempVar])
tempVector <- tempHeadPos - tempGazePos


df_Gaze_Vel <- df_Gaze_Vel %>% mutate(gazeVecX = EyePos_X - HeadPos_X)
df_Gaze_Vel <- df_Gaze_Vel %>% mutate(gazeVecY = EyePos_Y - HeadPos_Y)
df_Gaze_Vel <- df_Gaze_Vel %>% mutate(gazeVecZ = EyePos_Z - HeadPos_Z)




df_Gaze_Vel <- df_Gaze_Vel %>% mutate(dotProd = (initialVector[1] * gazeVecX)+ (initialVector[2] * gazeVecY)+(initialVector[3] * gazeVecZ))

initialVector_magnitude <- sqrt(sum(initialVector^2))
df_Gaze_Vel <- df_Gaze_Vel %>% mutate(GazeVecMagnitude = sqrt(gazeVecX^2 + gazeVecY^2 + gazeVecZ^2)) 


df_Gaze_Vel <- df_Gaze_Vel %>%  mutate(cos_theta = dotProd / (initialVector_magnitude * GazeVecMagnitude)) 
df_Gaze_Vel <- df_Gaze_Vel %>%  mutate(Radians = acos(cos_theta))

df_Gaze_Vel <- df_Gaze_Vel %>%  mutate(Degrees = Radians * (180 / pi))


df_Gaze_Vel <- df_Gaze_Vel %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  # mutate(
  #   delta_t = ModTime - lag(ModTime),
  #   v_x = (as.numeric(EyePos_X) - lag(as.numeric(EyePos_X))) / delta_t,
  #   v_y = (as.numeric(EyePos_Y) - lag(as.numeric(EyePos_Y))) / delta_t,
  #   v_z = (as.numeric(EyePos_Z) - lag(as.numeric(EyePos_Z))) / delta_t,
  #   velocity = sqrt(v_x^2 + v_y^2 + v_z^2)  # Magnitude of velocity
  # )
  mutate(
    delta_t = ModTime - lag(ModTime),  # Time difference
    delta_angle = abs(Degrees - lag(Degrees)),  # Change in degrees
    angular_velocity = delta_angle / delta_t  # Angular velocity (deg/sec)
  )


df_Gaze_Vel <- df_Gaze_Vel %>% dplyr::filter(ModTime > 40.5 & ModTime < 42)




ggplot(df_Gaze_Vel, aes(x = ModTime, y = angular_velocity, color = as.factor(Trial))) +
  geom_line(alpha = 0.8) +
  geom_point()+
  facet_wrap(~Condition) +  # Separate plots for each Condition
  labs(
    title = "Gaze Velocity Over Time",
    x = "Time (seconds)",
    y = "Velocity (units/s)",
    color = "Trial"
  ) +
  theme_minimal()

df_Gaze_Vel$CurrentGazeArea <- factor(df_Gaze_Vel$CurrentGazeArea, 
                                      levels = c("background_wall", "build_wall", "play_wall"))

#write.csv(df_Gaze_Vel, "gazeVel.csv", row.names = FALSE)

# plot_ly(df_Gaze_Vel, 
#         x = ~EyePos_X, y = ~EyePos_Y, z = ~EyePos_Z,
#         color = ~ CurrentGazeArea,
#         #colors = c("red", "blue", "green"),
#         type = "scatter3d", mode = "markers")
