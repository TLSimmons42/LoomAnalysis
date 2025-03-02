# Load required packages
library(dplyr)
library(ggplot2)
library(signal)  # For Butterworth filter
library(plotly)


#dirInput <- "C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Conversion Data/mergedData.csv"
#df_merged <- read.csv(dirInput, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)


df_Hand_Vel <- df_merged

df_Hand_Vel <- df_Hand_Vel %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  mutate(ModTime = (Time - Time[1])/10000000) 

df_Hand_Vel <- df_Hand_Vel %>% dplyr::filter(Condition == "comp" & Trial == 2)
df_Hand_Vel <- df_Hand_Vel %>% dplyr::filter(EyePos_X != 0)


df_Hand_Vel <- df_Hand_Vel %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  mutate(
    delta_t = ModTime - lag(ModTime),
    v_x = (HandPos_X - lag(HandPos_X)) / delta_t,
    v_y = (HandPos_Y - lag(HandPos_Y)) / delta_t,
    v_z = (HandPos_Z - lag(HandPos_Z)) / delta_t,
    velocity = sqrt(v_x^2 + v_y^2 + v_z^2)  # Magnitude of velocity
  )
print(df_Hand_Vel$v_x[2])


#FILTER TIME--------------------------------------------------

library(signal)  # For Butterworth filter
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
    velocity_smooth = filtfilt(bf, velocity)  # Smoothed velocity magnitude
  )
print(df_Hand_Vel$v_x[2])
print(df_Hand_Vel$velocity_smooth[1])

# Blue cube
gazeArrivePoint <- 62.75130
grabPoint <- 62.92226
dropPoint <- 64.99573
arrivePoint <- 64.54988

#white cube
gazeArrivePoint <- 68.53409
grabPoint <- 68.65318
dropPoint <- 69.78529
arrivePoint <- 70.16435


upper <- dropPoint + .6
lower <- grabPoint -.6
df_Hand_Vel <- df_Hand_Vel %>% dplyr::filter(ModTime > lower & ModTime < upper)

#df_Hand_Vel <- df_Hand_Vel %>% dplyr::filter(ModTime > 68 & ModTime < 71)

ggplot(df_Hand_Vel, aes(x = ModTime, y = velocity_smooth, color = as.factor(Trial))) +
  geom_line(alpha = 0.8) +
  geom_point()+
  facet_wrap(~Condition) +  # Separate plots for each Condition
  labs(
    title = "Hand Velocity Over Time",
    x = "Time (seconds)",
    y = "Velocity (m/s)",
    color = "Trial"
  ) +
  theme_minimal()

ggplot(df_Hand_Vel, aes(x = ModTime)) +
  geom_line(aes(y = velocity, color = as.factor(Trial)), alpha = 0.8) +
  geom_line(aes(y = velocity_smooth), color = "blue", linetype = "solid") + # New line
  geom_point(aes(y = velocity, color = as.factor(Trial))) +
  geom_point(aes(y = velocity_smooth, color = "blue")) +
  geom_vline(data = df_Hand_Vel, aes(xintercept = grabPoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_Hand_Vel, aes(xintercept = dropPoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_Hand_Vel, aes(xintercept = arrivePoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_Hand_Vel, aes(xintercept = gazeArrivePoint), color = "black", linetype = "solid", size = 1) +
  facet_wrap(~Condition) +  
  labs(
    title = "Hand Velocity Over Time White CUBE",
    x = "Time (seconds)",
    y = "Velocity (m/s)",
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
