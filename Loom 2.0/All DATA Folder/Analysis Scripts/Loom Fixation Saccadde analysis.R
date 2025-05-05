# Load required packages
library(dplyr)
library(ggplot2)
library(signal)  # For Butterworth filter
library(plotly)
library(bit64)
library(pracma)  # For rad2deg function


data_files <- list.files(pattern = "nuP15")
participantDataFile <- data_files[1]
print(participantDataFile)

df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)


df_trim <- df %>%
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

df_trim <- df_trim %>%
  group_by(Condition, Trial) %>%
  arrange(Time) %>%
  mutate(
    delta_t = ModTime - lag(ModTime),
    gaze_velocity = degrees / delta_t,
  )



fs <- 90   # Sampling frequency (Hz)
fc <- 6    # Cutoff frequency (Hz)
n <- 2     # 2nd-order filter (applied twice = 4th order total)
bf <- butter(n, fc / (fs / 2), type = "low")

df_trim <- df_trim %>% dplyr::filter(gaze_velocity < 800)
df_trim <- df_trim %>% mutate(gaze_velocity_filt_bf =  filtfilt(bf, gaze_velocity))
df_trim$gaze_velocity_filt <- runmed(df_trim$gaze_velocity, k = 10, endrule = "keep")

velocity_threshold <- 75  # degrees/second
df_trim$gazeType <- ifelse(df_trim$gaze_velocity_filt_bf < velocity_threshold, "fixation", "saccade")



df_trim <- df_trim %>%
  mutate(
    is_fixation = gazeType == "fixation",
    fixation_change = is_fixation != lag(is_fixation, default = FALSE),
    fixation_id = cumsum(fixation_change & is_fixation)
  )



min_fix_duration <- 0.01  # 100 ms

fixations <- df_trim %>%
  filter(gazeType == "fixation") %>%
  group_by(Condition, Trial, fixation_id) %>%
  summarise(
    start_time = first(ModTime),
    end_time = last(ModTime),
    duration = end_time - start_time,
    mean_velocity = mean(gaze_velocity_filt_bf, na.rm = TRUE),
    mean_gaze_angle = mean(degrees, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(duration >= min_fix_duration)

# Blue cube
gazeArrivePoint <- 62.75130
grabPoint <- 62.92226
placeLookPoint <- 64.42636
arrivePoint <- 64.54988
dropPoint <- 64.99573

#White cube
gazeArrivePoint <- 68.53409
grabPoint <- 68.65318
placeLookPoint <- 69.18653
dropPoint <- 69.78529
arrivePoint <- 70.16435


upper <- dropPoint + .6
lower <- grabPoint -.6
df_plot <- df_trim %>% dplyr::filter(ModTime > lower & ModTime < upper)

df_plot <- df_plot %>% dplyr::filter(Condition == "comp")
df_plot <- df_plot %>% dplyr::filter(Trial == 2)


#output_dir <- "C:\Users\Trent Simons\Desktop\Data\LoomAnalysis\Loom 2.0\Unity Conversion DATA" 
P1_a <- df_plot %>% dplyr::filter(ModTime <= gazeArrivePoint)
P1_b <- df_plot %>% dplyr::filter(ModTime <= grabPoint)
P2_a <- df_plot %>% dplyr::filter(ModTime <= placeLookPoint & ModTime >= grabPoint)
P2_b <- df_plot %>% dplyr::filter(ModTime <= arrivePoint & ModTime >= grabPoint)
P2_c <- df_plot %>% dplyr::filter(ModTime <= dropPoint & ModTime >= grabPoint)
P3_a <- df_plot %>% dplyr::filter(ModTime >= dropPoint)

write.csv(P1_a, "P1_a.csv", row.names = FALSE)
write.csv(P1_b, "P1_b.csv", row.names = FALSE)
write.csv(P2_a, "P2_a.csv", row.names = FALSE)
write.csv(P2_b, "P2_b.csv", row.names = FALSE)
write.csv(P2_c, "P2_c.csv", row.names = FALSE)
write.csv(P3_a, "P3_a.csv", row.names = FALSE)


ggplot(df_plot, aes(x = ModTime, y = gaze_velocity_filt_bf)) +
  geom_path(aes(color = as.factor(gazeType), group = 1), alpha = 0.8) +
  geom_point(aes(color = as.factor(gazeType))) +
  geom_vline(aes(xintercept = grabPoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(aes(xintercept = dropPoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_trim, aes(xintercept = placeLookPoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(aes(xintercept = arrivePoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(aes(xintercept = gazeArrivePoint), color = "black", linetype = "solid", size = 1) +
  labs(
    title = "Hand Velocity Over Time",
    x = "Time (seconds)",
    y = "Velocity (m/s)",
    color = "Gaze Type"
  ) +
  theme_minimal()

ggplot(df_plot, aes(x = ModTime , color = as.factor(gazeType))) +
  geom_line(aes(y = gaze_velocity, color = as.factor(Trial)), alpha = 0.8) +
  geom_line(aes(y = gaze_velocity_filt_bf), color = "blue", linetype = "solid") + # New line
  geom_line(aes(y = gaze_velocity_filt), color = "green", linetype = "solid") + # New line
  geom_point(aes(y = gaze_velocity, color = as.factor(Trial))) +
  geom_point(aes(y = gaze_velocity_filt_bf, color = "blue")) +
  geom_point(aes(y = gaze_velocity_filt, color = "green")) +
  geom_vline(data = df_trim, aes(xintercept = grabPoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_trim, aes(xintercept = dropPoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_trim, aes(xintercept = placeLookPoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_trim, aes(xintercept = arrivePoint), color = "black", linetype = "solid", size = 1) +
  geom_vline(data = df_trim, aes(xintercept = gazeArrivePoint), color = "black", linetype = "solid", size = 1) +
  #facet_wrap(~Condition) +  
  labs(
    title = "Head Rotation Velocity - White CUBE",
    x = "Time (seconds)",
    y = "Velocity (deg/s)",
    color = "Trial"
  ) +
  theme_minimal()



fixations <- df_plot %>%
  filter(gazeType == "fixation") %>%
  group_by(Condition, Trial, fixation_id) %>%
  summarise(
    start_time = first(ModTime),
    end_time = last(ModTime),
    duration = end_time - start_time,
    mean_velocity = mean(gaze_velocity_filt_bf, na.rm = TRUE),
    mean_gaze_angle = mean(degrees, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(duration >= min_fix_duration)

