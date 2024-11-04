library(ggplot2)
library(plot3D)
library(rgl)
library(dplyr)
library(bit64)
library(stringr)


data_files <- list.files(pattern = "nuP15.csv")
participantDataFile <- data_files[1]
print(participantDataFile)

df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
df <- df[!duplicated(df$Time), ]

df$Event <- gsub("\\(|\\)", "", df$Event)
df$CurrentGazeTarget <- gsub("\\(|\\)", "", df$CurrentGazeTarget)
gameOverTime <- df[df$Event == "Game Over",]
df <- df %>% filter(df$Time <= gameOverTime[1,1])

df$EyePos_Y <- as.numeric(df$EyePos_Y)

dfGazeOrigin <- df %>% filter(HeadPos_Y >= EyePos_Y-.1 & HeadPos_Y <= EyePos_Y+.1)
dfGazeOrigin <- df %>% filter(HeadPos_Y == EyePos_Y)


dfsim <- individualPACdf

dfsim <- dfsim %>% filter(PACtime == 251.7297)

# upperTime <- dfsim$PACstartTime[1] - 20000000
# lowerTime <- dfsim$PACendTime[1] + 20000000

upperTime <- dfsim$PACstartTime[1] - 10000000/1.5
lowerTime <- dfsim$PACendTime[1] + 10000000/1.5

dfsimTrim <- df %>% filter(Time >= upperTime & Time <= lowerTime)
dfsimTrim$EyePos_X <- as.numeric(dfsimTrim$EyePos_X)
dfsimTrim$EyePos_Y <- as.numeric(dfsimTrim$EyePos_Y)
dfsimTrim$EyePos_Z <- as.numeric(dfsimTrim$EyePos_Z)
dfsimTrim <- dfsimTrim %>% mutate(ModTime = (Time - Time[1])/10000000)
dfsimTrim <- dfsimTrim %>% mutate(handPosX = (abs(HandPos_X - HandPos_X[1])))
dfsimTrim <- dfsimTrim %>% filter(EyePos_X != 0)


initialHeadPos <- c(dfsimTrim$HeadPos_X[1],dfsimTrim$HeadPos_Y[1],dfsimTrim$HeadPos_Z[1])
initialGazePos <- c(dfsimTrim$EyePos_X[1],dfsimTrim$EyePos_Y[1],dfsimTrim$EyePos_Z[1])
initialVector <- initialGazePos - initialHeadPos

tempVar <- 2
tempHeadPos <- c(dfsimTrim$HeadPos_X[tempVar],dfsimTrim$HeadPos_Y[tempVar],dfsimTrim$HeadPos_Z[tempVar])
tempGazePos <- c(dfsimTrim$EyePos_X[tempVar],dfsimTrim$EyePos_Y[tempVar],dfsimTrim$EyePos_Z[tempVar])
tempVector <- tempHeadPos - tempGazePos

tempDotProd <- sum(initialVector * tempVector)
magnitude_V1 <- sqrt(sum(initialVector^2))
magnitude_V2 <- sqrt(sum(tempVector^2))
cos_theta <- tempDotProd / (magnitude_V1 * magnitude_V2)
theta_radians <- acos(cos_theta)
theta_radians
#theta_degrees <- theta_radians * (180 / pi)
#theta_degrees


dfsimTrim <- dfsimTrim %>% mutate(gazeVecX = EyePos_X - HeadPos_X)
dfsimTrim <- dfsimTrim %>% mutate(gazeVecY = EyePos_Y - HeadPos_Y)
dfsimTrim <- dfsimTrim %>% mutate(gazeVecZ = EyePos_Z - HeadPos_Z)

dfsimTrim <- dfsimTrim %>% mutate(dotProd = (initialVector[1] * gazeVecX)+ (initialVector[2] * gazeVecY)+(initialVector[3] * gazeVecZ))

initialVector_magnitude <- sqrt(sum(initialVector^2))
dfsimTrim <- dfsimTrim %>% mutate(GazeVecMagnitude = sqrt(gazeVecX^2 + gazeVecY^2 + gazeVecZ^2)) 


dfsimTrim <- dfsimTrim %>%  mutate(cos_theta = dotProd / (initialVector_magnitude * GazeVecMagnitude)) 
dfsimTrim <- dfsimTrim %>%  mutate(Radians = acos(cos_theta))

dfsimTrim <- dfsimTrim %>%  mutate(Degrees = Radians * (180 / pi))






# dfsimTrimFront <- dfsimTrim %>% filter(Time <= dfsim$PACstartTime[1])
# dfsimTrimDurring <- dfsimTrim %>% filter(Time >= dfsim$PACstartTime[1] & Time <= dfsim$PACendTime[1])
# dfsimTrimEnd <- dfsimTrim %>% filter(Time >= dfsim$PACendTime[1])


# #Sample data
# df <- data.frame(
#   x = 1:10,
#   y1 = c(3, 5, 2, 8, 7, 4, 6, 8, 9, 17),       # Data for left y-axis
#   y2 = c(30, 50, 20, 80, 70, 40, 60, 80, 90, 70), # Data for right y-axis
#   y3 = c(1, 4, 6, 3, 5, 7, 8, 6, 9, 10)       # Data for third line, sharing left y-axis
# )

dfsimTrim <- dfsimTrim %>% mutate(HeadRot_Y = HeadRot_Y*100)

# Plot with two y-axes and three lines, with style differentiation
ggplot(dfsimTrim, aes(x = ModTime)) +
  # Line for y1 with left y-axis in solid blue
  geom_line(aes(y = Degrees, color = "Eye Movement"), size = 1.2, linetype = "solid") +
  # Line for y2 with right y-axis in red, scaled to match y1 axis
  geom_line(aes(y = handPosX / max(handPosX) * max(Degrees), color = "Hand Movement"), size = 1.2, linetype = "solid") +
  # Line for y3, sharing left y-axis, in dashed blue
  geom_line(aes(y = HeadRot_Y, color = "Head Rotation Y-Axis"), size = 1.2, linetype = "dashed") +
  # Define primary y-axis
  scale_y_continuous(
    name = "Degrees", 
    # Define secondary y-axis with transformation
    sec.axis = sec_axis(~ . * max(dfsimTrim$handPosX) / max(dfsimTrim$Degrees), name = "Distance (meters)")
  ) +
  labs(x = "Time (s)") +
  # Define line colors with custom labels
  scale_color_manual(
    name = "Legend", 
    values = c("Eye Movement" = "blue", "Hand Movement" = "red", "Head Rotation Y-Axis" = "blue")
  ) +
  # Theme customization
  theme_minimal(base_size = 15) +
  theme(
    axis.title.y.left = element_text(color = "blue", size = 13),
    axis.title.y.right = element_text(color = "red", size = 13),
    axis.text.y.left = element_text(color = "blue"),
    axis.text.y.right = element_text(color = "red"),
    legend.position = "top",
    legend.title = element_blank()
  )