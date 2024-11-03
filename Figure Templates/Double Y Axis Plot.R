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

upperTime <- dfsim$PACstartTime[1] - 10000000/2
lowerTime <- dfsim$PACendTime[1] + 10000000/2

dfsimTrim <- df %>% filter(Time >= upperTime & Time <= lowerTime)
dfsimTrim$EyePos_X <- as.numeric(dfsimTrim$EyePos_X)
dfsimTrim$EyePos_Y <- as.numeric(dfsimTrim$EyePos_Y)
dfsimTrim$EyePos_Z <- as.numeric(dfsimTrim$EyePos_Z)



initialHeadPos <- c(dfsimTrim$HeadPos_X[1],dfsimTrim$HeadPos_Y[1],dfsimTrim$HeadPos_Z[1])
initialGazePos <- c(dfsimTrim$EyePos_X[1],dfsimTrim$EyePos_Y[1],dfsimTrim$EyePos_Z[1])
initialVector <- initialGazePos - initialHeadPos

tempVar <- 3
tempHeadPos <- c(dfsimTrim$HeadPos_X[tempVar],dfsimTrim$HeadPos_Y[tempVar],dfsimTrim$HeadPos_Z[tempVar])
tempGazePos <- c(dfsimTrim$EyePos_X[tempVar],dfsimTrim$EyePos_Y[tempVar],dfsimTrim$EyePos_Z[tempVar])
tempVector <- tempHeadPos - tempGazePos


tempDotProd <- sum(initialVector * tempVector)
magnitude_V1 <- sqrt(sum(initialVector^2))
magnitude_V2 <- sqrt(sum(tempVector^2))
cos_theta <- tempDotProd / (magnitude_V1 * magnitude_V2)
theta_radians <- acos(cos_theta)
theta_degrees <- theta_radians * (180 / pi)
theta_degrees


dfsimTrim <- dfsimTrim %>% mutate(gazeVecX = EyePos_X - HeadPos_X)
dfsimTrim <- dfsimTrim %>% mutate(gazeVecY = EyePos_Y - HeadPos_Y)
dfsimTrim <- dfsimTrim %>% mutate(gazeVecZ = EyePos_Z - HeadPos_Z)

dfsimTrim <- dfsimTrim %>% mutate(dotProd = sum(initialVector * c(gazeVecX,gazeVecY,gazeVecZ)))

initialVector_magnitude <- sqrt(sum(initialVector^2))
dfsimTrim <- dfsimTrim %>% mutate(GazeVecMagnitude = sqrt(sum(c(gazeVecX,gazeVecY,gazeVecZ)^2))) 


dfsimTrim <- dfsimTrim %>%  mutate(cos_theta = dotProd / (initialVector_magnitude * GazeVecMagnitude)) 
dfsimTrim <- dfsimTrim %>%  mutate(theta_radians = acos(cos_theta))

dfsimTrim <- dfsimTrim %>%  mutate(theta_degrees = theta_radians * (180 / pi))






# dfsimTrimFront <- dfsimTrim %>% filter(Time <= dfsim$PACstartTime[1])
# dfsimTrimDurring <- dfsimTrim %>% filter(Time >= dfsim$PACstartTime[1] & Time <= dfsim$PACendTime[1])
# dfsimTrimEnd <- dfsimTrim %>% filter(Time >= dfsim$PACendTime[1])


# Sample data
df <- data.frame(
  x = 1:10,
  temp = rnorm(10, mean = 20, sd = 5),   # Temperature in Celsius
  sales = rnorm(10, mean = 1000, sd = 200)  # Sales in units
)

# Improved plot
ggplot(dfsimTrim, aes(x = Time)) +
  # First line for temperature (Y1) with a smooth line
  geom_line(aes(y = HandPos_X, color = "Hand Movement X-Axis"), size = 1.2) +
  # Second line for sales (Y2) scaled down
  geom_line(aes(y = theta_degrees * (360 / max(theta_degrees, na.rm = TRUE)), color = "Sales (units)"), size = 1.2, linetype = "dashed") +
  
  # Primary y-axis for temperature
  scale_y_continuous(
    name = "Hand Movement (X-Axis)",  # Left axis label
    sec.axis = sec_axis(~ . * (max(dfsimTrim$theta_degrees, na.rm = TRUE) / 360), name = "Sales (units)")  # Secondary y-axis for theta_degrees scaled to 1-360
  ) +
  
  # Customize colors for the lines
  scale_color_manual(
    values = c("Temperature (°C)" = "blue", "Sales (units)" = "red")
  ) +
  
  # Labels
  labs(
    x = "Time",                  # X-axis label
    y = "Temperature (°C)",       # Primary Y-axis label
    color = "Legend"             # Legend title
  ) +
  
  # Improve theme
  theme_minimal() +  # Use a clean, minimalistic theme
  theme(
    # Customize axis title colors
    axis.title.y.left = element_text(color = "blue", size = 12, face = "bold"),
    axis.title.y.right = element_text(color = "red", size = 12, face = "bold"),
    
    # Customize axis tick colors
    axis.text.y.left = element_text(color = "blue", size = 10),
    axis.text.y.right = element_text(color = "red", size = 10),
    
    # Customize the legend
    legend.position = "top",        # Move legend to the top
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    
    # Customize grid lines
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray"),
    panel.grid.minor = element_blank()
  )
