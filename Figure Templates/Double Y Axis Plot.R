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



# Sample data
df <- data.frame(
  x = 1:10,
  temp = rnorm(10, mean = 20, sd = 5),   # Temperature in Celsius
  sales = rnorm(10, mean = 1000, sd = 200)  # Sales in units
)

# Improved plot
ggplot(df, aes(x = x)) +
  # First line for temperature (Y1) with a smooth line
  geom_line(aes(y = temp, color = "Temperature (°C)"), size = 1.2) +
  # Second line for sales (Y2) scaled down
  geom_line(aes(y = sales / 50, color = "Sales (units)"), size = 1.2, linetype = "dashed") +
  
  # Primary y-axis for temperature
  scale_y_continuous(
    name = "Temperature (°C)",   # Left axis label
    sec.axis = sec_axis(~.*50, name = "Sales (units)")  # Secondary y-axis
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
