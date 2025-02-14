# this is looking at the velocity and magnitude between eye-points
library(plot3D)
library(rgl)
library(dplyr)
library(bit64)
library(stringr)
library(tidyr)
library(ggplot2)

# dfAllData <- data.frame(Participant = factor(),
#                         Group = factor(),
#                         Trial = factor(),
#                         Trial = numeric(),
#                         Displacement = numeric(),
#                         Time = numeric(),
#                         Velocity = numeric(),
#                         stringsAsFactors = FALSE)

dfAllData = data.frame()

data_files <- list.files(pattern = "P26.csv")
for(f in 1:length(data_files))
{
  participantDataFile <- data_files[f]
  print(participantDataFile)
  
  df <- read.csv(participantDataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]
  
  Participant <- df$Participant[5]
  Group <- df$Group[5]
  Trial <- df$Trial_Num[5]
  
  dfMorph <- df %>% mutate(dx = GazePoint_X - lag(GazePoint_X, default = 0))
  dfMorph <- dfMorph %>% mutate(dy = GazePoint_Y - lag(GazePoint_Y, default = 0))
  dfMorph <- dfMorph %>% mutate(dt = Timestamp - lag(Timestamp, default = 0))
  dfMorph <- dfMorph %>% mutate(Displacement = sqrt(dx^2 + dy^2))
  dfMorph <- dfMorph %>% mutate(Velocity = Displacement/dt)

  
  dfAllData <- rbind(dfAllData, dfMorph)
  
  # for (i in 2:nrow(df)) {
  #   print(i)
  #   
  #   
  #   dx <- df$GazePoint_X[i] - df$GazePoint_X[i-1]
  #   dy <- df$GazePoint_Y[i] - df$GazePoint_Y[i-1]
  #   Time <- df$Timestamp[i] - df$Timestamp[i-1]
  #   
  #   Displacement = sqrt(dx^2 + dy^2)
  #   Velocity = Displacement/Time
  #   
  #   newPartRow <- data.frame(Participant, Group, Trial,Displacement, Time, Velocity)
  #   dfAllData <- rbind(dfAllData, newPartRow)
  # }
  
  
}
# 
dfAllDataFilt <- dfAllData %>% filter(!is.na(Velocity))

dfAllDataFilt <- dfAllDataFilt %>% group_by(Participant, Trial_Num)%>%
  mutate(ModTime = Timestamp  - Timestamp[1])

dfAllDataFilt <- dfAllDataFilt %>% filter(dt < 1)


p <- ggplot(dfHisto, aes(x = ModTime, y = Velocity, color = Trial_Num)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Fitted regression line with confidence interval
  theme_minimal() +
  labs(title = "Velocity between gaze points", x = "Time (s)", y = "Velocity (pixels/second)") 
  #theme(legend.position = "none")
p

model <- lm(Velocity ~ ModTime, data = dfAllDataFilt)

# Summary of the model
summary(model)


mean_value <- mean(dfAllDataFilt$Velocity)
sd_value <- sd(dfAllDataFilt$Velocity)

dfHisto <- dfAllDataFilt %>%
  filter(abs(Velocity - mean_value) <= 3 * sd_value)


dfHisto <- dfAllDataFilt
dfHisto <- dfHisto %>% filter(Velocity <= 2000)
dfHisto <- dfHisto %>% filter(Velocity > 0)

hist(
  dfHisto$Velocity,
  breaks = 20,        # Number of bins
  main = "Histogram of Values (0 to 3)",  # Title of the histogram
  xlab = "Values",    # Label for the x-axis
  col = "skyblue",    # Color of the bars
  border = "black"    # Border color of the bars
)




