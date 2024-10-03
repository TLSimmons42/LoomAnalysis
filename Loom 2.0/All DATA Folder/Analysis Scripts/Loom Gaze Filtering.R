library(plot3D)
library(rgl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(ggpubr)
library(ez)
library(cowplot)
library(ggsci)
library(gridExtra)
library(ggsignif)
library(bit64)
library(plotly)



data_files <- list.files(pattern = "nuP15.csv")
data_files[]

participantDataFile <- data_files[1]
print(participantDataFile)



df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
df <- df[!duplicated(df$Time), ]

df <- df %>% filter(EyePos_X != "N/A")
df$EyePos_X <- as.numeric(df$EyePos_X)
df$EyePos_Y <- as.numeric(df$EyePos_Y)
df$EyePos_Z <- as.numeric(df$EyePos_Z)

dfFilt <- df
dfFilt <- df %>%
  filter(EyePos_X > -5) %>%
  filter(EyePos_X != 0) %>%
  mutate(distance_from_prev_x = EyePos_X - lag(EyePos_X)) %>%
  mutate(distance_from_prev_x = abs(distance_from_prev_x))%>%
  mutate(distance_from_prev_y = EyePos_X - lag(EyePos_X)) %>%
  mutate(distance_from_prev_y = abs(distance_from_prev_y)) %>%
  mutate(disance_from_prev = sqrt((lag(EyePos_X) - EyePos_X)^2 + (lag(EyePos_Y) - EyePos_Y)^2 + (lag(EyePos_Z) - EyePos_Z)^2))


plot(dfFilt$Time, dfFilt$disance_from_prev)

plot3d(dfFilt$EyePos_X, dfFilt$EyePos_Y, dfFilt$EyePos_Z)


dfWall <- dfFilt %>% filter(CurrentGazeArea == "build_wall")

dfFilt <- dfFilt %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "background_wall" & EyePos_X == 1.6 & EyePos_Z >= -15 & EyePos_Z <= 16, "play_wall", CurrentGazeArea))
dfFilt <- dfFilt %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "background_wall" & EyePos_X <= 15.5 & EyePos_X >= 9.5, "view_wall", CurrentGazeArea))
dfFilt <- dfFilt %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "background_wall" & EyePos_X <= 15.5 & EyePos_X >= 10, "build_wall", CurrentGazeArea))

dfFilt <- dfFilt %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "view_wall" & EyePos_Z > 0 & EyePos_Z < 12, "build_wall", CurrentGazeArea ))
dfFilt <- dfFilt %>% mutate(CurrentGazeArea = ifelse(CurrentGazeArea == "view_wall" & EyePos_Z > 12, "background_wall", CurrentGazeArea ))

# dfPlot <- dfFilt %>% filter(Time < 638338369614321351)
dfPlot <- dfFilt %>% filter(Time >= 638338370339538401 & Time <= 638338370416430122)


dfPlot <- dfPlot %>% mutate(pointColor = ifelse(Time >= 638338370398863727 & Time <= 638338371172093471, "blue", "yellow"))
plot_ly(dfPlot, x = ~EyePos_X, y = ~EyePos_Y, z = ~EyePos_Z, color = ~ CurrentGazeArea, type = "scatter3d", mode = "markers")


#__________________________________________________________________________________________________________________
# Gaze Angle Plots

dfPlot <- dfPlot %>%mutate(Gaze_Angle_Distance = sqrt((HeadPos_X - eyeAngleX)^2 + (HeadPos_Y - eyeAngleY)^2 + (HeadPos_Z - EyePos_Z)^2))

dfPlot <- dfPlot %>%mutate(Gaze_Angle_Distance = sqrt((HeadPos_X - EyePos_X)^2 + (HeadPos_Y - EyePos_Y)^2 + (HeadPos_Z - EyePos_Z)^2))

dfPlot <- dfPlot %>% mutate(GazeAreaColor = "none")
dfPlot <- dfPlot %>% mutate(GazeAreaColor = ifelse(CurrentGazeArea == "play_wall", "red",GazeAreaColor))
dfPlot <- dfPlot %>% mutate(GazeAreaColor = ifelse(CurrentGazeArea == "build_wall", "blue",GazeAreaColor))
dfPlot <- dfPlot %>% mutate(GazeAreaColor = ifelse(CurrentGazeArea == "view_wall", "yellow",GazeAreaColor))
dfPlot <- dfPlot %>% mutate(GazeAreaColor = ifelse(CurrentGazeArea == "background_wall", "black",GazeAreaColor))

p <- plot_ly()

# Loop over each row and add a line connecting the two points
for (i in 1:nrow(dfPlot)) {
  p <- p %>%
    add_trace(
      # x = c(dfPlot$HeadPos_X[i], dfPlot$eyeAngleX[i]),  # X coordinates of both sets for the same row
      # y = c(dfPlot$HeadPos_Y[i], dfPlot$eyeAngleY[i]),  # Y coordinates of both sets for the same row
      # z = c(dfPlot$HeadPos_Z[i], dfPlot$eyeAngleZ[i]),  # Z coordinates of both sets for the same row

      x = c(dfPlot$HeadPos_X[i], dfPlot$EyePos_X[i]),  # X coordinates of both sets for the same row
      y = c(dfPlot$HeadPos_Y[i], dfPlot$EyePos_Y[i]),  # Y coordinates of both sets for the same row
      z = c(dfPlot$HeadPos_Z[i], dfPlot$EyePos_Z[i]),  # Z coordinates of both sets for the same row

      
      type = 'scatter3d',
      mode = 'lines+markers',
      marker = list(size = 4, color = dfPlot$GazeAreaColor[i]),    # Marker size for the points
      #line = list(width = 3, color = dfPlot$GazeAreaColor[i])      # Line width connecting the points
      line = list(width = 3, color = "blue")      # Line width connecting the points
      
      
    ) %>%
    add_trace(
      x = c(dfPlot$HeadPos_X[i], dfPlot$eyeAngleX[i]),  # X coordinates of both sets for the same row
      y = c(dfPlot$HeadPos_Y[i], dfPlot$eyeAngleY[i]),  # Y coordinates of both sets for the same row
      z = c(dfPlot$HeadPos_Z[i], dfPlot$eyeAngleZ[i]),  # Z coordinates of both sets for the same row
  
  
      
      type = 'scatter3d',
      mode = 'lines+markers',
      marker = list(size = 4, color = dfPlot$GazeAreaColor[i]),    # Marker size for the points
      line = list(width = 3, color = "red")      # Line width connecting the points
    )
}

# Customize the layout
p <- p %>%
  layout(scene = list(
    xaxis = list(title = 'X'),
    yaxis = list(title = 'Y'),
    zaxis = list(title = 'Z'),
    title = '3D Plot with Connected Points'
  ))

# Display the plot
p


b <- dfPlot %>%
  mutate(Index = row_number())%>%
  ggplot(aes(x = Index, y = HeadRot_Y, size = 1, color = CurrentGazeArea)) +
  #geom_line(size = 2)+
  geom_point()+
  # geom_line(aes(y = yHand), color = "red", linetype = "solid") +
  # geom_line(aes(y = zHand), color = "green", linetype = "solid") +
  labs(title = "", x = "Time (s)", y = "Rotation (Quaternions)") +
  theme_minimal() 
#p + geom_point(aes(color = factor(ActionEvent)), size = 3)
# p + geom_point(aes (y = yHand),(color = factor(ActionEvent)), size = 3)
# p + geom_point(aes (y = zHand),(color = factor(ActionEvent)), size = 3)
b