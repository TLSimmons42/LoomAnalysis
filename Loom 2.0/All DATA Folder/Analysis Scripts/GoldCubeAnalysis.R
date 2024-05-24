library(plot3D)
library(rgl)
library(dplyr)
library(bit64)
library(stringr)
library(plotly)



data_files <- list.files(pattern = ".csv")

strings_to_filter <- c("nuP2_old1","nuP2_old2","nuP2_old3","nuP2_old4")
data_files <- data_files[!(grepl(paste(strings_to_filter, collapse="|"), data_files))]

learnDF <- data.frame()

for(f in 1:length(data_files))
{
  participantDataFile <- data_files[f]
  print(participantDataFile)
  
  
  
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]
  
  if(df$Condition[5] == "solo"){
    learnDF <- rbind(learnDF, df)
  }
} 



trimDF <- learnDF %>% dplyr :: filter(EyePos_X != "N/A")
trimDF <- trimDF %>% filter(as.numeric(EyePos_X) > -20)

# trimDF <- trimDF %>% filter(as.numeric(EyePos_Z) > -1.1)
# trimDF <- trimDF %>% filter(as.numeric(EyePos_Z) < .1)
# trimDF <- trimDF %>% filter(as.numeric(EyePos_X) < 1.9)
# trimDF <- trimDF %>% filter(as.numeric(EyePos_X) > -4)

trimDF <- trimDF %>% filter(as.numeric(EyePos_Z) > -1.1)
trimDF <- trimDF %>% filter(as.numeric(EyePos_Z) < .1)
trimDF <- trimDF %>% filter(as.numeric(EyePos_Y) < 13.5)
trimDF <- trimDF %>% filter(as.numeric(EyePos_Y) > 12.1)


trimDF <- trimDF %>% filter(!grepl("Red",CurrentGazeTarget))
trimDF <- trimDF %>% filter(!grepl("Blue",CurrentGazeTarget))
trimDF <- trimDF %>% filter(!grepl("Neutral",CurrentGazeTarget))
trimDF <- trimDF %>% filter(!grepl("red",CurrentGazeTarget))
trimDF <- trimDF %>% filter(!grepl("white",CurrentGazeTarget))
trimDF <- trimDF %>% filter(!grepl("blue",CurrentGazeTarget))

# trimDF <- trimDF %>% filter(!grepl("Gold",CurrentGazeTarget))
# trimDF <- trimDF %>% filter(!grepl("gold",CurrentGazeTarget))
trimDF <- trimDF %>% filter(CurrentGazeArea == "play_wall")

# trimDF <- trimDF %>% filter(!grepl("none",CurrentGazeTarget))


xEye <- as.numeric(trimDF$EyePos_X)
yEye <- as.numeric(trimDF$EyePos_Y)
zEye <- as.numeric(trimDF$EyePos_Z)

plot3d(xEye, yEye, zEye)

plot_ly(trimDF, x = ~as.numeric(trimDF$EyePos_X), y = ~as.numeric(trimDF$EyePos_Y), z = ~as.numeric(trimDF$EyePos_Z),
        color = ~trimDF$CurrentGazeTarget, type = "scatter3d", mode = "markers", size = 1)


