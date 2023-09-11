library(plot3D)
library(rgl)
library(dplyr)
library(bit64)



data_files <- list.files(pattern = "sdP8")

for(f in 1:length(data_files))
{
  participantDataFile <- data_files[f]
  print(participantDataFile)
  
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]
  
  
  #trimedDF <- df %>% filter((round(EyePos_X, 2) > -5 & CurrentGazeArea == "play_wall" & round(EyePos_X, 2) < 2.5 ) | CurrentGazeArea == "build_wall"| CurrentGazeArea == "view_wall" | CurrentGazeArea == "background_wall")
  
  trimedGrabDF <- df[grep("picked", df$Event), ]


  
  
  
}



