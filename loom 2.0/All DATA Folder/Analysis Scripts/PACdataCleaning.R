library(plot3D)
library(rgl)
library(dplyr)
library(bit64)



participantDataFile <- "sdP8_old2.csv"
data_files <- list.files(pattern = "sdP8_old2")

combined_data <- data.frame()
for(f in 1:length(data_files))
{
  participantDataFile <- data_files[f]
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  combined_data <- bind_rows(combined_data, df)  # Combine data

}
combined_data <- combined_data[!duplicated(combined_data$Time), ]
df <- combined_data


# 
# df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
# df <- df[!duplicated(df$Time), ]

df$EyePos_X <- as.numeric(df$EyePos_X)
df$EyePos_Y <- as.numeric(df$EyePos_Y)
df$EyePos_Z <- as.numeric(df$EyePos_Z)


#trimedDF <- df %>% filter((round(EyePos_X, 2) > -5 & CurrentGazeArea == "play_wall" & round(EyePos_X, 2) < 2.5 ) | CurrentGazeArea == "build_wall"| CurrentGazeArea == "view_wall" | CurrentGazeArea == "background_wall")
trimedDF <- df %>% filter(CurrentGazeArea == "build_wall" & EyePos_X >6 & EyePos_Y > 6 & CurrentGazeTarget == "GameObject")

# x <- df[,13]
# y <- df[,14]
# z <- df[,15]

x <- trimedDF[,13]
y <- trimedDF[,14]
z <- trimedDF[,15]

plot3d(x, y, z)



