library(plot3D)
library(rgl)
library(dplyr)
library(bit64)

participantDataFile <- "loomtest1.csv"
#participantDataFile <- data_files[f]
df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)

playWalldf = df[df$CurrentGazeArea == "build_wall" | df$CurrentGazeArea == "view_wall" | df$CurrentGazeArea == "play_wall", ]


# x <- df[,13]
# y <- df[,14]
# z <- df[ ,15]

x <- playWalldf[,13]
y <- playWalldf[,14]
z <- playWalldf[ ,15]

plot3d(x, y, z)