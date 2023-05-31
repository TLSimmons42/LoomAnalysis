library(plot3D)
library(rgl)
library(dplyr)
library(bit64)

participantDataFile <- "loomtest1 (1).csv"

df <- read.csv(participantDataFile, colClasses=c("TimeStamp" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)


#shortDF <- df[(df$CurrentGazeArea == "play_wall" | df$CurrentGazeArea == "build_wall" | df$CurrentGazeArea == "view_wall") & df$CurrentGazeTarget == "none", ]
#shortDF <- df[(df$CurrentGazeArea == "play_wall" | df$CurrentGazeArea == "build_wall" | df$CurrentGazeArea == "view_wall"), ]
shortDF <- df[df$CurrentGazeArea == "play_wall"& df$CurrentGazeTarget == "none",]

# x <- df[,13]
# y <- df[,14]
# z <- df[,15]

x <- shortDF[,13]
y <- shortDF[,14]
z <- shortDF[,15]

plot3d(x, y, z)
