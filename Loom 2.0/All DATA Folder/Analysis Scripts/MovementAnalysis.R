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
library(signal)


data_files <- list.files(pattern = "sdP10.csv")

PACdf <- data.frame(Time = numeric(),
                    Participant = factor(),
                    Condition = factor(),
                    Trial = numeric(),
                    Group = factor(),
                    grabPACcount = numeric(),
                    placePACcount = numeric(),
                    avgGrabPAC = numeric(),
                    avgPlacePAC = numeric(),
                    grabAreaPACcount = numeric(),
                    placeAreaPACcount = numeric(),
                    avgGrabAreaPAC = numeric(),
                    avgPlaceAreaPAC = numeric(),
                    avgGoldCubeGrab = numeric(),
                    totalGoldCubeGrabCount = numeric(),
                    avgBlueCubeGrab = numeric(),
                    totalBlueCubeGrabCount = numeric(),
                    avgRedCubeGrab = numeric(),
                    totalRedCubeGrabCount = numeric(),
                    avgWhiteCubeGrab = numeric(),
                    totalWhiteCubeGrabCount = numeric(),
                    stringsAsFactors = FALSE)

PACAreadf <- data.frame(Time = numeric(),
                        Participant = factor(),
                        Condition = factor(),
                        Trial = numeric(),
                        Group = factor(),
                        grabAreaPACcount = numeric(),
                        placeAreaPACcount = numeric(),
                        avgGrabAreaPAC = numeric(),
                        avgPlaceAreaPAC = numeric(),
                        stringsAsFactors = FALSE)

individualPACdf <- data.frame(Time = numeric(),
                              Participant = factor(),
                              Condition = factor(),
                              Trial = numeric(),
                              Group = factor(),
                              PACtype = factor(),
                              PACtime = numeric(),
                              PACstartTime = numeric(),
                              PACendTime = numeric(),
                              Event = factor(),
                              stringsAsFactors = FALSE)

individualAreaPACdf <- data.frame(Time = numeric(),
                                  Participant = factor(),
                                  Condition = factor(),
                                  Trial = numeric(),
                                  Group = factor(),
                                  PACtype = factor(),
                                  PACtime = numeric(),
                                  PACstartTime = numeric(),
                                  PACendTime = numeric(),
                                  Event = factor(),
                                  stringsAsFactors = FALSE)



# for(f in 1:length(data_files))
# {
#   
  participantDataFile <- data_files[1]
  print(participantDataFile)
  
  
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]
  
  
  # endHandX <- first_instance_row[1,25]
  # endHandY <- first_instance_row[1,26]
  # endHandY <- first_instance_row[1,27]
  # distance <- sqrt((endHandX - currentHandX)^2 + (endHandY - currentHandY)^2 + (endHandY - currentHandZ)^2)
  # velocity <- distance/PACtime
  
  trimDF <- df %>%
    mutate(Time = (Time - Time[1])/10000000) 
  
  trimDF <- trimDF %>%
    dplyr :: filter(Time <= 25 & Time >=24)
  
  xHand <- trimDF$HandPos_X
  yHand <- trimDF$HandPos_Y
  zHand <- trimDF$HandPos_Z
  
  xHead <- trimDF$HeadPos_X
  yHead <- trimDF$HeadPos_Y
  zHead <- trimDF$HeadPos_Z
  
  xRay <- trimDF$RayCast_EndPos_X
  yRay <- trimDF$RayCast_EndPos_Y
  zRay <- trimDF$RayCast_EndPos_Z
  
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(grepl("picked", Event), Event, "none"))
  
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(grepl("P2", ActionEvent), "none", ActionEvent))
  
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(!grepl("P1", ActionEvent), ActionEvent, "none"))
 
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(ActionEvent != "none", "Grab", ActionEvent))
 
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(grepl("dropped", Event), Event, ActionEvent))
  
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(grepl("P1", ActionEvent), "Dropped", ActionEvent))
  
  trimDF <- trimDF %>%
    mutate(ActionEvent = ifelse(ActionEvent == "Grab" | ActionEvent == "Dropped", ActionEvent, "none"))

  # trimDF <- trimDF %>%
  #   mutate(ActionEvent = ifelse(ActionEvent != "none", "grab", ActionEvent))
  # 
  # 
   
  
  x <- xHand
  y <- yHand
  z <- zHand
  
  x <- xHead
  y <- yHead
  z <- zHead
  
  x <- xRay
  y <- yRay
  z <- zRay
  
  plot3d(x, y, z)
  

  
  # Example: Design a low-pass Butterworth filter
  order <- 4  # Filter order
  cutoff_freq <- 20 / (0.5 * 90)  # Normalized cutoff frequency
  
  butterworth_filter <- butter(order, cutoff_freq, type = "low")
  

  testCol <- trimDF$HandPos_X
  
  filtered_signal <- as.numeric(filter(butterworth_filter, testCol))
  
  
  p <- trimDF %>%
    ggplot(aes(x = Time, y = filtered_signal, color = ActionEvent)) +
    geom_line()+
    geom_point(size = .2)+
    # geom_line(aes(y = yHand), color = "red", linetype = "solid") +
    # geom_line(aes(y = zHand), color = "green", linetype = "solid") +
    labs(title = "", x = "Time (S)", y = "x axis hand pos") +
    theme_minimal()
  
  
  #p + geom_point(aes(color = factor(ActionEvent)), size = 3)
  # p + geom_point(aes (y = yHand),(color = factor(ActionEvent)), size = 3)
  # p + geom_point(aes (y = zHand),(color = factor(ActionEvent)), size = 3)
  
  p
  
  # plot3d(x, y, z)
  
# }