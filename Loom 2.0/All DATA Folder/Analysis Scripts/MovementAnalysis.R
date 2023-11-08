library(plot3D)
library(rgl)
library(dplyr)
library(bit64)
library(stringr)



data_files <- list.files(pattern = "sdP10")

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



for(f in 1:length(data_files))
{
  endHandX <- first_instance_row[1,25]
  endHandY <- first_instance_row[1,26]
  endHandY <- first_instance_row[1,27]
  distance <- sqrt((endHandX - currentHandX)^2 + (endHandY - currentHandY)^2 + (endHandY - currentHandZ)^2)
  velocity <- distance/PACtime
}