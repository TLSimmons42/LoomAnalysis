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


data_files <- list.files(pattern = "P14")

participantDataFile <- data_files[1]
print(participantDataFile)


df <- read.csv(participantDataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)
#df <- df[!duplicated(df$Time), ]
