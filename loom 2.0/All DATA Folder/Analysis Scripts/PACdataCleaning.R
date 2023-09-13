library(plot3D)
library(rgl)
library(dplyr)
library(bit64)



data_files <- list.files(pattern = "sdP8_old1")

for(f in 1:length(data_files))
{
  participantDataFile <- data_files[f]
  print(participantDataFile)
  
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]
  

  # Find all of the Picked events
  trimedGrabDF <- df[grep("picked", df$Event), ]
  trimedGrabDF <- trimedGrabDF %>% filter(CurrentGazeArea == "play_wall")
  
  for(i in 1:length(trimedGrabDF))
  {
    currentTime <- trimedGrabDF[i,1]
    subDF <- df %>% filter(Time < (10000000+currentTime) & Time > (currentTime-10000000) )
    
    
    input_string <- trimedGrabDF[i,10]
    
    # Find the position of " was picked up"
    pos <- regexpr(" was picked up", input_string)
    
    # Extract the substring before the target text
    result <- substr(input_string, 1, pos - 1)
    
    # Print the result
    print(result)

    
    first_instance <- which(subDF$CurrentGazeTarget == result)[1]
    first_instance_row <- subDF[first_instance, ]
    print((currentTime - first_instance_row[1,1])/10000)
    
    
    
    
  }
  

}



