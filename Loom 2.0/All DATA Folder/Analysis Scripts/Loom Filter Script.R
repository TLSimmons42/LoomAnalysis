# Load necessary library
library(dplyr)
library(bit64)


# Define input and output directories
input_dir <- "C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Raw Data"  # Change this to your actual folder path
output_dir <- "C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Filtered Data"  # Change this to your actual folder path

# Create the output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Get all CSV files in the input directory
csv_files <- list.files(input_dir, pattern = "nuP8", full.names = TRUE)

# Loop through each file, process it, and save the modified version
for(f in 1:length(csv_files)){
  
  participantDataFile <- csv_files[f]
  print(participantDataFile)
  
  
  
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]
  # Read in the CSV

  # Create output filename
   file_name <- paste0("filt_", basename(participantDataFile))
   output_path <- file.path(output_dir, file_name)
  
   # Write the modified CSV
   write.csv(df, output_path, row.names = FALSE)
  
   
}

cat("All files processed and saved in", output_dir, "\n")