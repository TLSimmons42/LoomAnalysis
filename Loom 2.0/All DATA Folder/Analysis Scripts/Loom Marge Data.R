# Load necessary library
library(dplyr)
library(bit64)


# Define input and output directories
input_dirBase <- "C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Filtered Data"
input_dir <- "C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Conversion Data"  # Change this to your actual folder path
output_file <- "C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Merged Data/nuP38_merged.csv"  # Change this to your desired output file

# Get all CSV files in the input directory
filePattern = "nuP38(\\D|$)"
csv_files <- list.files(input_dir, pattern = filePattern, full.names = TRUE)
csv_files_Base <- list.files(input_dirBase, pattern = filePattern, full.names = TRUE)

# Initialize an empty list to store data
data_list <- list()
data_list_Base <- list()

# Loop through each file and read it into a list
for (file in csv_files) {
  print(file)
  df <- read.csv(file, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  # if(df$Group[5] == "1" | df$Group[5] == "f"| is.na(df$Group[5])){
  #   
  #   df$Group <- "e"
  # }
  # if(df$Participant[5] == "sdP13"| df$Participant[5] == "sdP2"){
  #   df$Group <- "c"
  #   
  # }
  # if(df$Participant[5] == "sd12" | df$Participant[5] == "nuP15"| df$Participant[5] == "sdP7"| df$Participant[5] == "sdP15"){
  #   df$Sex <- "m"
  # }
  # 
  # if(df$Participant[5] == "nuP18" | df$Participant[5] == "nuP19"){
  #   df$Sex <- "f"
  # }
  # 
  # if(df$Participant[5] == "sdP13"){
  #   df$Age <- 20
  # }
  # if(df$Participant[5] == "sdP1"){
  #   df$Age <- 21
  # }
  # 
  # if(df$Participant[5] == "sd10"){
  #   df <- df %>% mutate(Group = ifelse(Group == 1, "e", Group))
  # }
  # if(df$Participant[5] == "sdP7"){
  #   df <- df %>% mutate(Sex = "m")
  # }
  # 
  
  data_list[[file]] <- df
}

merged_data <- bind_rows(data_list)
#merged_data$Time <- as.numeric(merged_data$Time)

for (file in csv_files_Base) {
  print(file)
  df <- read.csv(file, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  df$EyePos_X <- as.numeric(df$EyePos_X)
  df$EyePos_Y <- as.numeric(df$EyePos_Y)
  df$EyePos_Z <- as.numeric(df$EyePos_Z)
  
  
  
  if(df$Group[5] == "1" | df$Group[5] == "f"| is.na(df$Group[5])){
    
    df$Group <- "e"
  }
  if(df$Participant[5] == "sdP13"| df$Participant[5] == "sdP2"){
    df$Group <- "c"
    
  }
  if(df$Participant[5] == "sd12" | df$Participant[5] == "nuP15"| df$Participant[5] == "sdP7"| df$Participant[5] == "sdP15"){
    df$Sex <- "m"
  }
  
  if(df$Participant[5] == "nuP18" | df$Participant[5] == "nuP19"){
    df$Sex <- "f"
  }
  
  if(df$Participant[5] == "sdP13"){
    df$Age <- 20
  }
  if(df$Participant[5] == "sdP1"){
    df$Age <- 21
  }
  
  if(df$Participant[5] == "sd10"){
    df <- df %>% mutate(Group = ifelse(Group == 1, "e", Group))
  }
  if(df$Participant[5] == "sdP7"){
    df <- df %>% mutate(Sex = "m")
  }
  
  data_list_Base[[file]] <- df
}

Base_merged_data <- bind_rows(data_list_Base)

#write.csv(merged_data, output_file, row.names = FALSE)




# 
# # df2_selected <- df2 %>%
# #   select(Timestamp, Condition, Trial, Participant, col1, col2, col3, col4, col5)  # Replace with actual column names

# Base_merged_data <- Base_merged_data %>%
#   rename("TimeStamp" = "Time")


# Merge df2_selected into df1 using a left join
df_merged <- Base_merged_data %>%
  left_join(merged_data, by = c("Time", "Condition", "Trial", "Participant"))

# Check if everything merged correctly
#print(head(df_merged))

# Save the merged DataFrame
write.csv(df_merged, output_file, row.names = FALSE)

cat("Merged DataFrame saved successfully.\n")


