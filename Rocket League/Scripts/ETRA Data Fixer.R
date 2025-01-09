# This script wrangles the post-processing data from the RL competitive analysis, sorts it into sections of gaze strategy
# and analyses the group data

library(plot3D)
library(rgl)
library(dplyr)
library(bit64)
library(stringr)
library(tidyr)
library(ggplot2)


data_files <- list.files(pattern = "P42.csv")

participantDataFile <- data_files[1]
print(participantDataFile)

df <- read.csv(participantDataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)

df <- df %>%
  mutate(diff_from_last = Timestamp - lag(Timestamp)) # Subtract each value from the last value



dfMArk <- df %>% filter(diff_from_last > 20 & Frame != 154564)
#dfMArk <- dfMArk %>% filter(Frame != 55971 & Frame != 62010)


# df <- df %>%
#   mutate(Trial_Num = ntile(Timestamp, 6)) # Split into 6 sections using ntile()
# 
# 


df <- df %>% mutate(Trial)
counter = 2
for (i in 1:nrow(dfMArk)) {
  df <- df %>% mutate(Trial_Num = ifelse(Timestamp > dfMArk$Timestamp[i], counter, Trial_Num))
  counter = counter + 1
}



ggplot(df, aes(x = Timestamp, y = diff_from_last, color = Trial_Num)) +
  geom_point(size = 3) + # Add points with colors based on 'category_car'
  geom_line(aes(group = 1), color = "black") + # Connect points with a black line
  labs(
    title = "Difference from Previous Value by Category",
    x = "Values",
    y = "Difference from Previous",
    color = "Category" # Legend title
  ) +
  theme_minimal()

write.csv(df, "ScoringData_P42_REAL.csv", row.names = FALSE)




