# This script wrangles the post-processing data from the RL competitive analysis, sorts it into sections of gaze strategy
# and analyses the group data

library(plot3D)
library(rgl)
library(dplyr)
library(bit64)
library(stringr)
library(tidyr)
library(ggplot2)

library(tidyverse)
library(afex) # For ANOVA
library(emmeans) # For post-hoc comparisons

# dfAllData <- data.frame(Participant = factor(),
#                         Group = factor(),
#                         Trial = factor(),
#                         Trial = numeric(),
#                         Displacement = numeric(),
#                         Time = numeric(),
#                         Velocity = numeric(),
#                         stringsAsFactors = FALSE)

gazeStratTimes <- data.frame(Participant = factor(),
                            Trial = numeric(),
                            Group = factor(),
                            Rank = factor(),
                            GazeStrat = factor(),
                            GazeWinner = factor(),
                            stratTime = numeric(),
                            gazeStratStartTime = numeric(),
                            gazeStratEndTime = numeric(),
                            frameStart = numeric(),
                            frameEnd = numeric(),
                            stringsAsFactors = FALSE)

dfAllData = data.frame()

data_files <- list.files(pattern = ".csv")
for(f in 1:length(data_files))
{
  participantDataFile <- data_files[f]
  print(participantDataFile)
  
  df <- read.csv(participantDataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  #df <- df[!duplicated(df$Time), ]
  
  Participant <- df$Participant[5]
  Group <- df$Group[5]
  Rank <- df$Rank[5]
  
  # dfTrim <- data.frame()
  dfTrim <- df %>% filter(Gaze_Strat != "Missing Gaze Data")
  dfTotal <- nrow(df)
  dfFiltTotal <- nrow(dfTrim)
  totalDataPercent <- dfFiltTotal/dfTotal
  ParticipantGood <- TRUE
  print(totalDataPercent)
  if(totalDataPercent < .85){
    ParticipantGood <- FALSE
    print("not gonna happen")
  }
  
  
  gazeStratStartTime <- ""
  gazeStratEndTime <- ""
  frameStart <- ""
  frameEnd <- ""
  currentGazeStrat <- ""
  Trial <- ""
  GazeWinner <- ""
  
  looking4New <- TRUE
  looking4Target <- FALSE
  looking4CGP <- FALSE
  itsATie <- FALSE
  otherGaze <- FALSE

  currentTrial <- 0

  
  if(ParticipantGood){
    
    for (i in 1:nrow(dfTrim)){
      
      currentGazeStrat <- dfTrim$Gaze_Strat[i]
      
      if(!looking4New){
        
        if(currentTrial == dfTrim$Trial_Num[i]){
          if(looking4Target & (currentGazeStrat == "Looking at target" | currentGazeStrat == "Scoring Tie")){
            next
          }
          if(looking4CGP & (currentGazeStrat == "Looking at CGP" | currentGazeStrat == "Scoring Tie")){
            next
          }
          if(otherGaze & (currentGazeStrat == "No Winner")){
            next
          }
          if(itsATie & (currentGazeStrat == "Looking at CGP" | currentGazeStrat == "Scoring Tie" | currentGazeStrat == "Looking at target")){
            if(currentGazeStrat == "Looking at CGP"){
              otherGaze <- FALSE
              looking4CGP <- TRUE
            }
            if(currentGazeStrat == "Looking at target"){
              otherGaze <- FALSE
              looking4Target <- TRUE
            }
            next
          }
        }
        
          gazeStratEndTime <- dfTrim$Timestamp[i-1]
          frameEnd <- dfTrim$Frame[i-1]
          GazeStrat <- dfTrim$Gaze_Strat[i-1]
          stratTime <- gazeStratEndTime - gazeStratStartTime
          Trial <- currentTrial
          
          newRow <- data.frame(Participant, Trial, Group, Rank, GazeStrat, GazeWinner, stratTime, gazeStratStartTime, 
                               gazeStratEndTime, frameStart, frameEnd)
          gazeStratTimes <<- rbind(gazeStratTimes, newRow)
          
          looking4New <- TRUE
          looking4Target <- FALSE
          looking4CGP <- FALSE
          itsATie <- FALSE
          otherGaze <- FALSE
      }
          
      if(looking4New){
        currentTrial <- dfTrim$Trial_Num[i]
        firstGazeStrat <- currentGazeStrat
        GazeWinner <- dfTrim$Winner[i]
        if(firstGazeStrat == "Looking at target"){
          looking4Target <- TRUE
        }
        if(firstGazeStrat == "Looking at CGP"){
          looking4CGP <- TRUE
        }
        if(firstGazeStrat == "Scoring Tie"){
          itsATie <- TRUE
        }
        if(firstGazeStrat == "No Winner"){
          otherGaze <- TRUE
        }
        
        
        gazeStratStartTime <- dfTrim$Timestamp[i]
        frameStart <- dfTrim$Frame[i]
        
        looking4New <- FALSE
      }
    }
  }
  
}

# Data Analysis 


# Duration analysis_________________________________________________________
trimStratTimes <- gazeStratTimes %>% filter(stratTime > .1)
#trimStratTimes <- trimStratTimes %>% filter(GazeStrat == "Looking at CGP")
trimStratTimes <- trimStratTimes %>% filter(GazeStrat == "Looking at target")


analysisDurationDF <- trimStratTimes %>% group_by(Participant, Group, Trial) %>%
  summarise(avgStratDuration = mean(stratTime))
  
anova_results <- aov_ez(
  id = "Participant",       # Subject identifier
  dv = "avgStratDuration",        # Dependent variable
  between = "Group",    # Between-subjects factor
  within = "Trial",      # Within-subjects factor
  data = analysisDurationDF
)

# Display ANOVA results
print(anova_results)

# Percentage analysis_________________________________________________________
trimStratTimes <- gazeStratTimes %>% filter(stratTime > .1)

analysisPercentageDF <- trimStratTimes %>% group_by(Participant, Group, Trial, GazeStrat) %>%
  summarise(totalStratDuration = sum(stratTime))

analysisPercentageDF <- analysisPercentageDF %>%
  group_by(Participant, Trial) %>%
  mutate(total_values = sum(totalStratDuration)) %>%
  mutate(StratPercentage = totalStratDuration/total_values)


#trimStratTimes <- trimStratTimes %>% filter(GazeStrat == "Looking at CGP")
analysisPercentageDF <- analysisPercentageDF %>% filter(GazeStrat == "Looking at target")

anova_results <- aov_ez(
  id = "Participant",       # Subject identifier
  dv = "StratPercentage",        # Dependent variable
  between = "Group",    # Between-subjects factor
  within = "Trial",      # Within-subjects factor
  data = analysisPercentageDF
)
print(anova_results)

analysisPercentageDF %>%
  group_by(Group, Trial) %>%
  summarize(mean_score = mean(StratPercentage), 
            sd_score = sd(StratPercentage), 
            n = n(),
            ci_lower = mean_score - qt(0.975, df = n - 1) * (sd_score / sqrt(n)),
            ci_upper = mean_score + qt(0.975, df = n - 1) * (sd_score / sqrt(n)),
            .groups = "drop") %>%
  ggplot(aes(x = Trial, y = mean_score, group = Group, color = Group)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(
    title = "Mixed-Methods ANOVA Results",
    x = "Time",
    y = "Performance Scores",
    color = "Group"
  ) +
  theme_minimal()

# testingDF <- trimStratTimes %>% filter(Group == "Beginner")
# unique(testingDF$Participant)

