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
library(emmeans)
#library(emmeans) # For post-hoc comparisons

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
  
  df <- df %>% mutate(Gaze_Strat = ifelse(GazePoint_X == -1,"Missing Gaze Data", Gaze_Strat))
  df <- df %>% mutate(Gaze_Strat = ifelse(GazePoint_X > 1920,"Missing Gaze Data", Gaze_Strat))
  df <- df %>% mutate(Gaze_Strat = ifelse(GazePoint_X < 0,"Missing Gaze Data", Gaze_Strat))
  df <- df %>% mutate(Gaze_Strat = ifelse(GazePoint_Y> 1080,"Missing Gaze Data", Gaze_Strat))
  df <- df %>% mutate(Gaze_Strat = ifelse(GazePoint_Y < 0,"Missing Gaze Data", Gaze_Strat))
  
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
  if(totalDataPercent < .70){
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
  GazeStrat <- ""
  
  looking4New <- TRUE
  looking4Target <- FALSE
  looking4CGP <- FALSE
  itsATie <- FALSE
  otherGaze <- FALSE
  timeCheck <- TRUE

  currentTrial <- 0
  currentTime <- 0
  previousTime <- 0
  
  
  if(ParticipantGood){
    
    for (i in 1:nrow(dfTrim)){
      
      currentGazeStrat <- dfTrim$Gaze_Strat[i]
      currentTime <- dfTrim$Timestamp[i]
      previousTime <- dfTrim$Timestamp[i-1]
      
      if(!looking4New){
        
        if(currentTrial == dfTrim$Trial_Num[i] & ((currentTime - previousTime)<5)){
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
            GazeWinner <- dfTrim$Winner[i]
            if(currentGazeStrat == "Looking at CGP"){
              GazeStrat <- currentGazeStrat
              itsATie <- FALSE
              looking4CGP <- TRUE
            }
            if(currentGazeStrat == "Looking at target"){
              GazeStrat <- currentGazeStrat
              itsATie <- FALSE
              looking4Target <- TRUE
              GazeWinner <- dfTrim$Winner[i]
              
            }
            next
          }
        }
        
          gazeStratEndTime <- dfTrim$Timestamp[i-1]
          frameEnd <- dfTrim$Frame[i-1]
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
          GazeStrat <- currentGazeStrat
          looking4Target <- TRUE
        }
        if(firstGazeStrat == "Looking at CGP"){
          GazeStrat <- currentGazeStrat
          looking4CGP <- TRUE
        }
        if(firstGazeStrat == "Scoring Tie"){
          itsATie <- TRUE
        }
        if(firstGazeStrat == "No Winner"){
          GazeStrat <- currentGazeStrat
          otherGaze <- TRUE
        }
        
        
        gazeStratStartTime <- dfTrim$Timestamp[i]
        frameStart <- dfTrim$Frame[i]
        
        looking4New <- FALSE
      }
    }
  }
  
}
# tempGazeStratTimes <<- rbind(tempGazeStratTimes, gazeStratTimes)
# 
# tempGazeStratTimes <- gazeStratTimes
# gazeStratTimes <- tempGazeStratTimes

gazeStratTimes <- gazeStratTimes %>% mutate(Group = ifelse(Participant == "P43", "Expert", Group))
gazeStratTimes <- gazeStratTimes %>% mutate(Group = ifelse(Participant == "P37", "Beginner", Group))
gazeStratTimes <- gazeStratTimes %>% mutate(Group = ifelse(Participant == "P23", "Beginner", Group))


# Data Analysis 

# tempGazeStratTimes <- gazeStratTimes %>% filter(Participant == "P09")
# write.csv(gazeStratTimes, "gazeStratTimes 1-5.csv", row.names = FALSE)

# Duration analysis_________________________________________________________
trimStratTimes <- gazeStratTimes %>% filter(stratTime > .1)
trimStratTimes <- trimStratTimes %>% filter(GazeStrat == "Looking at target")

trimStratTimes <- trimStratTimes %>% filter(Participant != "P01"& Participant != "P02" & Participant != "P03" & Participant != "P04" & Participant != "P05"& 
                                                          Participant != "P06" & Participant != "P07" & Participant != "P08" & Participant != "P09" )

analysisDurationDF <- trimStratTimes %>% group_by(Participant, Group, Trial) %>%
  summarise(avgStratDuration = mean(stratTime))
 
anova_results <- aov_ez(
  id = "Participant",       # Subject identifier
  dv = "avgStratDuration",        # Dependent variable
  between = "Group",    # Between-subjects factor
  within = "Trial",      # Within-subjects factor
  data = analysisDurationDF
)
print(anova_results)

emmeans_results_trial <- emmeans(anova_results, pairwise ~ Trial, adjust = "bonferroni")
print(emmeans_results_trial$contrasts)
# pairwise_results <- pairwise.t.test(analysisDurationDF$avgStratDuration, analysisDurationDF$Trial, 
#                                     paired = TRUE, # Paired because trials are repeated measures
#                                     p.adjust.method = "bonferroni") # Use Bonferroni or other corrections
# pairwise_results

p1 <- analysisDurationDF %>%
  group_by(Group, Trial) %>%
  summarize(mean_score = mean(avgStratDuration), 
            sd_score = sd(avgStratDuration), 
            n = n(),
            ci_lower = mean_score - qt(0.975, df = n - 1) * (sd_score / sqrt(n)),
            ci_upper = mean_score + qt(0.975, df = n - 1) * (sd_score / sqrt(n)),
            .groups = "drop") %>%
  ggplot(aes(x = Trial, y = mean_score, group = Group, color = Group)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(
    title = "Target-Looking",
    x = "Trial",
    y = "Time (s)",
    color = "Group"
  ) +
  theme_minimal()+
  scale_y_continuous(limits = c(.25, .95))+
  theme(legend.position = "none")

combined_plot <- p1 + p2 + p3
combined_plot
#ggsave("Center Looking Individual Gaze Strategy Duration.pdf")

# Percentage analysis_______________________________________________________________________________

trimStratTimes <- gazeStratTimes %>% filter(stratTime > .1)
trimStratTimes <- trimStratTimes %>% mutate(Difficulty = ifelse(Trial == 1 | Trial == 2, "Easy",Trial))
trimStratTimes <- trimStratTimes %>% mutate(Difficulty = ifelse(Trial == 3 | Trial == 4, "Medium",Difficulty))
trimStratTimes <- trimStratTimes %>% mutate(Difficulty = ifelse(Trial == 5 | Trial == 6, "Hard",Difficulty))


analysisPercentageDF <- trimStratTimes %>% group_by(Participant, Group, Difficulty, GazeStrat) %>%
  summarise(totalStratDuration = sum(stratTime))

analysisPercentageDF <- analysisPercentageDF %>%
  group_by(Participant, Difficulty) %>%
  mutate(total_values = sum(totalStratDuration)) %>%
  mutate(StratPercentage = totalStratDuration/total_values) %>%
  mutate(StratPercentage = StratPercentage * 100)

analysisPercentageDF <- analysisPercentageDF %>% filter(Participant != "P01"& Participant != "P02" & Participant != "P03" & Participant != "P04" & Participant != "P05"& 
                                                         Participant != "P06" & Participant != "P07" & Participant != "P08" & Participant != "P09" )

#trimStratTimes <- trimStratTimes %>% filter(GazeStrat == "Looking at CGP")
analysisPercentageDF <- analysisPercentageDF %>% filter(GazeStrat == "Looking at target")


anova_results <- aov_ez(
  id = "Participant",       # Subject identifier
  dv = "StratPercentage",        # Dependent variable
  between = "Group",    # Between-subjects factor
  within = "Difficulty",      # Within-subjects factor
  data = analysisPercentageDF
)
print(anova_results)

# pairwise_results <- pairwise.t.test(analysisPercentageDF$StratPercentage, analysisPercentageDF$Difficulty, 
#                                     paired = TRUE, # Paired because trials are repeated measures
#                                     p.adjust.method = "bonferroni") # Use Bonferroni or other corrections
# pairwise_results

emmeans_results <- emmeans(anova_results, pairwise ~  Difficulty, adjust = "bonferroni")
print(emmeans_results$contrasts)
emmeans_results <- emmeans(anova_results, pairwise ~ Group | Group, adjust = "bonferroni")
print(emmeans_results$contrasts)

p3 <- analysisPercentageDF %>%
  mutate(Difficulty = factor(Difficulty, levels = c("Easy", "Medium", "Hard"))) %>%  # Set the order of Difficulty
  group_by(Group, Difficulty) %>%
  summarize(mean_score = mean(StratPercentage), 
            sd_score = sd(StratPercentage), 
            n = n(),
            ci_lower = mean_score - qt(0.975, df = n - 1) * (sd_score / sqrt(n)),
            ci_upper = mean_score + qt(0.975, df = n - 1) * (sd_score / sqrt(n)),
            .groups = "drop") %>%
  ggplot(aes(x = Difficulty, y = mean_score, group = Group, color = Group)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(
    title = "Other",
    x = "",
    y = "",
    color = "Group"
  ) +
  theme_minimal()+
  scale_y_continuous(limits = c(0, 65)) +
  theme(legend.position = "none")
p2

combined_plot <- p1  + p2 + p3
combined_plot
#ggsave("Center Looking Gaze Strategy Percentage Use.pdf")



# Combo and Target analysis_______________________________________________________________________________
trimStratTimes <- gazeStratTimes %>% filter(stratTime > .1)
trimStratTimes <- trimStratTimes %>% filter(GazeStrat == "Looking at target")

trimStratTimes <- trimStratTimes %>% filter(GazeWinner == "Self" | GazeWinner == "Ball" |GazeWinner == "Opponent" |GazeWinner == "Teammate" |
                                              GazeWinner == "Boost" |GazeWinner == "Goal Post" )

trimStratTimes <- trimStratTimes %>% filter(Participant != "P01"& Participant != "P02" & Participant != "P03" & Participant != "P04" & Participant != "P05"& 
                                              Participant != "P06" & Participant != "P07" & Participant != "P08" & Participant != "P09" )

analysisTargetDF <- trimStratTimes %>% group_by(Participant, GazeWinner) %>%
  summarise(totalStratDuration = sum(stratTime))

analysisTargetDF <- trimStratTimes %>%
  group_by(Participant,Group, GazeWinner) %>%
  summarise(totalStratDuration = sum(stratTime), .groups = "drop")

# Calculate the total strategy duration per Participant
analysisTargetDF <- analysisTargetDF %>%
  group_by(Participant) %>%
  mutate(participantTotalDuration = sum(totalStratDuration)) %>%
  ungroup()

# Calculate the percentage use for each GazeWinner category
analysisTargetDF <- analysisTargetDF %>%
  mutate(percentageUse = (totalStratDuration / participantTotalDuration) * 100)



ggplot(analysisTargetDF, aes(x = GazeWinner, y = percentageUse, fill = Group)) +
  geom_boxplot() +
  labs(
    title = "Percentage Use of Target-Looking Winners",
    x = "Gaze Winner Category",
    y = "Percentage Use (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) #+
  #scale_fill_brewer(palette = "Set1") # Optional: Adjust the color palette








trimStratTimes <- gazeStratTimes %>% filter(stratTime > .1)
trimStratTimes <- trimStratTimes %>% filter(GazeStrat == "Looking at CGP")
trimStratTimes <- trimStratTimes %>% filter(Participant != "P01"& Participant != "P02" & Participant != "P03" & Participant != "P04" & Participant != "P05"& 
                                              Participant != "P06" & Participant != "P07" & Participant != "P08" & Participant != "P09" )


trimStratTimes <- trimStratTimes %>%
  mutate(GazeWinner = gsub("\\d+", "", GazeWinner)) %>%
  filter(GazeWinner == "Ball-Self" | GazeWinner == "Ball-Opponent-Self" | GazeWinner == "Opponent-Self" |
           GazeWinner == "Ball-GoalPost-Self" | GazeWinner == "Self-Teammate" | GazeWinner == "Ball-Opponent")




# 
# 
# analysisComboDF <- trimStratTimes %>% group_by(Group,GazeWinner) %>%
#   summarise(totalStratDuration = sum(stratTime))
# 
# analysisComboDF <- analysisComboDF %>%
#   pivot_wider(names_from = Group, values_from = totalStratDuration)
# 
# analysisComboDF <- analysisComboDF %>%
#   mutate(diff = abs(Expert - Beginner))




analysisComboDF <- trimStratTimes %>%
  group_by(Participant,Group, GazeWinner) %>%
  summarise(totalStratDuration = sum(stratTime), .groups = "drop")

# Calculate the total strategy duration per Participant
analysisComboDF <- analysisComboDF %>%
  group_by(Participant) %>%
  mutate(participantTotalDuration = sum(totalStratDuration)) %>%
  ungroup()


analysisComboDF <- analysisComboDF %>%
  mutate(percentageUse = (totalStratDuration / participantTotalDuration) * 100)
analysisTargetDF <- analysisTargetDF %>%
  mutate(percentageUse = (totalStratDuration / participantTotalDuration) * 100)

ggplot(analysisComboDF, aes(x = GazeWinner, y = percentageUse, fill = Group)) +
  geom_boxplot() +
  labs(
    title = "Center-Looking Combinations",
    x = "",
    y = "Percentage Use (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )  + scale_y_continuous(limits = c(0, 75))
#scale_fill_brewer(palette = "Set1") # Optional: Adjust the color palette
#ggsave("Center Looking Combination Comparison.pdf")



testingDF <- trimStratTimes %>% filter(Group == "Expert")
unique(testingDF$Participant)


