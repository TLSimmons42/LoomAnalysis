# Load required packages
library(dplyr)
library(ggplot2)
#library(signal)  # For Butterworth filter
library(plotly)
library(bit64)
library(pracma)  # For rad2deg function



PhaseDF <- data.frame(Participant = factor(),
                      Condition = factor(),
                      Trial = numeric(),
                      Group = factor(),
                      Phase = factor(),
                      Duration = numeric(),
                      stringsAsFactors = FALSE)



 #data_files <- list.files(pattern = "nuP15(\\D|$)")
data_files <- list.files(pattern = ".csv")

for(f in 1:length(data_files))
{

  
  participantDataFile <- data_files[f]
  print(participantDataFile)
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  
  output_file <- "C:/Users/Trent Simons/Desktop/Data/LoomAnalysis/Loom 2.0/All DATA Folder/Gaze Merged Data/sdP15_Gaze_Events.csv"  # Change this to your desired output file
  
  trimDF <- df %>% filter(Phase != "none")
  trimDF <- trimDF %>% mutate(Phase_Duration = (lead(Time) - Time)/10000)
  
  
  
  Participant <- trimDF$Participant[5]
  Condition <- trimDF$Condition[5]
  Trial <- trimDF$Trial[5]
  Group <- trimDF$Group[5]
  
  
  
  looking4New <- TRUE
  
  P1 <- FALSE
  P2 <- FALSE
  P3 <- FALSE
  P4 <- FALSE
  P5 <- FALSE
  P6 <- FALSE
  
  current_Phase <- ""
  current_Trial <- 0
  current_Condition <- ""
  current_Duration <- 0
  
  phase1_duration <- 0
  phase2_duration <- 0
  phase3_duration <- 0
  phase4_duration <- 0
  phase5_duration <- 0
  
  
  for (i in 1:nrow(trimDF)) {
    current_Phase <- trimDF$Phase[i]
    current_Duration <- trimDF$Phase_Duration[i]
    current_Trial <- trimDF$Trial[i]
    current_Condition <- trimDF$Condition[i]
    
    print(current_Condition)
    print(Condition)
    print(Trial)
    print(current_Trial)
    print(current_Phase)
    
    if(!looking4New & current_Condition == Condition & current_Trial == Trial){
      if(P1 & current_Phase == "Phase 2"){
        print("phase 2")
        P1 <- FALSE
        P2 <- TRUE
        phase2_duration <- current_Duration
        next
        
      }else
      if(P2 & current_Phase == "Phase 3"){
        print("phase 3")
        
        P2 <- FALSE
        P3 <- TRUE
        phase3_duration <- current_Duration
        next
        
      }else
      if(P3 & current_Phase == "Phase 4"){
        print("phase 4")
        
        P3 <- FALSE
        P4 <- TRUE
        phase4_duration <- current_Duration
        next
        
      }else
      if(P4 & current_Phase == "Phase 5"){
        print("phase 5")
        
        P4 <- FALSE
        P5 <- TRUE
        phase5_duration <- current_Duration
        next
        
      }else
      if(P5 & current_Phase == "Phase 1"){
        print("done")
        
        P5 <- FALSE
        looking4New <- TRUE
  
        Phase <- "Phase 1"
        Duration <- phase1_duration
        newPartRow <- data.frame(Participant, Condition, Trial, Group, Phase, Duration)
        PhaseDF <- rbind(PhaseDF, newPartRow)
        Phase <- "Phase 2"
        Duration <- phase2_duration
        newPartRow <- data.frame(Participant, Condition, Trial, Group, Phase, Duration)
        PhaseDF <- rbind(PhaseDF, newPartRow)
        Phase <- "Phase 3"
        Duration <- phase3_duration
        newPartRow <- data.frame(Participant, Condition, Trial, Group, Phase, Duration)
        PhaseDF <- rbind(PhaseDF, newPartRow)
        Phase <- "Phase 4"
        Duration <- phase4_duration
        newPartRow <- data.frame(Participant, Condition, Trial, Group, Phase, Duration)
        PhaseDF <- rbind(PhaseDF, newPartRow)
        Phase <- "Phase 5"
        Duration <- phase5_duration
        newPartRow <- data.frame(Participant, Condition, Trial, Group, Phase, Duration)
        PhaseDF <- rbind(PhaseDF, newPartRow)
  
        
      }else{
        looking4New <- TRUE
        P1 <- FALSE
        P2 <- FALSE
        P3 <- FALSE
        P4 <- FALSE
        P5 <- FALSE
      }
    }else{
      print("fail")
      looking4New <- TRUE
    }
    
    
    if(looking4New & current_Phase == "Phase 1"){
      print("looking 4 new")
      looking4New = FALSE
      P1 <- TRUE
      phase1_duration = current_Duration
      
      Condition <- trimDF$Condition[i]
      Trial <- trimDF$Trial[i]
      
      Participant <- trimDF$Participant[i]
      Group <- trimDF$Group[i]
      
      
    }
    
  }

}

PhaseDF <- PhaseDF %>% mutate(Group = ifelse(Group == "c","Non-Aut",Group))
PhaseDF <- PhaseDF %>% mutate(Group = ifelse(Group == "e","Aut",Group))



ggplot(PhaseDF, aes(x = Phase, y = Duration, fill = Group)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = NA) +
  #geom_jitter(position = position_jitter(width = 0.2), size = 1, alpha = 0.6) +
  labs(x = "Phase", y = "Duration (ms)",
       title = "Phase Analysis ALL")+
       #subtitle = "Comparing two groups across five categories") +
  scale_fill_manual(values = c("skyblue", "salmon"), name = "Group") +
  theme_minimal(base_size = 15) + 
  ylim(0, 2500)+
  # Use a minimal theme with a larger base font size
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "top") # Move legend to the top

PhaseDFsolo <- PhaseDF %>% filter(Condition == "solo")
ggplot(PhaseDFsolo, aes(x = Phase, y = Duration, fill = Group)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = NA) +
  #geom_jitter(position = position_jitter(width = 0.2), size = 1, alpha = 0.6) +
  labs(x = "Phase", y = "Duration (ms)",
       title = "Phase Analysis Solo")+
  #subtitle = "Comparing two groups across five categories") +
  scale_fill_manual(values = c("skyblue", "salmon"), name = "Group") +
  theme_minimal(base_size = 15) + 
  ylim(0, 2500)+
  # Use a minimal theme with a larger base font size
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "top") # Move legend to the top

PhaseDFco <- PhaseDF %>% filter(Condition == "co")
ggplot(PhaseDFco, aes(x = Phase, y = Duration, fill = Group)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = NA) +
  #geom_jitter(position = position_jitter(width = 0.2), size = 1, alpha = 0.6) +
  labs(x = "Phase", y = "Duration (ms)",
       title = "Phase Analysis Co-Op")+
  #subtitle = "Comparing two groups across five categories") +
  scale_fill_manual(values = c("skyblue", "salmon"), name = "Group") +
  theme_minimal(base_size = 15) + 
  ylim(0, 2500)+
  # Use a minimal theme with a larger base font size
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "top") # Move legend to the top

PhaseDFcomp <- PhaseDF %>% filter(Condition == "comp")
ggplot(PhaseDFcomp, aes(x = Phase, y = Duration, fill = Group)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = NA) +
  #geom_jitter(position = position_jitter(width = 0.2), size = 1, alpha = 0.6) +
  labs(x = "Phase", y = "Duration (ms)",
       title = "Phase Analysis Comp")+
  #subtitle = "Comparing two groups across five categories") +
  scale_fill_manual(values = c("skyblue", "salmon"), name = "Group") +
  theme_minimal(base_size = 15) + 
  ylim(0, 2500)+
  # Use a minimal theme with a larger base font size
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "top") # Move legend to the top

