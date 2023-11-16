library(plot3D)
library(rgl)
library(dplyr)
library(bit64)



#participantDataFile <- "sdP1_old1.csv"

data_files <- list.files(pattern = ".csv")
data_files[]

Demodf <- data.frame(Participant = factor(),
                              Condition = factor(),
                              Trial = numeric(),
                              Group = factor(),
                              Sex = factor(),
                              Age = numeric(),
                              stringsAsFactors = FALSE)



for(f in 1:length(data_files))
{
  participantDataFile <- data_files[f]
  print(participantDataFile)
  
  
  
  df <- read.csv(participantDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[!duplicated(df$Time), ]
  
  
  Participant <- df[2,2]
  Condition <- df[8,8]
  Trial <- df[9,9]
  Group <- df[7,7]
  Age <- df[3,3]
  Sex <- df[4,4]
  
  newPartRow <- data.frame(Participant, Condition, Trial, Group, Sex, Age)
                           
  Demodf <- rbind(Demodf, newPartRow)
  
  
}



for(i in 1:nrow(Demodf))
{
  #if(df$Group[i] == "1" | df$Group[i] == "f"| is.na(df$Group[i]) | df$Participant[i] == "nuP20"| df$Participant[i] == "nuP17"){
  if(Demodf$Group[i] == "1" | Demodf$Group[i] == "f"| is.na(Demodf$Group[i])){
    
    Demodf$Group[i] <- "e"
  }
  if(Demodf$Participant[i] == "sdP13"| Demodf$Participant[i] == "sdP2"){
    Demodf$Group[i] <- "c"
    
  }
  if(Demodf$Participant[i] == "sd12" | Demodf$Participant[i] == "nuP15"| Demodf$Participant[i] == "sdP7"| Demodf$Participant[i] == "sdP15"){
    Demodf$Sex[i] <- "m"
  }
   
  if(Demodf$Participant[i] == "nuP18" | Demodf$Participant[i] == "nuP19"){
    Demodf$Sex[i] <- "f"
  }
  
  if(Demodf$Participant[i] == "sdP13"){
    Demodf$Age[i] <- 20
  }
  if(Demodf$Participant[i] == "sdP1"){
    Demodf$Age[i] <- 21
  }
}

dfAut <- Demodf %>% filter(Demodf$Group == "e")
dfNonAut <- Demodf %>% filter(Demodf$Group != "e")

mAutdf <- dfAut  %>% filter(dfAut$Sex == "m")
fAutdf <- dfAut  %>% filter(dfAut$Sex == "f")

mNondf <- dfNonAut  %>% filter(dfNonAut$Sex == "m")
fNondf <- dfNonAut  %>% filter(dfNonAut$Sex == "f")


