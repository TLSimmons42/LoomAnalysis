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



# grabDataFile <- "grabArousalSFNreal2.csv"
grabDataFile <- "grabArousalReal.csv"

placeDataFile <- "placeArousalReal.csv"



dfGrab <- read.csv(grabDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
dfPlace <- read.csv(placeDataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)

dfGrab <- dfGrab %>% filter(!is.na(dfGrab$Participant) | !dfGrab$Participant == "sdP18")
dfPlace <- dfPlace %>% filter(!is.na(dfPlace$Participant) | !dfPlace$Participant == "sdP18")


for(i in 1:nrow(dfGrab))
{
  if(dfGrab$group[i] == "1" | dfGrab$group[i] == "f" | is.na(dfGrab$group[i]))
  {
    dfGrab$group[i] <- "e"
  }
  if((dfGrab$Participant[i] == "sdP13" & !is.na(dfGrab$Participant[i])) | dfGrab$Participant[i] == "nuP13"){
    dfGrab$group[i] <- "c"
    
  }
  
}

for(i in 1:nrow(dfPlace))
{
  if(dfPlace$group[i] == "1" | dfPlace$group[i] == "f" | is.na(dfPlace$group[i]))
  {
    dfPlace$group[i] <- "e"
  }
  if((dfPlace$Participant[i] == "sdP13" & !is.na(dfPlace$Participant[i])) | dfPlace$Participant[i] == "nuP13"){
    dfPlace$group[i] <- "c"
  }
  
}

dfGrab <- dfGrab %>%
  mutate(condition = ifelse(condition == "solo", "Solo", condition))
dfGrab <- dfGrab %>%
  mutate(condition = ifelse(condition == "co", "Cooperative", condition))
dfGrab <- dfGrab %>%
  mutate(condition = ifelse(condition == "comp", "Competitive", condition))
dfGrab <- dfGrab %>%
  mutate(group = ifelse(group == "e", "Autistic", group))
dfGrab <- dfGrab %>%
  mutate(group = ifelse(group == "c", "Non-Autistic", group))



dfPlace <- dfPlace %>%
  mutate(condition = ifelse(condition == "solo", "Solo", condition))
dfPlace <- dfPlace %>%
  mutate(condition = ifelse(condition == "co", "Cooperative", condition))
dfPlace <- dfPlace %>%
  mutate(condition = ifelse(condition == "comp", "Competitive", condition))
dfPlace <- dfPlace %>%
  mutate(group = ifelse(group == "e", "Autistic", group))
dfPlace <- dfPlace %>%
  mutate(group = ifelse(group == "c", "Non-Autistic", group))







#THIS IS FOR INDIVIDUAL PARTICIPANT ANALYSIS
grabDFmean <- dfGrab  %>%
  group_by(Participant, group) %>%
  summarize(
    MeanPercent = mean(MeanPercentChange),
    MeanPupilSize = mean(Meanpupil),
    sd = sd(MeanPercent),
    CI_lower = MeanPercent - 1.96 * sd / sqrt(n()),
    CI_upper = MeanPercent + 1.96 * sd / sqrt(n())
  )





# THIS IS FOR CONDITION PARTICIPANT ANALYSIS
# grabDFmean <- dfGrab  %>%
#   group_by(TimeEpoch, condition, group) %>%
#   summarize(
#     MeanPercent = mean(MeanPercentChange),
#     MeanPupilSize = mean(Meanpupil))

# THIS IS FOR Group PARTICIPANT ANALYSIS
grabDFmean <- dfGrab  %>%
#  filter(group == "e")%>%
  group_by(TimeEpoch, group, condition) %>%
  summarize(
    MeanPercent = mean(MeanPercentChange),
    MeanPupilSize = mean(Meanpupil),
    sd = sd(MeanPercentChange),
    CI_lower = MeanPercent - 1.96 * sd / sqrt(n()),
    CI_upper = MeanPercent + 1.96 * sd / sqrt(n()))




# grabDFmean <- grabDFmean %>%
#   mutate(GroupColor = ifelse(group == "e" & condition == "solo", "e-solo",
#                              ifelse(group == "e" & condition == "co", "e-co",
#                                     ifelse(group == "e" & condition == "comp", "e-comp",
#                                            ifelse(group == "c" & condition == "solo", "c-solo",
#                                                   ifelse(group == "c" & condition == "co", "c-co",
#                                                          ifelse(group == "c" & condition == "comp", "c-comp",NA)))))))

p <- ggplot(grabDFmean, aes(x=TimeEpoch, y=MeanPercent,  color=group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(mapping = aes(ymin = CI_lower, ymax = CI_upper),
                width = .1, position = position_dodge(width = 0))+
  labs(
    #title = ,
    x = "Time Since Cube Grab Event (s)",
    y = "Pupil Response (% Change)",
    color = "Group"
  )+
  facet_wrap(~ condition)+
  theme(strip.text = element_text(face = "bold"), 
        strip.background = element_rect(fill = "gray", color = "black"))+
  theme_bw()

#+theme_pubclean()+scale_fill_startrek()

print(p)


#-----------------------------------------------------------------------------------------------------------

placeDFmean <- dfPlace  %>%
  #filter(condition == "co")%>%
  #group_by(TimeEpoch, group, condition) %>%
  group_by(TimeEpoch, group, condition) %>%
  summarize(
    MeanPercent = mean(MeanPercentChange),
    MeanPupilSize = mean(Meanpupil),
    sd = sd(MeanPercentChange),
    CI_lower = MeanPercent - 1.96 * sd / sqrt(n()),
    CI_upper = MeanPercent + 1.96 * sd / sqrt(n()))



# placeDFmean <- placeDFmean %>%
#   mutate(GroupColor = ifelse(group == "e" & condition == "solo", "e-solo",
#                              ifelse(group == "e" & condition == "co", "e-co",
#                                     ifelse(group == "e" & condition == "comp", "e-comp",
#                                            ifelse(group == "c" & condition == "solo", "c-solo",
#                                                   ifelse(group == "c" & condition == "co", "c-co",
#                                                          ifelse(group == "c" & condition == "comp", "c-comp",NA)))))))



p <- ggplot(placeDFmean, aes(x=TimeEpoch, y=MeanPercent,  color=group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(mapping = aes(ymin = CI_lower, ymax = CI_upper),
                width = .1, position = position_dodge(width = 0))+
  labs(
    #title = ,
    x = "Time Since Cube Place Event (s)",
    y = "Pupil Response (% Change)",
    color = "Group"
  )+
  facet_wrap(~ condition)+
  theme(strip.text = element_text(face = "bold"), 
        strip.background = element_rect(fill = "gray", color = "black"))+
  theme_bw()

#+theme_pubclean()+scale_fill_startrek()

print(p)



#-----------------------------------------------------------------------------------------------------------
# THIS IS FOR INDIVIDUAL PARTICIPANT ANALYSIS
# placeDFmean <- dfPlace  %>%
#   group_by(TimeEpoch, condition, Participant, group) %>%
#   summarize(
#     MeanPercent = mean(MeanPercentChange),
#     MeanPupilSize = mean(Meanpupil))


# THIS IS FOR CONDITION PARTICIPANT ANALYSIS
# placeDFmean <- dfPlace  %>%
#   group_by(TimeEpoch, condition, group) %>%
#   summarize(
#     MeanPercent = mean(MeanPercentChange),
#     MeanPupilSize = mean(Meanpupil))

# THIS IS FOR Group PARTICIPANT ANALYSIS
placeDFmean <- dfPlace  %>%
  group_by(TimeEpoch, group) %>%
  summarize(
    MeanPercent = mean(MeanPercentChange),
    MeanPupilSize = mean(Meanpupil))



placeDFmean <- placeDFmean %>%
  mutate(GroupColor = ifelse(group == "e" & condition == "solo", "red",
                             ifelse(group == "e" & condition == "co", "blue",
                                    ifelse(group == "e" & condition == "comp", "green",
                                           ifelse(group == "c" & condition == "solo", "yellow",
                                                  ifelse(group == "c" & condition == "co", "orange",
                                                         ifelse(group == "c" & condition == "comp", "purple",NA)))))))


p <- ggplot(placeDFmean, aes(x=TimeEpoch, y=MeanPercent,  color=group)) + 
  geom_line() +
  geom_point()
#geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
#              position=position_dodge(0.05))
print(p)





combined_ArousaldfPlace <- rawCombined_ArousaldfPlace  %>%
  group_by(TimeEpoch, condition, Participant, group) %>%
  summarize(
    MeanPercent = mean(MeanPercentChange),
    MeanPupilSize = mean(Meanpupil))

combined_ArousaldfmeanPlace <- combined_ArousaldfPlace  %>%
  group_by(TimeEpoch, condition, group) %>%
  summarize(
    MeanPercent = mean(MeanPercent),
    MeanPupilSize = mean(MeanPupilSize))








combined_ArousaldfmeanPlace <- combined_ArousaldfmeanPlace %>%
  mutate(GroupColor = ifelse(group == "e" & condition == "solo", "red",
                             ifelse(group == "e" & condition == "co", "blue",
                                    ifelse(group == "e" & condition == "comp", "green",
                                           ifelse(group == "c" & condition == "solo", "yellow",
                                                  ifelse(group == "c" & condition == "co", "orange",
                                                         ifelse(group == "c" & condition == "comp", "purple",NA)))))))


#plot(combined_Arousaldf$TimeEpoch,combined_Arousaldf$MeanPercent, col = as.factor(combined_Arousaldf$condition))

plot(combined_Arousaldfmean$TimeEpoch,combined_Arousaldfmean$MeanPercent, col = as.factor(combined_Arousaldfmean$condition))
plot(combined_Arousaldfmean$TimeEpoch,combined_Arousaldfmean$MeanPercent, col = combined_Arousaldfmean$GroupColor)

plot(combined_ArousaldfmeanPlace$TimeEpoch,combined_ArousaldfmeanPlace$MeanPercent, col = combined_ArousaldfmeanPlace$GroupColor)


combined_ArousaldfmeanPlace <- combined_ArousaldfPlace  %>%
  group_by(group, TimeEpoch) %>%
  summarize(
    MeanPercent = mean(MeanPercent),
    MeanPupilSize = mean(MeanPupilSize))

plot(combined_ArousaldfmeanPlace$TimeEpoch,combined_ArousaldfmeanPlace$MeanPercent, col = as.factor(combined_ArousaldfmeanPlace$group))



combined_Arousaldfmean <- combined_Arousaldf  %>%
  group_by(group, TimeEpoch) %>%
  summarize(
    MeanPercent = mean(MeanPercent),
    MeanPupilSize = mean(MeanPupilSize))

plot(combined_Arousaldfmean$TimeEpoch,combined_Arousaldfmean$MeanPercent, col = as.factor(combined_Arousaldfmean$group))




