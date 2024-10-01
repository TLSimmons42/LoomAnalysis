library(plot3D)
library(rgl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(ggpubr)
library(ez)
library(cowplot)

#dataFile <- "gazeDurationTimes 9-6.csv"
#dataFile <- "gazeDurationTimes 9-27-23.csv"

#dataFileGaze <- "PACsfnREAL.csv"
dataFile <- "move and gaze times 9-8-24.csv"
datFileGaze <- "GazeDurrationTimes 9_2_24.csv"

individualMovementDataFile <- "individualMovementTimes 9_11-24.csv"


gazeDF <- read.csv(datFileGaze, header = TRUE, sep = ",", stringsAsFactors = FALSE)
#df <- read.csv(dataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
df <- read.csv(dataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)
indiMoveDF <- read.csv(individualMovementDataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)


merged_df <- merge(df, gazeDF, by = c("Participant", "Group", "Condition", "Trial"), all.x = TRUE)

df <- merged_df
# View the merged data frame
print(merged_df)

df <- df %>% mutate(Group = ifelse(Group == 1, "e", Group))
df <- df %>% mutate(Group = ifelse(Group == "f" & Participant == "nuP28", "c", Group))
df <- df %>% mutate(Group = ifelse(Group == "f" & Participant == "sdP1", "e", Group))


# df <- df %>% mutate(Group = ifelse(Participant == "sdP2", "c", Group))
# df <- df %>% mutate(Group = ifelse(Participant == "nuP10", "e", Group))
# df <- df %>% mutate(Group = ifelse(Participant == "nuP02", "c", Group))
# df <- df %>% mutate(Group = ifelse(Participant == "nuP02", "c", Group))
# df <- df %>% mutate(Group = ifelse(Participant == "nuP05", "c", Group))
# df <- df %>% mutate(Group = ifelse(Participant == "nuP04", "c", Group))
# df <- df %>% mutate(Group = ifelse(Participant == "sdP11", "c", Group))
# 
# 
# df <- df %>% mutate(Group = ifelse(Participant == "sdP19", "e", Group))
# df <- df %>% mutate(Group = ifelse(Participant == "nuP13", "e", Group))
# df <- df %>% mutate(Group = ifelse(Participant == "nuP20", "e", Group))
# df <- df %>% mutate(Group = ifelse(Participant == "nuP15", "e", Group))



# 
# df <- df %>% filter(Participant != "nuP30")
# df <- df %>% filter(Participant != "nuP31")
#df <- df %>% filter(!(Condition == "solo" & Trial == 1))
# df <- df %>% filter(avgDrop < 4)




#df <- df %>% filter(df$Condition != "solo")
df <- df %>% mutate(Group = ifelse(Group == "e", "AUT", Group))
df <- df %>% mutate(Group = ifelse(Group == "c", "Non-AUT", Group))


df <- df %>% mutate(Group = ifelse(Participant == "sdP3" , "AUT" , Group))
df <- df %>% distinct(avgGrab, avgDrop, avgDropStart, .keep_all = TRUE)
df <- df %>% mutate(Age = ifelse(Participant == "nuP10" & Sex == "f", 23 , Age))
df <- df %>% mutate(Participant = ifelse(Participant == "p012" , "nuP12" , Participant))
df <- df %>% mutate(Participant = ifelse(Participant == "P31" , "nuP31" , Participant))
df <- df %>% mutate(Participant = ifelse(Participant == "Py" , "P7" , Participant))
df <- df %>% mutate(Sex = ifelse(Participant == "nuP18" , "m" , Sex))
df <- df %>% mutate(Age = ifelse(Participant == "nuP18" , 25 , Age))
df <- df %>% mutate(Sex = ifelse(Participant == "nuP24" , "f" , Sex))
df <- df %>% mutate(Participant = ifelse(Participant == "sd12" , "sdP12" , Participant))
df <- df %>% mutate(Sex = ifelse(Participant == "sdP12" , "m" , Sex))
df <- df %>% mutate(Sex = ifelse(Participant == "nuP19" , "f" , Sex))
df <- df %>% mutate(Age = ifelse(Participant == "nuP24" , 24 , Age))
df <- df %>% mutate(Age = ifelse(Participant == "sdP1" , 21 , Age))
df <- df %>% mutate(Age = ifelse(Participant == "sdP13" , 20 , Age))
df <- df %>% mutate(Sex = ifelse(Participant == "sdP15" , "m" , Sex))
df <- df %>% mutate(Sex = ifelse(Participant == "sdP7" , "m" , Sex))
df <- df %>% mutate(Participant = ifelse(Participant == "sdP27" , "nuP27" , Participant))

df <- df %>% filter(Participant != "sdP18")
df <- df %>% filter(!(Participant == "nuP10" & Age == 21))
df <- df %>% filter(Participant != "nuP31")
# df <- df %>% filter(Participant != "nuP30")
# df <- df %>% filter(Participant != "nuP25")




# p <- df %>%
#   group_by(avgGrab, Group, Condition)%>%
#   summarise(mATT = mean(avgDrop), sATT = sd(avgDrop))%>%
#   ggplot(aes(Condition,mATT, fill = reorder(Group,mATT)))+
#   geom_boxplot() +
#   # geom_boxplot(outlier.shape = NA) +
#   labs(title = "Drop Movement Time", x = "", y = "Time (ms)")+
#   #theme(legend.position = "none")+
#   theme_bw()
# p <- p + guides(fill=guide_legend(title="Group"))
# p
#ggsave("Drop Time.pdf")


tempDF<- df %>% filter(!is.na(avgGrab))
tempDF <- tempDF %>% filter(grabCount > 8)
#tempDF <- tempDF %>% mutate(avgViewWAllDurration = avgViewWAllDurration / 1000)
#tempDF <- tempDF %>% filter(Condition != "solo")

p <- tempDF %>%
  group_by(Group, Condition)%>%
  summarize(
    Mean = mean(avgGrab),
    sd = sd(avgGrab),
    CI_lower = Mean - 1.96 * sd / sqrt(n()),
    CI_upper = Mean + 1.96 * sd / sqrt(n())) %>%
  ggplot(aes(Condition,Mean, fill = Group))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(mapping = aes(ymin = CI_lower, ymax = CI_upper),
                width = .3, position = position_dodge(width = .9))+
  labs(title = "View Wall Gaze Duration Time", x = "", y = "Time (s)")+
  theme_bw()

p <- p + guides(fill=guide_legend(title="Group"))
p
#ggsave("View Builc Gaze Duration Time.pdf")

# anova_model <- aov(Duration ~ Group, data = comboFigDF)
# summary(anova_model)


tTestDF <- df %>% group_by(Group,Participant) %>%
  filter(Condition == "co")  %>%
  summarize(Duration = mean(avgDrop))


group_A <- subset(tTestDF, Group == 'AUT')$Duration
group_B <- subset(tTestDF, Group == 'Non-AUT')$Duration
result <- t.test(group_A, group_B, var.equal = FALSE)
print(result)



ggplot(df, aes(x = Condition, y = avgDropStart, fill = reorder(Group,avgDropStart))) +
  geom_violin() +
  theme_minimal() +
  labs(title = "Violin Plot", x = "Category", y = "Value")

# 
# ggplot(df, aes(x = Condition, y = avgViewWAllDurration)) +
#   geom_boxplot() +
#   geom_jitter(width = 0.2, alpha = 0.5) +  # Add jittered points
#   labs(title = "Box Plot with Individual Points for Three Conditions",
#        x = "Condition",
#        y = "Value")
# 
# 
# #data_excluded <- subset(df, Condition != "solo")
# 
# 
# anova_result <- aov(avgViewWAllDurration ~ Condition, data = df)
# summary(anova_result)
#   
# gazePlot <- df%>%
#   group_by(Condition, group)%>%
#   summarise(mATT = mean(avgBuild2Play), sATT = sd(avgBuild2Play))%>%
#   ggplot(aes(reorder(condition,mATT),mATT, fill = reorder(group,mATT)))+
#   geom_bar(stat = "identity", position = "dodge")+
#   #geom_text(mapping=aes(label=round(mATT,2)), position = position_dodge(width = 0.9),
#   #        cex= 2.5, vjust=-2)+
#   labs(title = "PAC Cube Placement Sequence",
#        subtitle = "",
#        x = "Trial Condition", y = "Time (ms)",
#        #caption = "moo",
#        fill = "")+
#   geom_errorbar(mapping = aes(ymin = mATT-sATT, ymax = mATT + sATT),
#                 width = 0.2, position = position_dodge(width = 0.9))+
#   theme_pubclean()+scale_fill_startrek() 
# 
# gazePlot




demoDFagg <- df %>% group_by(Participant, Group, Age, Sex) %>%
  summarise(count = n())

demoDFagg2 <- demoDFagg %>% group_by(Group, Sex) %>%
  summarise(count = n())





