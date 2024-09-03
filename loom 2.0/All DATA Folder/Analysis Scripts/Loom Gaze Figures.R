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
dataFile <- "Grab_Place_FullPart3.csv"
datFileGaze <- "GazeDurrationTimes 9_2_24.csv"

gazeDF <- read.csv(datFileGaze, header = TRUE, sep = ",", stringsAsFactors = FALSE)
#df <- read.csv(dataFile, colClasses=c("Time" = "integer64"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
df <- read.csv(dataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)



merged_df <- merge(df, gazeDF, by = c("Participant", "Group", "Condition", "Trial"), all.x = TRUE)

df <- merged_df
# View the merged data frame
print(merged_df)

df <- df %>% mutate(Group = ifelse(Group == 1, "e", Group))
df <- df %>% mutate(Group = ifelse(Group == "f" & Participant == "nuP28", "c", Group))
df <- df %>% mutate(Group = ifelse(Group == "f" & Participant == "sdP1", "e", Group))


df <- df %>% mutate(Group = ifelse(Participant == "sdP2", "c", Group))
# df <- df %>% mutate(Group = ifelse(Participant == "nuP10", "c", Group))
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




df <- df %>% filter(Participant != "nuP30")
df <- df %>% filter(Participant != "nuP29")
#df <- df %>% filter(!(Condition == "solo" & Trial == 1))
df <- df %>% filter(avgDrop < 4)




#df <- df %>% filter(df$Condition != "solo")
df <- df %>% mutate(Group = ifelse(Group == "e", "AUT", Group))
df <- df %>% mutate(Group = ifelse(Group == "c", "Non-AUT", Group))

subDF <- df %>% filter(Condition == "co")

p <- df %>%
  group_by(avgGrab, Group, Condition)%>%
  summarise(mATT = mean(avgDrop), sATT = sd(avgDrop))%>%
  ggplot(aes(Condition,mATT, fill = reorder(Group,mATT)))+
  geom_boxplot() +
  # geom_boxplot(outlier.shape = NA) +
  labs(title = "Drop Movement Time", x = "", y = "Time (ms)")+
  #theme(legend.position = "none")+
  theme_bw()
p <- p + guides(fill=guide_legend(title="Group"))
p
#ggsave("Drop Time.pdf")


df<- df %>% filter(!is.na(avgViewWAllDurration))
df <- df %>% mutate(avgViewWAllDurration = avgViewWAllDurration / 1000)

p <- df %>%
  group_by(Group, Condition)%>%
  summarize(
    Mean = mean(avgViewWAllDurration),
    sd = sd(avgViewWAllDurration),
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
ggsave("View Wall Gaze Duration Time.pdf")

# anova_model <- aov(Duration ~ Group, data = comboFigDF)
# summary(anova_model)


tTestDF <- df %>% group_by(Group,Participant) %>%
  filter(Condition == "co")  %>%
  summarize(Duration = mean(avgDrop))


group_A <- subset(tTestDF, Group == 'AUT')$Duration
group_B <- subset(tTestDF, Group == 'Non-AUT')$Duration
result <- t.test(group_A, group_B, var.equal = FALSE)
print(result)


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
