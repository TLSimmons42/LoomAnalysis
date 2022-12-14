library(plot3D)
library(rgl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(ggsci)
library(ggpubr)

# dataFile <- "AllSubjectGazeDataFinal2.csv"
dataFile <- "AllSubjectGazeData12-7_new.csv"
dataFile2 <- "pacMoving.csv"



df <- read.csv(dataFile, header = TRUE, sep = ",")
df2 <-  read.csv(dataFile2, header = TRUE, sep = ",")



soloDF <- df[df$condition == "s",]
coDF <- df[df$condition == "co",]

cDF <- df[df$group == "c",]
eDF <- df[df$group == "e",]

amtofControl <- nrow(cDF)/2
amtofAut <- nrow(eDF)/2
amtofControl
amtofAut

CsoloDF <- df[df$condition == "s" & df$group == "c",]
EsoloDF <- df[df$condition == "s" & df$group == "e",]

CcoDF <- df[df$condition == "co" & df$group == "c",]
EcoDF <- df[df$condition == "co" & df$group == "e",]

#-------------------------------------------------------------------------------------------------------------------------------
data_summary <- aggregate(avgTotalTransferTime ~ condition, df,
                          function(x) c(mean = mean(x),
                                        se = sd(x)/ sqrt(length(x))))

data_summary <- data.frame(condition = data_summary[,1], data_summary$avgTotalTransferTime)
data_summary

data_summary2 <- aggregate(avgTotalTransferTime ~ group, df,
                           function(x) c(mean = mean(x),
                                         se = sd(x)/ sqrt(length(x))))

data_summary2 <- data.frame(group = data_summary2[,1], data_summary2$avgTotalTransferTime)
data_summary2


base_r_plot <- barplot(data_summary$mean ~ condition,
                       data_summary,
                       ylab = "Total Gaze Transfer Time(ms)",
                       ylim = c(0,3000))

arrows(x0 = base_r_plot,
       y0 = data_summary$mean + data_summary$se,
       y1 = data_summary$mean - data_summary$se,
       angle = 90,
       code = 3,
       length = .1)



base_r_plot <- barplot(data_summary2$mean ~ group,
                       ylab = "Total Gaze Transfer Time(ms)",
                       data_summary2,
                       ylim = c(0,3000))

arrows(x0 = base_r_plot,
       y0 = data_summary2$mean + data_summary2$se,
       y1 = data_summary2$mean - data_summary2$se,
       angle = 90,
       code = 3,
       length = .1)




#-------------------------------------------------------------------------------------------------------------------------------------------------------------

soloAvgTotalTransfer <- mean(soloDF$avgTotalTransferTime)
coAvgTotalTransfer <- mean(coDF$avgTotalTransferTime)

CsoloAvgTotalTransfer <- mean(CsoloDF$avgTotalTransferTime)
EsoloAvgTotalTransfer <- mean(EsoloDF$avgTotalTransferTime)
CcoAvgTotalTransfer <- mean(CcoDF$avgTotalTransferTime)
EcoAvgTotalTransfer <- mean(EcoDF$avgTotalTransferTime)




barTable <- matrix(c(soloAvgTotalTransfer, coAvgTotalTransfer, CsoloAvgTotalTransfer, EsoloAvgTotalTransfer, CcoAvgTotalTransfer, EcoAvgTotalTransfer), ncol = 6)
colnames(barTable) <- c('soloTotalTime','cooperative TotalTime','sTTc','sTTe','coTTc','coTTe')
barTable <- as.table(barTable)


barplot(barTable, main="Gaze Transfer Times",
        ylab = "Total Gaze Transfer Time(ms)",
        xlab="Groups and Conditions")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

soloAvgGrab2Build <- mean(soloDF$avgGrab2Build)
coAvgGrab2Build <- mean(coDF$avgGrab2Build)

CsoloAvgGrab2Build <- mean(CsoloDF$avgGrab2Build)
EsoloAvgGrab2Build <- mean(EsoloDF$avgGrab2Build)
CcoAvgGrab2Build <- mean(CcoDF$avgGrab2Build)
EcoAvgGrab2Build <- mean(EcoDF$avgGrab2Build)


barTable2 <- matrix(c(soloAvgGrab2Build, coAvgGrab2Build, CsoloAvgGrab2Build, EsoloAvgGrab2Build, CcoAvgGrab2Build, EcoAvgGrab2Build), ncol = 6)
colnames(barTable2) <- c('sTT','coTT','sTTc','sTTe','coTTc','coTTe')
barTable2 <- as.table(barTable)


barplot(barTable2, main="Gaze Transfer Times",
        xlab="Number of Gears")


#-------------------------------------------------------------------------------------------------------------------------------------------------------------

anovaTTDF <- data.frame(group = df$group,
                          condition = df$condition,
                          avgTotalTransferTime = df$avgTotalTransferTime,
                          stringsAsFactors = FALSE)



plot(df$avgTotalTransferTime ~ factor(df$group), data = df)
plot(df$avgTotalTransferTime ~ factor(df$condition), data = df)

plot(df$avgBuild2Play ~ factor(df$group), data = df)
plot(df$avgBuild2Play ~ factor(df$condition), data = df)

plot(df$avgView2Play ~ factor(df$group), data = df)
plot(df$avgView2Play ~ factor(df$condition), data = df)

plot(df$avgPlay2Build ~ factor(df$group), data = df,
     ylab = "Gaze Transfer Time from Play to Build wall(ms)",
     xlab = "group")
plot(df$avgPlay2Build ~ factor(df$condition), data = df)

plot(coDF$avgTotalTransferTime)



#ANOVA TESTS
 twoANOVA <- aov(df$avgTotalTransferTime ~ factor(df$condition) * factor(df$group) , data = df)
summary(twoANOVA)

twoANOVA <- aov(df$avgGameTime ~ factor(df$condition) * factor(df$group) , data = df)
summary(twoANOVA)


twoANOVA <- aov(avgTotalTransferTime ~ factor(condition), data = anovaTTDF)
summary(twoANOVA)

twoANOVA <- aov(coDF$avgGameTime ~ factor(coDF$group), data = coDF)
summary(twoANOVA)


twoANOVA <- aov(df$avgPlay2Build ~ factor(df$condition) * factor(df$group) , data = df)
summary(twoANOVA)

twoANOVA <- aov(df$avgGrab2Build ~ factor(df$condition) * factor(df$group) , data = df)
summary(twoANOVA)



#Regression models
model <- lm(df$avgGameTime ~ factor(df$group) + factor(df$condition) + df$Age + factor(df$Gender)+ df$avgPlay2Build, data = df)
summary(model)

model <- lm(df$avgTotalTransferTime ~ df$avgGameTime + df$avgBuild2Play + df$avgGrab2Build, data = df)
summary(model)

model <- lm(cDF$avgTotalTransferTime ~ cDF$avgGameTime + cDF$avgBuild2Play + cDF$avgGrab2Build, data = cDF)
summary(model)

model2 <- lm(df$avgPlay2Build ~ factor(group)+ factor(condition), data = anovaTTDF)
summary(model2)

model2 <- lm(df$avgGrab2Build ~ factor(df$group)+ factor(df$condition), data = anovaTTDF)
summary(model2)


#Good Models

#Why is this a thing??????
model <- lm(soloDF$avgTotalTransferTime ~ soloDF$avgGameTime + soloDF$avgBuild2Play + soloDF$avgGrab2Build, data = soloDF)
summary(model)

model <- lm(coDF$avgTotalTransferTime ~ coDF$avgGameTime + coDF$avgBuild2Play + coDF$avgGrab2Build, data = coDF)
summary(model)


#Why is this a thing??????
model <- lm(eDF$avgTotalTransferTime ~ eDF$avgGameTime + eDF$avgBuild2Play + eDF$avgGrab2Build, data = eDF)
summary(model)

model <- lm(cDF$avgTotalTransferTime ~ cDF$avgGameTime + cDF$avgBuild2Play + cDF$avgGrab2Build, data = cDF)
summary(model)



#--------------------------------------------------------------------------------------------------------------------
#combining PAC and Gaze


model <- lm(df$pacStay ~  df$avgBuild2Play + df$avgGrab2Build + df$pacMove + df$avgTotalTransferTime, data = df)
summary(model)

model <- lm(coDF$pacStay ~ coDF$avgGameTime + coDF$avgBuild2Play + coDF$avgGrab2Build + coDF$pacMove + coDF$avgTotalTransferTime, data = coDF)
summary(model)

model <- lm(soloDF$pacMove ~ soloDF$avgGameTime + soloDF$avgBuild2Play + soloDF$avgGrab2Build + soloDF$pacStay + soloDF$avgTotalTransferTime, data = soloDF)
summary(model)






