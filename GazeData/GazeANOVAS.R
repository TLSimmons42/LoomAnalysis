library(plot3D)
library(rgl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(ggsci)
library(ggpubr)


#dataFile <- "AllSubjectGazeData2-25-23_FullTime_Trimmed.csv"
#dataFile <- "AllSubjectGazeData2-25-23_FullTime.csv"
#dataFile <- "AllSubjectGazeData2-25-23_OneMin.csv"
dataFile <- "AllSubjectGazeData2-25-23_OneMin_Trimmed.csv"






df <- read.csv(dataFile, header = TRUE, sep = ",", stringsAsFactors = FALSE)
df2 <-  read.csv(dataFile2, header = TRUE, sep = ",", stringsAsFactors = FALSE)



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



#MAIN ANOVA TESTS
twoANOVA <- aov(df$avgTotalTransferTime ~ factor(df$condition) * factor(df$group) , data = df)
summary(twoANOVA)

twoANOVA <- aov(df$avgPlay2Build ~ factor(df$condition) * factor(df$group) , data = df)
summary(twoANOVA)

twoANOVA <- aov(df$avgBuild2Play ~ factor(df$condition) * factor(df$group) , data = df)
summary(twoANOVA)


twoANOVA <- aov(df$avgView2Play ~ factor(df$condition) * factor(df$group) , data = df)
summary(twoANOVA)

twoANOVA <- aov(df$avgPlay2View ~ factor(df$condition) * factor(df$group) , data = df)
summary(twoANOVA)
#__________________________________________________________________________________________________________________________________

twoANOVA <- aov(df$avgGameTime ~ factor(df$condition) * factor(df$group) , data = df)
summary(twoANOVA)


twoANOVA <- aov(df$avgTotalTransferTime ~ factor(df$group), data = df)
summary(twoANOVA)

twoANOVA <- aov(coDF$avgTotalTransferTime ~ factor(coDF$group), data = coDF)
summary(twoANOVA)

twoANOVA <- aov(soloDF$avgGrab2Build ~ factor(soloDF$group), data = soloDF)
summary(twoANOVA)

twoANOVA <- aov(df$avgTotalTransferTime ~ factor(df$condition), data = df)
summary(twoANOVA)


twoANOVA <- aov(df$avgBuild2Play ~ factor(df$condition) * factor(df$group) , data = df)
summary(twoANOVA)

twoANOVA <- aov(df$avgGrab2Build ~ factor(df$condition) * factor(df$group) , data = df)
summary(twoANOVA)

twoANOVA <- aov(df$pacStay ~ factor(df$condition) * factor(df$group) , data = df)
summary(twoANOVA)

twoANOVA <- aov(df$pacMove ~ factor(df$condition) * factor(df$group) , data = df)
summary(twoANOVA)



#Regression models
model <- lm(df$avgGameTime ~ factor(df$group) + factor(df$condition) + df$Age + factor(df$Gender)+ df$avgPlay2Build, data = df)
summary(model)

model <- lm(df$avgTotalTransferTime ~ df$avgGameTime + df$avgBuild2Play + df$avgGrab2Build, data = df)
summary(model)

model <- lm(cDF$avgTotalTransferTime ~ cDF$avgGameTime + cDF$avgBuild2Play + cDF$avgGrab2Build, data = cDF)
summary(model)

model <- lm(soloDF$avgTotalTransferTime ~ soloDF$avgGameTime + soloDF$avgBuild2Play + soloDF$avgGrab2Build, data = soloDF)
summary(model)

model2 <- lm(df$avgTotalTransferTime ~ factor(df$group)+ factor(df$condition), data = df)
summary(model2)

model2 <- lm(df$avgPlay2Build ~ factor(df$group)+ factor(df$condition), data = df)
summary(model2)

model2 <- lm(soloDF$avgPlay2Build ~ factor(soloDF$group), data = soloDF)
summary(model2)

model2 <- lm(coDF$avgPlay2Build ~ factor(coDF$group), data = df)
summary(model2)


#--------------------------------------------------------------------------------------------------------------------
#PAC models\

model2 <- lm(df$pacStay ~ factor(df$group)+ factor(df$condition), data = df)
summary(model2)

model2 <- lm(soloDF$pacStay ~ factor(soloDF$group), data = soloDF)
summary(model2)

model2 <- lm(coDF$pacStay ~ factor(coDF$group), data = coDF)
summary(model2)

model2 <- lm(df$pacMove ~ factor(df$group)+ factor(df$condition), data = df)
summary(model2)

model2 <- lm(soloDF$pacMove ~ factor(soloDF$group), data = soloDF)
summary(model2)

model2 <- lm(coDF$pacMove ~ factor(coDF$group), data = coDF)
summary(model2)

model <- lm(eDF$pacStay ~ eDF$avgGameTime + eDF$avgGrab2Build, data = eDF)
summary(model)

model <- lm(cDF$pacStay ~ cDF$avgGameTime + cDF$avgGrab2Build, data = cDF)
summary(model)




#Good Models

#Why is this a thing??????
model <- lm(soloDF$avgTotalTransferTime ~ soloDF$avgGameTime + soloDF$avgBuild2Play + soloDF$avgGrab2Build, data = soloDF)
summary(model)

model <- lm(coDF$avgTotalTransferTime ~ coDF$avgGameTime + coDF$avgBuild2Play + coDF$avgGrab2Build, data = coDF)
summary(model)

model <- lm(soloDF$avgTotalTransferTime ~ soloDF$avgGameTime + soloDF$avgBuild2Play + soloDF$avgView2Play + soloDF$avgPlay2Build + soloDF$totalPlay2View, data = soloDF)
summary(model)

model <- lm(coDF$avgTotalTransferTime ~ coDF$avgGameTime + coDF$avgBuild2Play + coDF$avgView2Play + coDF$avgPlay2Build + coDF$totalPlay2View, data = coDF)
summary(model)

model <- lm(soloDF$avgTotalTransferTime ~ soloDF$avgPlay2Build , data = soloDF)
summary(model)
plot(soloDF$avgTotalTransferTime ~ soloDF$avgPlay2Build, data = soloDF)
abline(model)


model <- lm(coDF$avgTotalTransferTime ~ coDF$avgPlay2Build, data = coDF)
summary(model)
plot(coDF$avgTotalTransferTime ~ coDF$avgPlay2Build, data = coDF)
abline(model)



#Why is this a thing??????
model <- lm(eDF$avgTotalTransferTime ~ eDF$avgGameTime + eDF$avgBuild2Play + eDF$avgGrab2Build, data = eDF)
summary(model)

model <- lm(cDF$avgTotalTransferTime ~ cDF$avgGameTime + cDF$avgBuild2Play + cDF$avgGrab2Build, data = cDF)
summary(model)


#------------------------------------------------------------------------------------------------------------
# Dont touch this -  the 1 min vs total is weird


model <- lm(CcoDF$pacStay ~ CcoDF$pacMove, data = CcoDF)
summary(model)
plot(CcoDF$pacStay ~ CcoDF$pacMove, data = CcoDF)
abline(model)

model <- lm(EcoDF$pacStay ~ EcoDF$pacMove, data = EcoDF)
summary(model)
plot(EcoDF$pacStay ~ EcoDF$pacMove, data = EcoDF)
abline(model)

model <- lm(CsoloDF$pacStay ~ CsoloDF$pacMove, data = CsoloDF)
summary(model)
plot(CsoloDF$pacStay ~ CsoloDF$pacMove, data = CsoloDF)
abline(model)

model <- lm(EsoloDF$pacStay ~ EsoloDF$pacMove, data = EsoloDF)
summary(model)
plot(EsoloDF$pacStay ~ EsoloDF$pacMove, data = EsoloDF)
abline(model)


#------------------------------------------------------------------------------------------------------------
# this is validating the DV as a real thing 
model <- lm(CcoDF$avgTimeBetweenViewWallChecks ~ CcoDF$avgGameTime, data = CcoDF)
summary(model)
plot(CcoDF$avgTimeBetweenViewWallChecks ~ CcoDF$avgGameTime, data = CcoDF)
abline(model)

model <- lm(EcoDF$avgTimeBetweenViewWallChecks ~ EcoDF$avgGameTime, data = EcoDF)
summary(model)
plot(EcoDF$avgTimeBetweenViewWallChecks ~ EcoDF$avgGameTime, data = EcoDF)
abline(model)

model <- lm(CsoloDF$avgTimeBetweenViewWallChecks ~ CsoloDF$avgGameTime, data = CsoloDF)
summary(model)
plot(CsoloDF$avgTimeBetweenViewWallChecks ~ CsoloDF$avgGameTime, data = CsoloDF)
abline(model)

model <- lm(EsoloDF$avgTimeBetweenViewWallChecks ~ EsoloDF$avgGameTime, data = EsoloDF)
summary(model)
plot(EsoloDF$avgTimeBetweenViewWallChecks ~ EsoloDF$avgGameTime, data = EsoloDF)
abline(model)

#------------------------------------------------------------------------------------------------------------


model <- lm(CcoDF$avgTimeBetweenViewWallChecks ~ CcoDF$pacStay, data = CcoDF)
summary(model)
plot(CcoDF$avgTimeBetweenViewWallChecks ~ CcoDF$pacStay, data = CcoDF)
abline(model)

model <- lm(EcoDF$avgTimeBetweenViewWallChecks ~ EcoDF$pacStay, data = EcoDF)
summary(model)
plot(EcoDF$avgTimeBetweenViewWallChecks ~ EcoDF$pacStay, data = EcoDF)
abline(model)

model <- lm(CsoloDF$avgTimeBetweenViewWallChecks ~ CsoloDF$pacStay, data = CsoloDF)
summary(model)
plot(CsoloDF$avgTimeBetweenViewWallChecks ~ CsoloDF$pacStay, data = CsoloDF)
abline(model)

model <- lm(EsoloDF$avgTimeBetweenViewWallChecks ~ EsoloDF$pacStay, data = EsoloDF)
summary(model)
plot(EsoloDF$avgTimeBetweenViewWallChecks ~ EsoloDF$pacStay, data = EsoloDF)
abline(model)




