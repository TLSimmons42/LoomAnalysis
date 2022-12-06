library(plot3D)
library(rgl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(ggsci)
library(ggpubr)

# dataFile <- "AllSubjectGazeDataFinal2.csv"
dataFile <- "AllSubjectGazeData.csv"


df <- read.csv(dataFile, header = TRUE, sep = ",")


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
 ntwoANOVA <- aov(avgTotalTransferTime ~ factor(condition) * factor(group) , data = anovaTTDF)
summary(twoANOVA)

twoANOVA <- aov(df$avgPlay2Build ~ factor(df$condition) * factor(df$group) , data = anovaTTDF)
summary(twoANOVA)


twoANOVA <- aov(avgTotalTransferTime ~ factor(condition), data = anovaTTDF)
summary(twoANOVA)

twoANOVA <- aov(avgTotalTransferTime ~ factor(group), data = anovaTTDF)
summary(twoANOVA)


twoANOVA <- aov(df$AvgPlay2Build ~ factor(df$condition) * factor(df$group) , data = df)
summary(twoANOVA)

twoANOVA <- aov(df$avgGrab2Build ~ factor(df$condition) * factor(df$group) , data = df)
summary(twoANOVA)



#Regression model
model <- lm(df$avgTotalTransferTime ~ factor(df$group) + factor(df$condition) + df$Age + factor(df$Gender), data = df)
summary(model)

model2 <- lm(avgTotalTransferTime ~ factor(group)* factor(condition), data = anovaTTDF)
summary(model2)

model2 <- lm(df$avgGrab2Build ~ factor(df$group)* factor(df$condition), data = anovaTTDF)
summary(model2)





