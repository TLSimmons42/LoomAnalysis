library(plot3D)
library(rgl)
library(dplyr)
library("ggplot2")
library(tidyverse)


# dataFile <- "AllSubjectGazeDataFinal2.csv"
#dataFile <- "pacMovingTest.csv"
#dataFile <- "pacMoving.csv"
dataFile <- "pacMoving_2-25-23_FullTime.csv"
#3dataFile <- "pacMoving_2-25-23_FullTime_Trimmed.csv"




df <- read.csv(dataFile, header = TRUE, sep = ",")


soloDF <- df[df$condition == "s",]
coDF <- df[df$condition == "co",]

cDF <- df[df$partGroup == "c",]
eDF <- df[df$partGroup == "e",]

CsoloDF <- df[df$condition == "s" & df$partGroup == "c",]
EsoloDF <- df[df$condition == "s" & df$partGroup == "e",]

CcoDF <- df[df$condition == "co" & df$partGroup == "c",]
EcoDF <- df[df$condition == "co" & df$partGroup == "e",]



cAge <- mean(cDF$Age)
cAge
eAge <- mean(eDF$Age)
eAge
#------------------------------------------------------------------

data_summary <- aggregate(pacMove ~ condition, df,
                          function(x) c(mean = mean(x),
                                        se = sd(x)/ sqrt(length(x))))

data_summary <- data.frame(condition = data_summary[,1], data_summary$pacMove)
data_summary

base_r_plot <- barplot(data_summary$mean ~ condition,
                       data_summary,
                       ylab = "Perception Action Coupling Sequence 1(ms)",
                       ylim = c(0,600))

arrows(x0 = base_r_plot,
       y0 = data_summary$mean + data_summary$se,
       y1 = data_summary$mean - data_summary$se,
       angle = 90,
       code = 3,
       length = .1)


#------------------------------------------------------------------
data_summary2 <- aggregate(pacStay ~ condition, df,
                          function(x) c(mean = mean(x),
                                        se = sd(x)/ sqrt(length(x))))

data_summary2 <- data.frame(condition = data_summary2[,1], data_summary2$pacStay)
data_summary2

base_r_plot <- barplot(data_summary2$mean ~ condition,
                       data_summary2,
                       ylab = "Perception Action Coupling Sequence 2(ms)",
                       ylim = c(0,1000))

arrows(x0 = base_r_plot,
       y0 = data_summary$mean + data_summary$se,
       y1 = data_summary$mean - data_summary$se,
       angle = 90,
       code = 3,
       length = .1)

#------------------------------------------------------------------


#df <- df[df$Age<24,]


#-------------------------------------------------------------------------------------------------------------------------------------------------------------

soloAvgTotalTransfer <- mean(soloDF$pacMove)
coAvgTotalTransfer <- mean(coDF$pacMove)

CsoloAvgTotalTransfer <- mean(CsoloDF$pacMove)
EsoloAvgTotalTransfer <- mean(EsoloDF$pacMove)
CcoAvgTotalTransfer <- mean(CcoDF$pacMove)
EcoAvgTotalTransfer <- mean(EcoDF$pacMove)


barTable <- matrix(c(soloAvgTotalTransfer, coAvgTotalTransfer, CsoloAvgTotalTransfer, EsoloAvgTotalTransfer, CcoAvgTotalTransfer, EcoAvgTotalTransfer), ncol = 6)
colnames(barTable) <- c('sMove','coMove','sMoveC','sMoveE','cMoveC','coMoveE')
barTable <- as.table(barTable)


barplot(barTable, main="PACMove",
        xlab="Number of Gears")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------


soloAvgTotalTransfer <- mean(soloDF$pacStay)
coAvgTotalTransfer <- mean(coDF$pacStay)

CsoloAvgTotalTransfer <- mean(CsoloDF$pacStay)
EsoloAvgTotalTransfer <- mean(EsoloDF$pacStay)
CcoAvgTotalTransfer <- mean(CcoDF$pacStay)
EcoAvgTotalTransfer <- mean(EcoDF$pacStay)


barTable <- matrix(c(soloAvgTotalTransfer, coAvgTotalTransfer, CsoloAvgTotalTransfer, EsoloAvgTotalTransfer, CcoAvgTotalTransfer, EcoAvgTotalTransfer), ncol = 6)
colnames(barTable) <- c('sMove','coMove','sMoveC','sMoveE','cMoveC','coMoveE')
barTable <- as.table(barTable)


barplot(barTable, main="PACStay",
        xlab="Number of Gears")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

soloAvgTotalTransfer <- mean(soloDF$pacStay)
coAvgTotalTransfer <- mean(coDF$pacStay)

soloAvgTotalTransfer2 <- mean(soloDF$pacMove)
coAvgTotalTransfer2 <- mean(coDF$pacMove)


barTable <- matrix(c(soloAvgTotalTransfer, coAvgTotalTransfer, soloAvgTotalTransfer2, coAvgTotalTransfer2), ncol = 4)
colnames(barTable) <- c('sStay','coStay','sMove','coMove')
barTable <- as.table(barTable)


barplot(barTable, main="PACStay",
        xlab="Number of Gears")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

CsoloAvgTotalTransfer <- mean(CsoloDF$pacStay)
EsoloAvgTotalTransfer <- mean(EsoloDF$pacStay)
CcoAvgTotalTransfer <- mean(CcoDF$pacStay)
EcoAvgTotalTransfer <- mean(EcoDF$pacStay)

CsoloAvgTotalTransfer2 <- mean(CsoloDF$pacMove)
EsoloAvgTotalTransfer2 <- mean(EsoloDF$pacMove)
CcoAvgTotalTransfer2 <- mean(CcoDF$pacMove)
EcoAvgTotalTransfer2 <- mean(EcoDF$pacMove)


barTable <- matrix(c(CsoloAvgTotalTransfer, EsoloAvgTotalTransfer, CcoAvgTotalTransfer, EcoAvgTotalTransfer, CsoloAvgTotalTransfer2, EsoloAvgTotalTransfer2, CcoAvgTotalTransfer2, EcoAvgTotalTransfer2), ncol = 8)
colnames(barTable) <- c('soloPAC2C','soloPAC2E','coPAC2C','coPAC2E','soloPAC1C','soloPAC1E','coPAC1C','coPAC1E')
barTable <- as.table(barTable)


barplot(barTable, main="Perception Action Coupling Sequences",
        xlab="(conditions and groups)",
        ylab = "time (ms)")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

# PAC MAin
twoANOVA <- aov(df$pacMove ~ factor(df$condition) * factor(df$partGroup) , data = df)
summary(twoANOVA)

twoANOVA <- aov(df$pacStay ~ factor(df$condition) * factor(df$partGroup) , data = df)
summary(twoANOVA)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------


model <- lm(soloDF$pacMove ~ factor(soloDF$partGroup), data = soloDF)
summary(model)

model <- lm(coDF$pacMove ~ factor(coDF$partGroup), data = coDF)
summary(model)

# PAC Stay

twoANOVA <- aov(df$pacStay ~ factor(df$condition) * factor(df$partGroup) , data = df)
summary(twoANOVA)

twoANOVA <- aov(df$pacStay ~ factor(df$condition) , data = df)
summary(twoANOVA)

model <- lm(df$pacStay ~ factor(df$partGroup) + factor(df$condition), data = df)
summary(model)

model <- lm(soloDF$pacStay ~ factor(soloDF$partGroup), data = soloDF)
summary(model)

model <- lm(coDF$pacStay ~ factor(coDF$partGroup), data = coDF)
summary(model)


twoANOVA <- aov(df$pacStay ~ factor(df$condition) , data = df)
summary(twoANOVA)

twoANOVA <- aov(df$pacMove * df$pacStay ~ factor(df$condition) * factor(df$partGroup) , data = df)
summary(twoANOVA)
