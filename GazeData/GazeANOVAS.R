library(plot3D)
library(rgl)
library(dplyr)


dataFile <- "AllSubjectGazeDataFinal2.csv"
df <- read.csv(dataFile, header = TRUE, sep = ",")


soloDF <- df[df$condition == "s",]
coDF <- df[df$condition == "co",]

CsoloDF <- df[df$condition == "s" & df$group == "c",]
EsoloDF <- df[df$condition == "s" & df$group == "e",]

CcoDF <- df[df$condition == "co" & df$group == "c",]
EcoDF <- df[df$condition == "co" & df$group == "e",]


soloAvgTotalTransfer <- mean(soloDF$avgTotalTransferTime)
coAvgTotalTransfer <- mean(coDF$avgTotalTransferTime)

CsoloAvgTotalTransfer <- mean(CsoloDF$avgTotalTransferTime)
EsoloAvgTotalTransfer <- mean(EsoloDF$avgTotalTransferTime)
CcoAvgTotalTransfer <- mean(CcoDF$avgTotalTransferTime)
EcoAvgTotalTransfer <- mean(EcoDF$avgTotalTransferTime)


barTable <- matrix(c(soloAvgTotalTransfer, coAvgTotalTransfer, CsoloAvgTotalTransfer, EsoloAvgTotalTransfer, CcoAvgTotalTransfer, EcoAvgTotalTransfer), ncol = 6)
colnames(barTable) <- c('sTT','coTT','sTTc','sTTe','coTTc','coTTe')
barTable <- as.table(barTable)


barplot(barTable, main="Gaze Transfer Times",
        xlab="Number of Gears")



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

plot(df$avgPlay2Build ~ factor(df$group), data = df)
plot(df$avgPlay2Build ~ factor(df$condition), data = df)

plot(coDF$avgTotalTransferTime)



#ANOVA TESTS
twoANOVA <- aov(avgTotalTransferTime ~ factor(condition) * factor(group) , data = anovaTTDF)
summary(twoANOVA)


twoANOVA <- aov(avgTotalTransferTime ~ factor(condition), data = anovaTTDF)
summary(twoANOVA)

twoANOVA <- aov(avgTotalTransferTime ~ factor(group), data = anovaTTDF)
summary(twoANOVA)


twoANOVA <- aov(df$AvgPlay2Build ~ factor(df$condition) * factor(df$group) , data = df)
summary(twoANOVA)


#Regression model
model <- lm(df$avgTotalTransferTime ~ factor(df$group) + factor(df$condition) + df$Age + factor(df$Gender), data = df)
summary(model)

model2 <- lm(avgTotalTransferTime ~ factor(group)* factor(condition), data = anovaTTDF)
summary(model2)



