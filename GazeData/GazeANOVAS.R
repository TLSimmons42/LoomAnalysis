library(plot3D)
library(rgl)
library(dplyr)


dataFile <- "AllSubjectGazeDataFinal.csv"
df <- read.csv(dataFile, header = TRUE, sep = ",")


soloDF <- df[df$Condition == "s",]
coDF <- df[df$Condition == "co",]

CsoloDF <- df[df$Condition == "s" & df$Group == "c",]
EsoloDF <- df[df$Condition == "s" & df$Group == "e",]

CcoDF <- df[df$Condition == "co" & df$Group == "c",]
EcoDF <- df[df$Condition == "co" & df$Group == "e",]



soloAvgTotalTransfer <- mean(soloDF$AvgTotalTransferTime)
coAvgTotalTransfer <- mean(coDF$AvgTotalTransferTime)

CsoloAvgTotalTransfer <- mean(CsoloDF$AvgTotalTransferTime)
EsoloAvgTotalTransfer <- mean(EsoloDF$AvgTotalTransferTime)
CcoAvgTotalTransfer <- mean(CcoDF$AvgTotalTransferTime)
EcoAvgTotalTransfer <- mean(EcoDF$AvgTotalTransferTime)


barTable <- matrix(c(soloAvgTotalTransfer, coAvgTotalTransfer, CsoloAvgTotalTransfer, EsoloAvgTotalTransfer, CcoAvgTotalTransfer, EcoAvgTotalTransfer), ncol = 6)
colnames(barTable) <- c('sTT','coTT','sTTc','sTTe','coTTc','coTTe')
barTable <- as.table(barTable)


barplot(barTable, main="Gaze Transfer Times",
        xlab="Number of Gears")



anovaTTDF <- data.frame(Group = df$Group,
                          Condition = df$Condition,
                          AvgTotalTransferTime = df$AvgTotalTransferTime,
                          stringsAsFactors = FALSE)


plot(df$AvgTotalTransferTime ~ factor(df$Group) + factor(df$Condition) , data = df)


plot(df$AvgTotalTransferTime ~ factor(df$Group), data = df)
plot(df$AvgTotalTransferTime ~ factor(df$Condition), data = df)

plot(df$AvgBuild2Play ~ factor(df$Group), data = df)
plot(df$AvgBuild2Play ~ factor(df$Condition), data = df)

plot(df$AvgView2Play ~ factor(df$Group), data = df)
plot(df$AvgView2Play ~ factor(df$Condition), data = df)

plot(df$AvgPlay2Build ~ factor(df$Group), data = df)
plot(df$AvgPlay2Build ~ factor(df$Condition), data = df)

plot(coDF$AvgTotalTransferTime)






#ANOVA TESTS
twoANOVA <- aov(AvgTotalTransferTime ~ factor(Condition) * factor(Group) , data = anovaTTDF)
summary(twoANOVA)


twoANOVA <- aov(AvgTotalTransferTime ~ factor(Condition), data = anovaTTDF)
summary(twoANOVA)

twoANOVA <- aov(AvgTotalTransferTime ~ factor(Group), data = anovaTTDF)
summary(twoANOVA)


twoANOVA <- aov(df$AvgPlay2Build ~ factor(df$Condition) * factor(df$Group) , data = df)
summary(twoANOVA)


#Regression model
model <- lm(df$AvgTotalTransferTime ~ factor(df$Group) + factor(df$Condition) + df$Age + factor(df$Gender), data = df)
summary(model)

model2 <- lm(AvgTotalTransferTime ~ factor(Group)* factor(Condition), data = anovaTTDF)
summary(model2)



