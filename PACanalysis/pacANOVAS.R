library(plot3D)
library(rgl)
library(dplyr)


# dataFile <- "AllSubjectGazeDataFinal2.csv"
dataFile <- "pacMoving.csv"


df <- read.csv(dataFile, header = TRUE, sep = ",")

df <- df[df$Age<24,]


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
colnames(barTable) <- c('sStayC','sStayE','coStayC','coStayE','sMoveC','sMoveE','coMoveC','coMoveE')
barTable <- as.table(barTable)


barplot(barTable, main="PACStay",
        xlab="Number of Gears")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------


twoANOVA <- aov(df$pacMove ~ factor(df$condition) * factor(df$partGroup) , data = df)
summary(twoANOVA)

twoANOVA <- aov(df$pacStay ~ factor(df$condition) * factor(df$partGroup) , data = df)
summary(twoANOVA)

twoANOVA <- aov(df$pacMove * df$pacStay ~ factor(df$condition) * factor(df$partGroup) , data = df)
summary(twoANOVA)
