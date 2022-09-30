originalDF <- read.csv("analytics2.csv", header = TRUE, sep = ",")

originalDF <-  originalDF[originalDF$xPos != "c",]

soloDF <- originalDF[originalDF$Condition == "s" &((originalDF$Event =="looking at View wall" & originalDF$zPos < 0) | (originalDF$Event == "looking at Play wall") | (originalDF$Event == "looking at Build wall" &originalDF$zPos > 0)),]
#soloDF <- originalDF[originalDF$Event == "looking at View wall" | originalDF$Event == "looking at Build wall" | originalDF$Event == "looking at Play wall" ,]


coDF <- originalDF[originalDF$Condition == "co" &((originalDF$Event =="looking at View wall" & originalDF$zPos < 0) | (originalDF$Event == "looking at Play wall") | (originalDF$Event == "looking at Build wall" &originalDF$zPos > 0)),]
#coDF <- originalDF[originalDF$Event == "looking at View wall" | originalDF$Event == "looking at Build wall" | originalDF$Event == "looking at Play wall",]


xSolo <- soloDF[,9]
ySolo <- soloDF[,10]
zSolo <- soloDF[ ,11]

xCo <- coDF[,9]
yCo <- coDF[,10]
zCo <- coDF[ ,11]


#plot3d(xSolo, ySolo, zSolo)
plot3d(xCo, yCo, zCo)


#Set up the new PAC1 and PAC2 DF's
soloDFshort <- data.frame(TimeStamp = numeric(),
                   Participant = factor(),
                   Condition = factor(),
                   Trial = numeric(),
                   Age = numeric(),
                   Gender = factor(),
                   SessionTime = numeric(),
                   Event = factor(),
                   xPos = numeric(),
                   yPos = numeric(),
                   zPos = numeric(),
                   group = factor(),
                   stringsAsFactors = FALSE)



#Setup Variables
lookingForViewWall <- FALSE
lookingForBuildWall <- FALSE
lookingForPlayWall <- FALSE
lookingForNewSeq <- TRUE

viewWall <- "looking at View wall"
viewWall <- toString(viewWall)

playWall <- "looking at Play wall"
playWall <- toString(playWall)

buildWall <- "looking at Build wall"
buildWall <- toString(buildWall)


#this loop is picking out all of the row that are needed for analysis and putting them into their DF's

for (i in 2:nrow(soloDF))
{
  currentEvent <- soloDF[i,8]
  currentEvent <- toString(currentEvent)
  #print(currentEvent)
  
  currentCondition <- soloDF[i,3]
  currentCondition <- toString(currentCondition)
  

  if(lookingForNewSeq == FALSE){
      if(lookingForPlayWall){
        if(currentEvent == viewWall | currentEvent == buildWall){
          second_row_to_add <- soloDF[i,]
          #soloDFshort <- rbind(soloDFshort, first_row_to_add)
          soloDFshort <- rbind(soloDFshort, second_row_to_add)
          #print("added a new Blue sequence")
          lookingForPlayWall <- FALSE
          lookingForNewSeq <- TRUE
          
        }else if(currentEvent == playWall){
          #print("continue looking at blue")
        }else{
          #print("bad blue")
          lookingForPlayWall <- FALSE
          lookingForNewSeq <- TRUE
        }
      }
      if(lookingForBuildWall){
        if(currentEvent == viewWall | currentEvent == playWall){
          second_row_to_add <- soloDF[i,]
          #soloDFshort <- rbind(soloDFshort, first_row_to_add)
          soloDFshort <- rbind(soloDFshort, second_row_to_add)
          #print("added a new Red sequence")
          lookingForBuildWall <- FALSE
          lookingForNewSeq <- TRUE
          
        }else if(currentEvent == buildWall){
          #print("continue looking at Red")
        }else{
          #print("bad red")
          
          lookingForBuildWall <- FALSE
          lookingForNewSeq <- TRUE
        }
      }
      if(lookingForViewWall){
        if(currentEvent == buildWall | currentEvent == playWall){
          second_row_to_add <- soloDF[i,]
          #soloDFshort <- rbind(soloDFshort, first_row_to_add)
          soloDFshort <- rbind(soloDFshort, second_row_to_add)
          #print("added a new Invis sequence")
          lookingForViewWall <- FALSE
          lookingForNewSeq <- TRUE
          
        }else if(currentEvent == viewWall){
          #print("continue looking at Invis")
        }else{
          #print("bad invis")
          
          lookingForViewWall <- FALSE
          lookingForNewSeq <- TRUE
        }
      }
  }
  if(lookingForNewSeq){
    if(currentEvent == viewWall){
      first_row_to_add <- soloDF[i,]
      lookingForNewSeq <- FALSE
      lookingForViewWall <- TRUE
      print("looking for View wall")
    }
    if(currentEvent == playWall){
      first_row_to_add <- soloDF[i,]
      lookingForNewSeq <- FALSE
      lookingForPlayWall <- TRUE
      print("looking for Play wall")
    }
    if(currentEvent == buildWall){
      first_row_to_add <- soloDF[i,]
      lookingForNewSeq <- FALSE
      lookingForBuildWall <- TRUE
      print("looking for Build wall")
    }
  }
  
}

previousEvent <- ""

previousTime <- 0
counter <- (-1) # -1 because the first itteration doesnt count 


totalViewWallCount <- 0
totalPlayWallCount <- 0
totalBuildWallCount <- 0

totalTime <- 0
totalPlay2View <- 0
totalPlay2Build <- 0
totalBuild2Play <- 0
totalView2Play <- 0

avgTotalTransferTime <- 0
avgPlay2View <- 0
avgPlay2Build <- 0
avgBuild2Play <- 0
avgView2Play <- 0

play2ViewCounter <- 0
play2BuildCounter <- 0
build2PlayCounter <- 0
view2PlayCounter <- 0


for (i in 2:nrow(soloDFshort))
{
  counter <- counter +1
  currentEvent <- soloDFshort[i,8]
  currentEvent <- toString(currentEvent)
  
  currentTime <- soloDFshort[i,1]/10000
  if(previousTime != 0){
    reactionTime <- currentTime - previousTime
  }
  
  if(currentEvent == viewWall){
    if(previousEvent == playWall){
      
      totalViewWallCount <- totalViewWallCount + 1
      totalTime <- totalTime + reactionTime 
      totalPlay2View <- totalPlay2View + reactionTime
      play2ViewCounter <- play2ViewCounter + 1
      
      }else{
      print("nothin bb")
    }
  }
  
  if(currentEvent == buildWall){
    if(previousEvent == playWall){
      
      totalBuildWallCount <- totalBuildWallCount + 1
      totalTime <- totalTime + reactionTime 
      totalPlay2Build <- totalPlay2Build + reactionTime
      play2BuildCounter <- play2BuildCounter + 1

    }else{
      print("nothin bb")
    }
  }
  
  if(currentEvent == playWall){
    if(previousEvent == viewWall){
      
      totalPlayWallCount <- totalPlayWallCount + 1
      totalTime <- totalTime + reactionTime 
      totalView2Play <- totalView2Play + reactionTime
      view2PlayCounter <- view2PlayCounter + 1
      
    }else if(previousEvent == buildWall){
      
      totalPlayWallCount <- totalPlayWallCount + 1
      totalTime <- totalTime + reactionTime 
      totalBuild2Play <- totalBuild2Play + reactionTime
      build2PlayCounter <- build2PlayCounter + 1
      
    }else{
      print("nothin bb")
    }
  }
  previousEvent <- currentEvent
  previousTime <- soloDFshort[i,1]/10000
  
  
}


avgTotalTransferTime = totalTime/counter
avgView2Play = totalView2Play/view2PlayCounter
avgBuild2Play = totalBuild2Play/build2PlayCounter
avgPlay2Build = totalPlay2Build/play2BuildCounter
avgPlay2View = totalPlay2View/play2BuildCounter

