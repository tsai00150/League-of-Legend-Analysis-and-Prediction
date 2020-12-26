library(xlsx)
library(ggplot2)
library(ggradar)
library(scales)
library(dplyr)
library(tibble)
library(openxlsx)
matches_data <- read.xlsx('matches.xlsx',sheet = 1)
#drop na
matches <- na.omit(matches_data)
#analyze win in blue_first  with ggradar
blue_win <- ifelse((matches$blue_win=='Win'),1,0)
blue_firstblood <- ifelse((matches$blue_firstBlood=='TRUE'),1,0)
blue_firstTower <- ifelse((matches$blue_firstTower=='TRUE'),1,0)
blue_firstDragon <- ifelse((matches$blue_firstDragon=='TRUE'),1,0)
blue_firstBaron <- ifelse((matches$blue_firstBaron=='TRUE'),1,0)
blue_firstInhibitor <- ifelse((matches$blue_firstInhibitor=='TRUE'),1,0)
blue_firstRiftHerald <- ifelse((matches$blue_firstRiftHerald=='TRUE'),1,0)
blue_win_first <- data.frame(blue_win=blue_win,blue_firstBlood=blue_firstblood,
                             blue_firstTowerd=blue_firstTower,blue_firstDragon=blue_firstDragon,
                             blue_firstBaron=blue_firstBaron,blue_firstInhibitor=blue_firstInhibitor,
                             blue_firstRiftHerald=blue_firstRiftHerald)

win <- c('Blue_Win','Purple_Win')
firstblood_fix <- c(sum(blue_win_first$blue_firstBlood=='1'&blue_win_first$blue_win=='1')/1803,sum(blue_win_first$blue_firstBlood=='1'&blue_win_first$blue_win=='0')/1803)
firstTower_fix <- c(sum(blue_win_first$blue_firstTower=='1'&blue_win_first$blue_win=='1')/1803,sum(blue_win_first$blue_firstTower=='1'&blue_win_first$blue_win=='0')/1803)
firstDragon_fix <- c(sum(blue_win_first$blue_firstDragon=='1'&blue_win_first$blue_win=='1')/1803,sum(blue_win_first$blue_firstDragon=='1'&blue_win_first$blue_win=='0')/1803)
firstBaron_fix <- c(sum(blue_win_first$blue_firstBaron=='1'&blue_win_first$blue_win=='1')/1803,sum(blue_win_first$blue_firstBaron=='1'&blue_win_first$blue_win=='0')/1803)
firstInhibitor_fix <- c(sum(blue_win_first$blue_firstInhibitor=='1'&blue_win_first$blue_win=='1')/1803,sum(blue_win_first$blue_firstInhibitor=='1'&blue_win_first$blue_win=='0')/1803)
firstRiftHerald_fix <- c(sum(blue_win_first$blue_firstRiftHerald=='1'&blue_win_first$blue_win=='1')/1803,sum(blue_win_first$blue_firstRiftHerald=='1'&blue_win_first$blue_win=='0')/1803)

blue_win_first_fix <- cbind.data.frame(blue_win=win,blue_firstBlood=as.numeric(firstblood_fix),
                                       blue_firstTowerd=as.numeric(firstTower_fix),blue_firstDragon=as.numeric(firstDragon_fix),
                                       blue_firstBaron=as.numeric(firstBaron_fix),blue_firstInhibitor=as.numeric(firstInhibitor_fix),
                                       blue_firstRiftHerald=as.numeric(firstRiftHerald_fix))

ggradar(blue_win_first_fix,grid.line.width = 0.5)

#analyze win in purple_first  with ggradar
purple_win <- ifelse((matches$blue_win=='Win'),0,1)
purple_firstblood <- ifelse((matches$purple_firstBlood=='TRUE'),1,0)
purple_firstTower <- ifelse((matches$purple_firstTower=='TRUE'),1,0)
purple_firstDragon <- ifelse((matches$purple_firstDragon=='TRUE'),1,0)
purple_firstBaron <- ifelse((matches$purple_firstBaron=='TRUE'),1,0)
purple_firstInhibitor <- ifelse((matches$purple_firstInhibitor=='TRUE'),1,0)
purple_firstRiftHerald <- ifelse((matches$purple_firstRiftHerald=='TRUE'),1,0)
purple_win_first <- data.frame(purple_win=purple_win,purple_firstBlood=purple_firstblood,
                               purple_firstTowerd=purple_firstTower,purple_firstDragon=purple_firstDragon,
                               purple_firstBaron=purple_firstBaron,purple_firstInhibitor=purple_firstInhibitor,
                               purple_firstRiftHerald=purple_firstRiftHerald)

win <- c('Blue_Win','Purple_Win')
firstblood_fix <- c(sum(purple_win_first$purple_firstBlood=='1'&purple_win_first$purple_win=='0')/1876,sum(purple_win_first$purple_firstBlood=='1'&purple_win_first$purple_win=='1')/1876)
firstTower_fix <- c(sum(purple_win_first$purple_firstTower=='1'&purple_win_first$purple_win=='0')/1876,sum(purple_win_first$purple_firstTower=='1'&purple_win_first$purple_win=='1')/1876)
firstDragon_fix <- c(sum(purple_win_first$purple_firstDragon=='1'&purple_win_first$purple_win=='0')/1876,sum(purple_win_first$purple_firstDragon=='1'&purple_win_first$purple_win=='1')/1876)
firstBaron_fix <- c(sum(purple_win_first$purple_firstBaron=='1'&purple_win_first$purple_win=='0')/1876,sum(purple_win_first$purple_firstBaron=='1'&purple_win_first$purple_win=='1')/1876)
firstInhibitor_fix <- c(sum(purple_win_first$purple_firstInhibitor=='1'&purple_win_first$purple_win=='0')/1876,sum(purple_win_first$purple_firstInhibitor=='1'&purple_win_first$purple_win=='1')/1876)
firstRiftHerald_fix <- c(sum(purple_win_first$purple_firstRiftHerald=='1'&purple_win_first$purple_win=='0')/1876,sum(purple_win_first$purple_firstRiftHerald=='1'&purple_win_first$purple_win=='1')/1876)

purple_win_first_fix <- cbind.data.frame(purple_win=win,purple_firstBlood=as.numeric(firstblood_fix),
                                         purple_firstTowerd=as.numeric(firstTower_fix),purple_firstDragon=as.numeric(firstDragon_fix),
                                         purple_firstBaron=as.numeric(firstBaron_fix),purple_firstInhibitor=as.numeric(firstInhibitor_fix),
                                         purple_firstRiftHerald=as.numeric(firstRiftHerald_fix))

ggradar(purple_win_first_fix,grid.line.width = 0.5)

#ggradar的數據需要介於0-1，數據齊全後需要重新rescale

