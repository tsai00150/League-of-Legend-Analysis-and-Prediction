library(xlsx)
library(ggplot2)
library(ggradar)
library(scales)
library(dplyr)
library(tibble)
library(openxlsx)
matches_data <- read.xlsx('matches_final.xlsx',sheet = 1)
#drop na
matches <- na.omit(matches_data)
#analyze win in blue_first  with ggradar
win <- c('Blue_Win','Purple_Win')
firstblood_fix <- c(sum(matches_data$blue_firstBlood=='1'&matches_data$blue_win=='1')/3008,sum(matches_data$blue_firstBlood=='1'&matches_data$blue_win=='0')/3008)
firstTower_fix <- c(sum(matches_data$blue_firstTower=='1'&matches_data$blue_win=='1')/3008,sum(matches_data$blue_firstTower=='1'&matches_data$blue_win=='0')/3008)
firstDragon_fix <- c(sum(matches_data$blue_firstDragon=='1'&matches_data$blue_win=='1')/3008,sum(matches_data$blue_firstDragon=='1'&matches_data$blue_win=='0')/3008)
firstBaron_fix <- c(sum(matches_data$blue_firstBaron=='1'&matches_data$blue_win=='1')/3008,sum(matches_data$blue_firstBaron=='1'&matches_data$blue_win=='0')/3008)
firstInhibitor_fix <- c(sum(matches_data$blue_firstInhibitor=='1'&matches_data$blue_win=='1')/3008,sum(matches_data$blue_firstInhibitor=='1'&matches_data$blue_win=='0')/3008)
firstRiftHerald_fix <- c(sum(matches_data$blue_firstRiftHerald=='1'&matches_data$blue_win=='1')/3008,sum(matches_data$blue_firstRiftHerald=='1'&matches_data$blue_win=='0')/3008)

blue_win_first_fix <- cbind.data.frame(blue_win=win,blue_firstBlood=as.numeric(firstblood_fix),
                                       blue_firstTowerd=as.numeric(firstTower_fix),blue_firstDragon=as.numeric(firstDragon_fix),
                                       blue_firstBaron=as.numeric(firstBaron_fix),blue_firstInhibitor=as.numeric(firstInhibitor_fix),
                                       blue_firstRiftHerald=as.numeric(firstRiftHerald_fix))

ggradar(blue_win_first_fix,grid.line.width = 0.5)

#analyze win in purple_first  with ggradar

win <- c('Blue_Win','Purple_Win')
firstblood_fix <- c(sum(matches_data$purple_firstBlood=='1'&matches_data$blue_win=='1')/3011,sum(matches_data$purple_firstBlood=='1'&matches_data$blue_win=='0')/3011)
firstTower_fix <- c(sum(matches_data$purple_firstTower=='1'&matches_data$blue_win=='1')/3011,sum(matches_data$purple_firstTower=='1'&matches_data$blue_win=='0')/3011)
firstDragon_fix <- c(sum(matches_data$purple_firstDragon=='1'&matches_data$blue_win=='1')/3011,sum(matches_data$purple_firstDragon=='1'&matches_data$blue_win=='0')/3011)
firstBaron_fix <- c(sum(matches_data$purple_firstBaron=='1'&matches_data$blue_win=='1')/3011,sum(matches_data$purple_firstBaron=='1'&matches_data$blue_win=='0')/3011)
firstInhibitor_fix <- c(sum(matches_data$purple_firstInhibitor=='1'&matches_data$blue_win=='1')/3011,sum(matches_data$purple_firstInhibitor=='1'&matches_data$blue_win=='0')/3011)
firstRiftHerald_fix <- c(sum(matches_data$purple_firstRiftHerald=='1'&matches_data$blue_win=='1')/3011,sum(matches_data$purple_firstRiftHerald=='1'&matches_data$blue_win=='0')/3011)

purple_win_first_fix <- cbind.data.frame(purple_win=win,purple_firstBlood=as.numeric(firstblood_fix),
                                         purple_firstTowerd=as.numeric(firstTower_fix),purple_firstDragon=as.numeric(firstDragon_fix),
                                         purple_firstBaron=as.numeric(firstBaron_fix),purple_firstInhibitor=as.numeric(firstInhibitor_fix),
                                         purple_firstRiftHerald=as.numeric(firstRiftHerald_fix))

ggradar(purple_win_first_fix,grid.line.width = 0.5)

# analyze win in blue  with ggradar
win <- c('Blue_Win','Purple_Win')
blue_towerKills_fix <- c(sum(matches_data$blue_towerKills=='1'&matches_data$blue_win=='1')/1857,sum(matches_data$blue_towerKills=='1'&matches_data$blue_win=='0')/1857)
blue_inhibitorKills_fix <- c(sum(matches_data$blue_inhibitorKills=='1'&matches_data$blue_win=='1')/1857,sum(matches_data$blue_inhibitorKills=='1'&matches_data$blue_win=='0')/1857)
blue_baronKills_fix <- c(sum(matches_data$blue_baronKills=='1'&matches_data$blue_win=='1')/1857,sum(matches_data$blue_baronKills=='1'&matches_data$blue_win=='0')/1857)
blue_dragonKills_fix <- c(sum(matches_data$blue_dragonKills=='1'&matches_data$blue_win=='1')/1857,sum(matches_data$blue_dragonKills=='1'&matches_data$blue_win=='0')/1857)
blue_riftHeraldKills_fix <- c(sum(matches_data$blue_riftHeraldKills=='1'&matches_data$blue_win=='1')/1857,sum(matches_data$blue_riftHeraldKills=='1'&matches_data$blue_win=='0')/1857)

blue_win_fix <- cbind.data.frame(blue_win=win,blue_towerKills=as.numeric(blue_towerKills_fix),
                                       blue_inhibitorKills=as.numeric(blue_inhibitorKills_fix),blue_riftHeraldKills=as.numeric(blue_riftHeraldKills_fix)
                                 ,blue_dragonKills=as.numeric(blue_dragonKills_fix),
                                       blue_baronKills=as.numeric(blue_baronKills_fix)
                                       )

ggradar(blue_win_fix,grid.line.width = 0.5)

# analyze win in purple  with ggradar
win <- c('Blue_Win','Purple_Win')
purple_towerKills_fix <- c(sum(matches_data$purple_towerKills=='1'&matches_data$blue_win=='1')/1995,sum(matches_data$purple_towerKills=='1'&matches_data$blue_win=='0')/1995)
purple_inhibitorKills_fix <- c(sum(matches_data$purple_inhibitorKills=='1'&matches_data$blue_win=='1')/1995,sum(matches_data$purple_inhibitorKills=='1'&matches_data$blue_win=='0')/1995)
purple_baronKills_fix <- c(sum(matches_data$purple_baronKills=='1'&matches_data$blue_win=='1')/1995,sum(matches_data$purple_baronKills=='1'&matches_data$blue_win=='0')/1995)
purple_dragonKills_fix <- c(sum(matches_data$purple_dragonKills=='1'&matches_data$blue_win=='1')/1995,sum(matches_data$purple_dragonKills=='1'&matches_data$blue_win=='0')/1995)
purple_riftHeraldKills_fix <- c(sum(matches_data$purple_riftHeraldKills=='1'&matches_data$blue_win=='1')/1995,sum(matches_data$purple_riftHeraldKills=='1'&matches_data$blue_win=='0')/1995)

purple_win_fix <- cbind.data.frame(purple_win=win,purple_towerKills=as.numeric(purple_towerKills_fix),
                                   purple_inhibitorKills=as.numeric(purple_inhibitorKills_fix),purple_riftHeraldKills=as.numeric(purple_riftHeraldKills_fix)
                                   ,purple_dragonKills=as.numeric(purple_dragonKills_fix),purple_baronKills=as.numeric(purple_baronKills_fix)
                                         )

ggradar(purple_win_fix,grid.line.width = 0.5)
