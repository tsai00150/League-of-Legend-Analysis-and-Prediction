# --blue_purple_win_radar--
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
# --blue_purple_win_radar--


# --blue_purple_champion_final--
library(ggplot2)
library(xlsx)
library(openxlsx)
matches_data <- read.xlsx('matches_na.xlsx',sheet = 1)
matches <- na.omit(matches_data)
#analyze win in blue_champ  with point
blue_win <- matches$blue_win

top10 <- function(champ){
  col <- c()
  for (i in 1:10){
    champ_names <- names(sort(summary(champ),T))[i]
    col <- c(col,champ_names)
  }
  return(col)
}

#blue_ad_champ 使用top 10  
blue_ad_champion <- as.factor(matches_data$blue_ad_champname)
x_blue_ad <- c()
for(i in 1:10){
  champ <- top10(blue_ad_champion)[i]
  x_blue_ad <- c(x_blue_ad,champ)
}
y_blue_ad <- c()
for(i in 1:10){
  ratio <- sum(matches$blue_ad_champname==x_blue_ad[i]&matches$blue_win=='1')/sum(matches$blue_ad_champname==x_blue_ad[i])
  y_blue_ad <- c(y_blue_ad,ratio)
}

blue_ad_champ <- data.frame(champ=as.factor(x_blue_ad),winning_rate=y_blue_ad)
blue_ad_plot <- ggplot(blue_ad_champ,aes(x=champ,y=winning_rate))
blue_ad_plot+geom_col()

#blue_sup_champ 使用top 10  
blue_sup_champion<- as.factor(matches$blue_sup_champname)
x_blue_sup <- c()
for(i in 1:10){
  champ <- top10(blue_sup_champion)[i]
  x_blue_sup <- c(x_blue_sup,champ)
}
y_blue_sup <- c()
for(i in 1:10){
  ratio <- sum(matches$blue_sup_champname==x_blue_sup[i]&matches$blue_win=='1')/sum(matches$blue_sup_champname==x_blue_sup[i])
  y_blue_sup <- c(y_blue_sup,ratio)
}

blue_sup_champ <- data.frame(champ=as.factor(x_blue_sup),winning_rate=y_blue_sup)
blue_sup_plot <- ggplot(blue_sup_champ,aes(x=champ,y=winning_rate))
blue_sup_plot+geom_col()

#blue_mid_champ 使用top 10  
blue_mid_champion <- as.factor(matches$blue_mid_champname)
x_blue_mid <- c()
for(i in 1:10){
  champ <- top10(blue_mid_champion)[i]
  x_blue_mid <- c(x_blue_mid,champ)
}
y_blue_mid <- c()
for(i in 1:10){
  ratio <- sum(matches$blue_mid_champname==x_blue_mid[i]&matches$blue_win=='1')/sum(matches$blue_mid_champname==x_blue_mid[i])
  y_blue_mid <- c(y_blue_mid,ratio)
}
blue_mid_champ <- data.frame(champ=as.factor(x_blue_mid),winning_rate=y_blue_mid)
blue_mid_plot <- ggplot(blue_mid_champ,aes(x=champ,y=winning_rate))
blue_mid_plot+geom_col()

#blue_jungle_champ 使用top 10  
blue_jungle_champion<- as.factor(matches_data$blue_jungle_champname)
x_blue_jg <- c()
for(i in 1:10){
  champ <- top10(blue_jungle_champion)[i]
  x_blue_jg <- c(x_blue_jg,champ)
}
y_blue_jg <- c()
for(i in 1:10){
  ratio <- sum(matches$blue_jungle_champname==x_blue_jg[i]&matches$blue_win=='1')/sum(matches$blue_jungle_champname==x_blue_jg[i])
  y_blue_jg <- c(y_blue_jg,ratio)
}
blue_jg_champ <- data.frame(champ=as.factor(x_blue_jg),winning_rate=y_blue_jg)
blue_jg_plot <- ggplot(blue_jg_champ,aes(x=champ,y=winning_rate))
blue_jg_plot+geom_col()

#blue_top_champ 使用top 10 
blue_top_champion <- as.factor(matches$blue_top_champname)
x_blue_top <- c()
for(i in 1:10){
  champ <- top10(blue_top_champion)[i]
  x_blue_top <- c(x_blue_top,champ)
}
y_blue_top <- c()
for(i in 1:10){
  ratio <- sum(matches$blue_top_champname==x_blue_top[i]&matches$blue_win=='1')/sum(matches$blue_top_champname==x_blue_top[i])
  y_blue_top <- c(y_blue_top,ratio)
}
blue_top_champ <- data.frame(champ=as.factor(x_blue_top),winning_rate=y_blue_top)
blue_top_plot <- ggplot(blue_top_champ,aes(x=champ,y=winning_rate))
blue_top_plot+geom_col()

#analyze win in purple_champ  with point
blue_win <- matches$blue_win

#purple_ad_champ 使用top 10 
purple_ad_champion <- as.factor(matches$purple_ad_champname)
x_purple_ad <- c()
for(i in 1:10){
  champ <- top10(purple_ad_champion)[i]
  x_purple_ad <- c(x_purple_ad,champ)
}
y_purple_ad <- c()
for(i in 1:10){
  ratio <- sum(matches$purple_ad_champname==x_purple_ad[i]&matches$blue_win=='0')/sum(matches$purple_ad_champname==x_purple_ad[i])
  y_purple_ad <- c(y_purple_ad,ratio)
}
purple_ad_champ <- data.frame(champ=as.factor(x_purple_ad),winning_rate=y_purple_ad)
purple_ad_plot <- ggplot(purple_ad_champ,aes(x=champ,y=winning_rate))
purple_ad_plot+geom_col()

#purple_sup_champ 使用top 10 
purple_sup_champion<- as.factor(matches$purple_sup_champname)
x_purple_sup <- c()
for(i in 1:10){
  champ <- top10(purple_sup_champion)[i]
  x_purple_sup <- c(x_purple_sup,champ)
}
y_purple_sup <- c()
for(i in 1:10){
  ratio <- sum(matches$purple_sup_champname==x_purple_sup[i]&matches$blue_win=='0')/sum(matches$purple_sup_champname==x_purple_sup[i])
  y_purple_sup <- c(y_purple_sup,ratio)
}
purple_sup_champ <- data.frame(champ=as.factor(x_purple_sup),winning_rate=y_purple_sup)
purple_sup_plot <- ggplot(purple_sup_champ,aes(x=champ,y=winning_rate))
purple_sup_plot+geom_col()

#purple_mid_champ 使用top 10 
purple_mid_champion <- as.factor(matches$purple_mid_champname)
x_purple_mid <- c()
for(i in 1:10){
  champ <- top10(purple_mid_champion)[i]
  x_purple_mid <- c(x_purple_mid,champ)
}
y_purple_mid <- c()
for(i in 1:10){
  ratio <- sum(matches$purple_mid_champname==x_purple_mid[i]&matches$blue_win=='0')/sum(matches$purple_mid_champname==x_purple_mid[i])
  y_purple_mid <- c(y_purple_mid,ratio)
}
purple_mid_champ <- data.frame(champ=as.factor(x_purple_mid),winning_rate=y_purple_mid)
purple_mid_plot <- ggplot(purple_mid_champ,aes(x=champ,y=winning_rate))
purple_mid_plot+geom_col()

#purple_jungle_champ 使用top 10 
purple_jungle_champion<- as.factor(matches$purple_jungle_champname)
x_purple_jg <- c()
for(i in 1:10){
  champ <- top10(purple_jungle_champion)[i]
  x_purple_jg <- c(x_purple_jg,champ)
}
y_purple_jg <- c()
for(i in 1:10){
  ratio <- sum(matches$purple_jungle_champname==x_purple_jg[i]&matches$blue_win=='0')/sum(matches$purple_jungle_champname==x_purple_jg[i])
  y_purple_jg <- c(y_purple_jg,ratio)
}
purple_jg_champ <- data.frame(champ=as.factor(x_purple_jg),winning_rate=y_purple_jg)
purple_jg_plot <- ggplot(purple_jg_champ,aes(x=champ,y=winning_rate))
purple_jg_plot+geom_col()

#purple_top_champ 使用top 10 
purple_top_champion <- as.factor(matches$purple_top_champname)
x_purple_top <- c()
for(i in 1:10){
  champ <- top10(purple_top_champion)[i]
  x_purple_top <- c(x_purple_top,champ)
}
y_purple_top <- c()
for(i in 1:10){
  ratio <- sum(matches$purple_top_champname==x_purple_top[i]&matches$blue_win=='0')/sum(matches$purple_top_champname==x_purple_top[i])
  y_purple_top <- c(y_purple_top,ratio)
}
purple_top_champ <- data.frame(champ=as.factor(x_purple_top),winning_rate=y_purple_top)
purple_top_plot <- ggplot(purple_top_champ,aes(x=champ,y=winning_rate))
purple_top_plot+geom_col()
# --blue_purple_champion_final--


# --KDA--
library(ggplot2)
matches_data <- read.xlsx('matches_na.xlsx',sheet = 1)
matches <- na.omit(matches_data)
#analyze win in blue_kda in range
matches <- cbind(matches,
                 blue_ad_kad = ((matches$blue_ad_kills+matches$blue_ad_assists)/matches$blue_ad_deaths),
                 blue_sup_kad = ((matches$blue_sup_kills+matches$blue_sup_assists)/matches$blue_sup_deaths),
                 blue_mid_kad = ((matches$blue_mid_kills+matches$blue_mid_assists)/matches$blue_mid_deaths),
                 blue_jungle_kad = ((matches$blue_jungle_kills+matches$blue_jungle_assists)/matches$blue_jungle_deaths),
                 blue_top_kad = ((matches$blue_top_kills+matches$blue_top_assists)/matches$blue_top_deaths),
                 purple_ad_kad = ((matches$purple_ad_kills+matches$purple_ad_assists)/matches$purple_ad_deaths),
                 purple_sup_kad = ((matches$purple_sup_kills+matches$purple_sup_assists)/matches$purple_sup_deaths),
                 purple_mid_kad = ((matches$purple_mid_kills+matches$purple_mid_assists)/matches$purple_mid_deaths),
                 purple_jungle_kad = ((matches$purple_jungle_kills+matches$purple_jungle_assists)/matches$purple_jungle_deaths),
                 purple_top_kad = ((matches$purple_top_kills+matches$purple_top_assists)/matches$purple_top_deaths))
matches_blue_kda<-matches[,c('blue_ad_kad','blue_sup_kad','blue_mid_kad','blue_jungle_kad','blue_top_kad'
)]
cor(matches_blue_kda, method="pearson")
pairs(matches_blue_kda,spread = F,lty.smooth=2,main='blue_kda_correlation')

matches_purple_kda<-matches[,c('purple_ad_kad','purple_sup_kad','purple_mid_kad','purple_jungle_kad','purple_top_kad'
)]

cor(matches_purple_kda, method="pearson")
pairs(matches_purple_kda,spread = F,lty.smooth=2,main='purple_kda_correlation')
matches <- cbind(matches,
                 blue_ad_kad_range = ifelse(matches$blue_ad_kad>6,6,matches$blue_ad_kad),
                 blue_sup_kad_range = ifelse(matches$blue_sup_kad>6,6,matches$blue_sup_kad),
                 blue_mid_kad_range = ifelse(matches$blue_mid_kad>6,6,matches$blue_mid_kad),
                 blue_jungle_kad_range = ifelse(matches$blue_jungle_kad>6,6,matches$blue_jungle_kad),
                 blue_top_kad_range = ifelse(matches$blue_top_kad>6,6,matches$blue_top_kad),
                 purple_ad_kad_range = ifelse(matches$purple_ad_kad>6,6,matches$purple_ad_kad),
                 purple_sup_kad_range = ifelse(matches$purple_sup_kad>6,6,matches$purple_sup_kad),
                 purple_mid_kad_range = ifelse(matches$purple_mid_kad>6,6,matches$purple_mid_kad),
                 purple_jungle_kad_range = ifelse(matches$purple_jungle_kad>6,6,matches$purple_jungle_kad),
                 purple_top_kad_range = ifelse(matches$purple_top_kad>6,6,matches$purple_top_kad)
)

matches_blue_kda_range<-matches[,c('blue_ad_kad_range','blue_sup_kad_range','blue_mid_kad_range','blue_jungle_kad_range','blue_top_kad_range'
)]
cor(matches_blue_kda_range, method="pearson")
pairs(matches_blue_kda_range,spread = F,lty.smooth=2,main='blue_kda_range_correlation')
matches_purple_kda_range<-matches[,c('purple_ad_kad_range','purple_sup_kad_range','purple_mid_kad_range','purple_jungle_kad_range','purple_top_kad_range'
)]
cor(matches_purple_kda_range, method="pearson")
pairs(matches_purple_kda_range,spread = F,lty.smooth=2,main='purple_kda_range_correlation')
# --KDA--




library(shiny)
ui <- navbarPage("LOL Analysis", fluid = TRUE, 
      tabPanel("Matches", headerPanel("Match analysis: predict the winning team"), 
               plotOutput("radar_plot", height = "1000px"),
               plotOutput("champion"), 
               plotOutput("kda", height = "1000px")
               ), 
      
      
      tabPanel("Teams", headerPanel("Team analysis: find the best team")))

server <- function(input, output){
  output$radar_plot <- renderPlot(ggradar(purple_win_fix,grid.line.width = 0.5))
  output$champion <- renderPlot(purple_top_plot+geom_col())
  output$kda <- renderPlot(pairs(matches_purple_kda_range,spread = F,lty.smooth=2,main='purple_kda_range_correlation'))
  
}

shinyApp(ui=ui, server=server)