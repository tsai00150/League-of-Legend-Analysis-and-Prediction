library(ggplot2)
matches_data <- read.xlsx('matches.xlsx',sheet = 1)
matches <- na.omit(matches_data)
#analyze win in blue_champ  with point
blue_win <- ifelse((matches$blue_win=='Win'),1,0)

#blue_ad_champ 使用top 10  (202,81,145,22,236,51,21,67,360,222)
blue_ad_champion <- as.factor(matches$blue_ad_champion)
x <- c(202,81,145,22,236,51,21,67,360,222)
y <- c(sum(matches$blue_ad_champion=='202'&matches$blue_win=='Win')/sum(matches$blue_ad_champion=='202'),
       sum(matches$blue_ad_champion=='81'&matches$blue_win=='Win')/sum(matches$blue_ad_champion=='81'),
       sum(matches$blue_ad_champion=='145'&matches$blue_win=='Win')/sum(matches$blue_ad_champion=='145'),
       sum(matches$blue_ad_champion=='22'&matches$blue_win=='Win')/sum(matches$blue_ad_champion=='22'),
       sum(matches$blue_ad_champion=='236'&matches$blue_win=='Win')/sum(matches$blue_ad_champion=='236'),
       sum(matches$blue_ad_champion=='51'&matches$blue_win=='Win')/sum(matches$blue_ad_champion=='51'),
       sum(matches$blue_ad_champion=='21'&matches$blue_win=='Win')/sum(matches$blue_ad_champion=='21'),
       sum(matches$blue_ad_champion=='67'&matches$blue_win=='Win')/sum(matches$blue_ad_champion=='67'),
       sum(matches$blue_ad_champion=='360'&matches$blue_win=='Win')/sum(matches$blue_ad_champion=='360'),
       sum(matches$blue_ad_champion=='222'&matches$blue_win=='Win')/sum(matches$blue_ad_champion=='222'))
blue_ad_champ <- data.frame(champ=as.factor(x),winning_rate=y)
c <- ggplot(blue_ad_champ,aes(x=champ,y=winning_rate))
c+geom_point()

#blue_sup_champ 使用top 10 (412,89,99,111,350,555,25,53,117,12)
blue_sup_champion <- as.factor(matches$blue_sup_champion)
x <- c(412,89,99,111,350,555,25,53,117,12)
y <- c(sum(matches$blue_sup_champion=='412'&matches$blue_win=='Win')/sum(matches$blue_sup_champion=='412'),
       sum(matches$blue_sup_champion=='89'&matches$blue_win=='Win')/sum(matches$blue_sup_champion=='89'),
       sum(matches$blue_sup_champion=='99'&matches$blue_win=='Win')/sum(matches$blue_sup_champion=='99'),
       sum(matches$blue_sup_champion=='111'&matches$blue_win=='Win')/sum(matches$blue_sup_champion=='111'),
       sum(matches$blue_sup_champion=='350'&matches$blue_win=='Win')/sum(matches$blue_sup_champion=='350'),
       sum(matches$blue_sup_champion=='555'&matches$blue_win=='Win')/sum(matches$blue_sup_champion=='555'),
       sum(matches$blue_sup_champion=='25'&matches$blue_win=='Win')/sum(matches$blue_sup_champion=='25'),
       sum(matches$blue_sup_champion=='53'&matches$blue_win=='Win')/sum(matches$blue_sup_champion=='53'),
       sum(matches$blue_sup_champion=='117'&matches$blue_win=='Win')/sum(matches$blue_sup_champion=='117'),
       sum(matches$blue_sup_champion=='12'&matches$blue_win=='Win')/sum(matches$blue_sup_champion=='12'))
blue_sup_champ <- data.frame(champ=as.factor(x),winning_rate=y)
c <- ggplot(blue_sup_champ,aes(x=champ,y=winning_rate))
c+geom_point()

#blue_mid_champ 使用top 10 (84,157,517,238,777,103,61,55,7,245)
blue_mid_champion <- as.factor(matches$blue_mid_champion)
x <- c(84,157,517,238,777,103,61,55,7,245)
y <- c(sum(matches$blue_mid_champion=='84'&matches$blue_win=='Win')/sum(matches$blue_mid_champion=='84'),
       sum(matches$blue_mid_champion=='157'&matches$blue_win=='Win')/sum(matches$blue_mid_champion=='157'),
       sum(matches$blue_mid_champion=='517'&matches$blue_win=='Win')/sum(matches$blue_mid_champion=='517'),
       sum(matches$blue_mid_champion=='238'&matches$blue_win=='Win')/sum(matches$blue_mid_champion=='238'),
       sum(matches$blue_mid_champion=='777'&matches$blue_win=='Win')/sum(matches$blue_mid_champion=='777'),
       sum(matches$blue_mid_champion=='103'&matches$blue_win=='Win')/sum(matches$blue_mid_champion=='103'),
       sum(matches$blue_mid_champion=='61'&matches$blue_win=='Win')/sum(matches$blue_mid_champion=='61'),
       sum(matches$blue_mid_champion=='55'&matches$blue_win=='Win')/sum(matches$blue_mid_champion=='55'),
       sum(matches$blue_mid_champion=='7'&matches$blue_win=='Win')/sum(matches$blue_mid_champion=='7'),
       sum(matches$blue_mid_champion=='245'&matches$blue_win=='Win')/sum(matches$blue_mid_champion=='245'))
blue_mid_champ <- data.frame(champ=as.factor(x),winning_rate=y)
c <- ggplot(blue_mid_champ,aes(x=champ,y=winning_rate))
c+geom_point()

#blue_jungle_champ 使用top 10 (104,64,141,120,121,876,245,11,203,60)
#order(summary(blue_jungle_champion),decreasing = T)
blue_jungle_champion <- as.factor(matches$blue_jungle_champion)
x <- c(104,64,141,120,121,876,245,11,203,60)
y <- c(sum(matches$blue_jungle_champion=='104'&matches$blue_win=='Win')/sum(matches$blue_jungle_champion=='104'),
       sum(matches$blue_jungle_champion=='64'&matches$blue_win=='Win')/sum(matches$blue_jungle_champion=='64'),
       sum(matches$blue_jungle_champion=='141'&matches$blue_win=='Win')/sum(matches$blue_jungle_champion=='141'),
       sum(matches$blue_jungle_champion=='120'&matches$blue_win=='Win')/sum(matches$blue_jungle_champion=='120'),
       sum(matches$blue_jungle_champion=='121'&matches$blue_win=='Win')/sum(matches$blue_jungle_champion=='121'),
       sum(matches$blue_jungle_champion=='876'&matches$blue_win=='Win')/sum(matches$blue_jungle_champion=='876'),
       sum(matches$blue_jungle_champion=='245'&matches$blue_win=='Win')/sum(matches$blue_jungle_champion=='245'),
       sum(matches$blue_jungle_champion=='11'&matches$blue_win=='Win')/sum(matches$blue_jungle_champion=='11'),
       sum(matches$blue_jungle_champion=='203'&matches$blue_win=='Win')/sum(matches$blue_jungle_champion=='203'),
       sum(matches$blue_jungle_champion=='60'&matches$blue_win=='Win')/sum(matches$blue_jungle_champion=='60'))
blue_jungle_champ <- data.frame(champ=as.factor(x),winning_rate=y)
c <- ggplot(blue_jungle_champ,aes(x=champ,y=winning_rate))
c+geom_point()    

#blue_top_champ 使用top 10 (164,54,58,98,39,114,122,24,516,86)
blue_top_champion <- as.factor(matches$blue_top_champion)
x <- c(164,54,58,98,39,114,122,24,516,86)
y <- c(sum(matches$blue_top_champion=='164'&matches$blue_win=='Win')/sum(matches$blue_top_champion=='164'),
       sum(matches$blue_top_champion=='54'&matches$blue_win=='Win')/sum(matches$blue_top_champion=='54'),
       sum(matches$blue_top_champion=='58'&matches$blue_win=='Win')/sum(matches$blue_top_champion=='58'),
       sum(matches$blue_top_champion=='98'&matches$blue_win=='Win')/sum(matches$blue_top_champion=='98'),
       sum(matches$blue_top_champion=='39'&matches$blue_win=='Win')/sum(matches$blue_top_champion=='39'),
       sum(matches$blue_top_champion=='114'&matches$blue_win=='Win')/sum(matches$blue_top_champion=='114'),
       sum(matches$blue_top_champion=='122'&matches$blue_win=='Win')/sum(matches$blue_top_champion=='122'),
       sum(matches$blue_top_champion=='24'&matches$blue_win=='Win')/sum(matches$blue_top_champion=='24'),
       sum(matches$blue_top_champion=='516'&matches$blue_win=='Win')/sum(matches$blue_top_champion=='516'),
       sum(matches$blue_top_champion=='86'&matches$blue_win=='Win')/sum(matches$blue_top_champion=='86'))
blue_top_champ <- data.frame(champ=as.factor(x),winning_rate=y)
c <- ggplot(blue_top_champ,aes(x=champ,y=winning_rate))
c+geom_point()    

#analyze win in purple_champ  with point
#purple_ad_champ 使用top 10 (81,202,145,22,51,21,236,67,222,360)
purple_ad_champion <- as.factor(matches$purple_ad_champion)
x <- c(81,202,145,22,51,21,236,67,222,360)
y <- c(sum(matches$purple_ad_champion=='81'&matches$blue_win=='Fail')/sum(matches$purple_ad_champion=='81'),
       sum(matches$purple_ad_champion=='202'&matches$blue_win=='Fail')/sum(matches$purple_ad_champion=='202'),
       sum(matches$purple_ad_champion=='145'&matches$blue_win=='Fail')/sum(matches$purple_ad_champion=='145'),
       sum(matches$purple_ad_champion=='22'&matches$blue_win=='Fail')/sum(matches$purple_ad_champion=='22'),
       sum(matches$purple_ad_champion=='51'&matches$blue_win=='Fail')/sum(matches$purple_ad_champion=='51'),
       sum(matches$purple_ad_champion=='21'&matches$blue_win=='Fail')/sum(matches$purple_ad_champion=='21'),
       sum(matches$purple_ad_champion=='236'&matches$blue_win=='Fail')/sum(matches$purple_ad_champion=='236'),
       sum(matches$purple_ad_champion=='67'&matches$blue_win=='Fail')/sum(matches$purple_ad_champion=='67'),
       sum(matches$purple_ad_champion=='222'&matches$blue_win=='Fail')/sum(matches$purple_ad_champion=='222'),
       sum(matches$purple_ad_champion=='360'&matches$blue_win=='Fail')/sum(matches$purple_ad_champion=='360'))
purple_ad_champ <- data.frame(champ=as.factor(x),winning_rate=y)
c <- ggplot(purple_ad_champ,aes(x=champ,y=winning_rate))
c+geom_point()   

#purple_sup_champ 使用top 10 (412,89,555,111,99,53,25,350,117,80)
purple_sup_champion <- as.factor(matches$purple_sup_champion)
x <- c(412,89,555,111,99,53,25,350,117,80)
y <- c(sum(matches$purple_sup_champion=='412'&matches$blue_win=='Fail')/sum(matches$purple_sup_champion=='412'),
       sum(matches$purple_sup_champion=='89'&matches$blue_win=='Fail')/sum(matches$purple_sup_champion=='89'),
       sum(matches$purple_sup_champion=='555'&matches$blue_win=='Fail')/sum(matches$purple_sup_champion=='555'),
       sum(matches$purple_sup_champion=='111'&matches$blue_win=='Fail')/sum(matches$purple_sup_champion=='111'),
       sum(matches$purple_sup_champion=='99'&matches$blue_win=='Fail')/sum(matches$purple_sup_champion=='99'),
       sum(matches$purple_sup_champion=='53'&matches$blue_win=='Fail')/sum(matches$purple_sup_champion=='53'),
       sum(matches$purple_sup_champion=='25'&matches$blue_win=='Fail')/sum(matches$purple_sup_champion=='25'),
       sum(matches$purple_sup_champion=='350'&matches$blue_win=='Fail')/sum(matches$purple_sup_champion=='350'),
       sum(matches$purple_sup_champion=='117'&matches$blue_win=='Fail')/sum(matches$purple_sup_champion=='117'),
       sum(matches$purple_sup_champion=='80'&matches$blue_win=='Fail')/sum(matches$purple_sup_champion=='80'))
purple_sup_champ <- data.frame(champ=as.factor(x),winning_rate=y)
c <- ggplot(purple_sup_champ,aes(x=champ,y=winning_rate))
c+geom_point()   

#purple_mid_champ 使用top 10 (84,238,157,517,777,55,105,245,61,112)
purple_mid_champion <- as.factor(matches$purple_mid_champion)
x <- c(84,238,157,517,777,55,105,245,61,112)
y <- c(sum(matches$purple_mid_champion=='84'&matches$blue_win=='Fail')/sum(matches$purple_mid_champion=='84'),
       sum(matches$purple_mid_champion=='238'&matches$blue_win=='Fail')/sum(matches$purple_mid_champion=='238'),
       sum(matches$purple_mid_champion=='157'&matches$blue_win=='Fail')/sum(matches$purple_mid_champion=='157'),
       sum(matches$purple_mid_champion=='517'&matches$blue_win=='Fail')/sum(matches$purple_mid_champion=='517'),
       sum(matches$purple_mid_champion=='777'&matches$blue_win=='Fail')/sum(matches$purple_mid_champion=='777'),
       sum(matches$purple_mid_champion=='55'&matches$blue_win=='Fail')/sum(matches$purple_mid_champion=='55'),
       sum(matches$purple_mid_champion=='105'&matches$blue_win=='Fail')/sum(matches$purple_mid_champion=='105'),
       sum(matches$purple_mid_champion=='245'&matches$blue_win=='Fail')/sum(matches$purple_mid_champion=='245'),
       sum(matches$purple_mid_champion=='61'&matches$blue_win=='Fail')/sum(matches$purple_mid_champion=='61'),
       sum(matches$purple_mid_champion=='112'&matches$blue_win=='Fail')/sum(matches$purple_mid_champion=='112'))
purple_mid_champ <- data.frame(champ=as.factor(x),winning_rate=y)
c <- ggplot(purple_mid_champ,aes(x=champ,y=winning_rate))
c+geom_point()   

#purple_jungle_champ 使用top 10

##purple_top_champ 使用top 10



