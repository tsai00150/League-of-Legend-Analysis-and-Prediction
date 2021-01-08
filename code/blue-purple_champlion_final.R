library(ggplot2)
matches_data <- read.xlsx('matches_final.xlsx',sheet = 1)
matches <- na.omit(matches_data)
#analyze win in blue_champ  with point
blue_win <- matches$blue_win

#blue_ad_champ 使用top 10  (Thresh,Jhin,Ezreal,Leona,Lux,Kaisa,Blitzcrank,Morgana,Ashe,Senna)
blue_ad_champion <- as.factor(matches$blue_ad_champname)
x_ad <- c('Thresh','Jhin','Ezreal','Leona','Lux','Kaisa','Blitzcrank','Morgana','Ashe','Senna')
y_ad <- c(sum(matches$blue_ad_champname=='Thresh'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Thresh'),
       sum(matches$blue_ad_champname=='Jhin'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Jhin'),
       sum(matches$blue_ad_champname=='Ezreal'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Ezreal'),
       sum(matches$blue_ad_champname=='Leona'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Leona'),
       sum(matches$blue_ad_champname=='Lux'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Lux'),
       sum(matches$blue_ad_champname=='Kaisa'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Kaisa'),
       sum(matches$blue_ad_champname=='Blitzcrank'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Blitzcrank'),
       sum(matches$blue_ad_champname=='Morgana'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Morgana'),
       sum(matches$blue_ad_champname=='Ashe'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Ashe'),
       sum(matches$blue_ad_champname=='Senna'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Senna'))
blue_ad_champ <- data.frame(champ=as.factor(x_ad),winning_rate=y_ad)
ad_plot <- ggplot(blue_ad_champ,aes(x=champ,y=winning_rate))
ad_plot+geom_point()

#blue_sup_champ 使用top 10  (Thresh,Leona,Nautilus,Morgana,Pyke,Nami,Senna,Camille,Lux,Yuumi)
blue_sup_champion<- as.factor(matches$blue_sup_champname)
x_sup <- c('Thresh','Leona','Nautilus','Morgana','Pyke','Nami','Senna','Camille','Lux','Yuumi')
y_sup <-  c(sum(matches$blue_ad_champname=='Thresh'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Thresh'),
            sum(matches$blue_ad_champname=='Leona'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Leona'),
            sum(matches$blue_ad_champname=='Nautilus'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Nautilus'),
            sum(matches$blue_ad_champname=='Morgana'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Morgana'),
            sum(matches$blue_ad_champname=='Pyke'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Pyke'),
            sum(matches$blue_ad_champname=='Nami'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Nami'),
            sum(matches$blue_ad_champname=='Senna'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Senna'),
            sum(matches$blue_ad_champname=='Camille'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Camille'),
            sum(matches$blue_ad_champname=='Lux'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Lux'),
            sum(matches$blue_ad_champname=='Yuumi'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Yuumi'))
blue_sup_champ <- data.frame(champ=as.factor(x_sup),winning_rate=y_sup)
sup_plot <- ggplot(blue_sup_champ,aes(x=champ,y=winning_rate))
sup_plot+geom_point()

#blue_mid_champ 使用top 10  (Akali,Camille,Sylas,Garen,Irelia,Malphite,Sett,Renekton,Darius,Aatrox)
blue_mid_champion <- as.factor(matches$blue_mid_champname)
x_mid <- c('Akali','Camille','Sylas','Garen','Irelia','Malphite','Sett','Renekton','Darius','Aatrox')
y_mid <- c(sum(matches$blue_ad_champname=='Akali'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Akali'),
           sum(matches$blue_ad_champname=='Camille'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Camille'),
           sum(matches$blue_ad_champname=='Sylas'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Sylas'),
           sum(matches$blue_ad_champname=='Garen'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Garen'),
           sum(matches$blue_ad_champname=='Irelia'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Irelia'),
           sum(matches$blue_ad_champname=='Malphite'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Malphite'),
           sum(matches$blue_ad_champname=='Sett'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Sett'),
           sum(matches$blue_ad_champname=='Renekton'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Renekton'),
           sum(matches$blue_ad_champname=='Darius'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Darius'),
           sum(matches$blue_ad_champname=='Aatrox'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Aatrox'))
blue_mid_champ <- data.frame(champ=as.factor(x_mid),winning_rate=y_mid)
mid_plot <- ggplot(blue_mid_champ,aes(x=champ,y=winning_rate))
mid_plot+geom_point()

#blue_jungle_champ 使用top 10  ('Ezreal','Jhin','Kaisa','Ashe','Caitlyn','Lucian','MissFortune','Samira','Vayne','Graves')
blue_jungle_champion<- as.factor(matches$blue_jungle_champname)
x_jg <- c('Ezreal','Jhin','Kaisa','Ashe','Caitlyn','Lucian','MissFortune','Samira','Vayne','Graves')
y_jg <- c(sum(matches$blue_ad_champname=='Ezreal'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Ezreal'),
           sum(matches$blue_ad_champname=='Jhin'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Jhin'),
           sum(matches$blue_ad_champname=='Kaisa'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Kaisa'),
           sum(matches$blue_ad_champname=='Ashe'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Ashe'),
           sum(matches$blue_ad_champname=='Caitlyn'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Caitlyn'),
           sum(matches$blue_ad_champname=='Lucian'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Lucian'),
           sum(matches$blue_ad_champname=='MissFortune'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='MissFortune'),
           sum(matches$blue_ad_champname=='Samira'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Samira'),
           sum(matches$blue_ad_champname=='Vayne'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Vayne'),
           sum(matches$blue_ad_champname=='Graves'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Graves'))
blue_jg_champ <- data.frame(champ=as.factor(x_jg),winning_rate=y_jg)
jg_plot <- ggplot(blue_jg_champ,aes(x=champ,y=winning_rate))
jg_plot+geom_point()

#blue_top_champ 使用top 10 ('LeeSin','Graves','Malphite','Camille','Darius','Akali','Jax','Irelia','Kayn','Ekko')
blue_top_champion <- as.factor(matches$blue_top_champname)
x_top <- c('LeeSin','Graves','Malphite','Camille','Darius','Akali','Jax','Irelia','Kayn','Ekko')
y_top <- c(sum(matches$blue_ad_champname=='LeeSin'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='LeeSin'),
          sum(matches$blue_ad_champname=='Graves'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Graves'),
          sum(matches$blue_ad_champname=='Malphite'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Malphite'),
          sum(matches$blue_ad_champname=='Camille'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Camille'),
          sum(matches$blue_ad_champname=='Darius'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Darius'),
          sum(matches$blue_ad_champname=='Akali'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Akali'),
          sum(matches$blue_ad_champname=='Jax'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Jax'),
          sum(matches$blue_ad_champname=='Irelia'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Irelia'),
          sum(matches$blue_ad_champname=='Kayn'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Kayn'),
          sum(matches$blue_ad_champname=='Ekko'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Ekkoc'))
blue_top_champ <- data.frame(champ=as.factor(x_top),winning_rate=y_top)
top_plot <- ggplot(blue_top_champ,aes(x=champ,y=winning_rate))
top_plot+geom_point()

#analyze win in purple_champ  with point
blue_win <- matches$blue_win

#purple_ad_champ 使用top 10  ('Thresh','Ezreal','Jhin','Kaisa','Ashe','Lux','Leona','Caitlyn','MissFortune','Lucian')
purple_ad_champion <- as.factor(matches$purple_ad_champname)
x_ad <- c('Thresh','Ezreal','Jhin','Kaisa','Ashe','Lux','Leona','Caitlyn','MissFortune','Lucian')
y_ad <- c(sum(matches$blue_ad_champname=='Thresh'&matches$blue_win=='0')/sum(matches$blue_ad_champname=='Thresh'),
          sum(matches$blue_ad_champname=='Ezreal'&matches$blue_win=='0')/sum(matches$blue_ad_champname=='Ezreal'),
          sum(matches$blue_ad_champname=='Jhin'&matches$blue_win=='0')/sum(matches$blue_ad_champname=='Jhin'),
          sum(matches$blue_ad_champname=='Kaisa'&matches$blue_win=='0')/sum(matches$blue_ad_champname=='Kaisa'),
          sum(matches$blue_ad_champname=='Ashe'&matches$blue_win=='0')/sum(matches$blue_ad_champname=='Ashe'),
          sum(matches$blue_ad_champname=='Lux'&matches$blue_win=='0')/sum(matches$blue_ad_champname=='Lux'),
          sum(matches$blue_ad_champname=='Leona'&matches$blue_win=='0')/sum(matches$blue_ad_champname=='Leona'),
          sum(matches$blue_ad_champname=='Caitlyn'&matches$blue_win=='0')/sum(matches$blue_ad_champname=='Caitlyn'),
          sum(matches$blue_ad_champname=='MissFortune'&matches$blue_win=='0')/sum(matches$blue_ad_champname=='MissFortune'),
          sum(matches$blue_ad_champname=='Lucian'&matches$blue_win=='0')/sum(matches$blue_ad_champname=='Lucian'))
purple_ad_champ <- data.frame(champ=as.factor(x_ad),winning_rate=y_ad)
ad_plot <- ggplot(purple_ad_champ,aes(x=champ,y=winning_rate))
ad_plot+geom_point()

#purple_sup_champ 使用top 10  (Thresh,Leona,Nautilus,Morgana,Pyke,Nami,Senna,Camille,Lux,Yuumi)
purple_sup_champion<- as.factor(matches$purple_sup_champname)
x_sup <- c('Akali','Yasuo','Zed','Sylas','Thresh','Lux','Yone','Lucian','Katarina','Ahri')
y_sup <-  c(sum(matches$blue_ad_champname=='Thresh'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Thresh'),
            sum(matches$blue_ad_champname=='Leona'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Leona'),
            sum(matches$blue_ad_champname=='Nautilus'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Nautilus'),
            sum(matches$blue_ad_champname=='Morgana'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Morgana'),
            sum(matches$blue_ad_champname=='Pyke'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Pyke'),
            sum(matches$blue_ad_champname=='Nami'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Nami'),
            sum(matches$blue_ad_champname=='Senna'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Senna'),
            sum(matches$blue_ad_champname=='Camille'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Camille'),
            sum(matches$blue_ad_champname=='Lux'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Lux'),
            sum(matches$blue_ad_champname=='Yuumi'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Yuumi'))
blue_sup_champ <- data.frame(champ=as.factor(x_sup),winning_rate=y_sup)
sup_plot <- ggplot(blue_sup_champ,aes(x=champ,y=winning_rate))
sup_plot+geom_point()

#blue_mid_champ 使用top 10  (Akali,Camille,Sylas,Garen,Irelia,Malphite,Sett,Renekton,Darius,Aatrox)
blue_mid_champion <- as.factor(matches$blue_mid_champname)
x_mid <- c('Akali','Camille','Sylas','Garen','Irelia','Malphite','Sett','Renekton','Darius','Aatrox')
y_mid <- c(sum(matches$blue_ad_champname=='Akali'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Akali'),
           sum(matches$blue_ad_champname=='Camille'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Camille'),
           sum(matches$blue_ad_champname=='Sylas'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Sylas'),
           sum(matches$blue_ad_champname=='Garen'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Garen'),
           sum(matches$blue_ad_champname=='Irelia'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Irelia'),
           sum(matches$blue_ad_champname=='Malphite'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Malphite'),
           sum(matches$blue_ad_champname=='Sett'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Sett'),
           sum(matches$blue_ad_champname=='Renekton'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Renekton'),
           sum(matches$blue_ad_champname=='Darius'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Darius'),
           sum(matches$blue_ad_champname=='Aatrox'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Aatrox'))
blue_mid_champ <- data.frame(champ=as.factor(x_mid),winning_rate=y_mid)
mid_plot <- ggplot(blue_mid_champ,aes(x=champ,y=winning_rate))
mid_plot+geom_point()

#blue_jungle_champ 使用top 10  ('Ezreal','Jhin','Kaisa','Ashe','Caitlyn','Lucian','MissFortune','Samira','Vayne','Graves')
blue_jungle_champion<- as.factor(matches$blue_jungle_champname)
x_jg <- c('Ezreal','Jhin','Kaisa','Ashe','Caitlyn','Lucian','MissFortune','Samira','Vayne','Graves')
y_jg <- c(sum(matches$blue_ad_champname=='Ezreal'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Ezreal'),
          sum(matches$blue_ad_champname=='Jhin'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Jhin'),
          sum(matches$blue_ad_champname=='Kaisa'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Kaisa'),
          sum(matches$blue_ad_champname=='Ashe'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Ashe'),
          sum(matches$blue_ad_champname=='Caitlyn'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Caitlyn'),
          sum(matches$blue_ad_champname=='Lucian'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Lucian'),
          sum(matches$blue_ad_champname=='MissFortune'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='MissFortune'),
          sum(matches$blue_ad_champname=='Samira'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Samira'),
          sum(matches$blue_ad_champname=='Vayne'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Vayne'),
          sum(matches$blue_ad_champname=='Graves'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Graves'))
blue_jg_champ <- data.frame(champ=as.factor(x_jg),winning_rate=y_jg)
jg_plot <- ggplot(blue_jg_champ,aes(x=champ,y=winning_rate))
jg_plot+geom_point()

#blue_top_champ 使用top 10 ('LeeSin','Graves','Malphite','Camille','Darius','Akali','Jax','Irelia','Kayn','Ekko')
blue_top_champion <- as.factor(matches$blue_top_champname)
x_top <- c('LeeSin','Graves','Malphite','Camille','Darius','Akali','Jax','Irelia','Kayn','Ekko')
y_top <- c(sum(matches$blue_ad_champname=='LeeSin'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='LeeSin'),
           sum(matches$blue_ad_champname=='Graves'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Graves'),
           sum(matches$blue_ad_champname=='Malphite'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Malphite'),
           sum(matches$blue_ad_champname=='Camille'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Camille'),
           sum(matches$blue_ad_champname=='Darius'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Darius'),
           sum(matches$blue_ad_champname=='Akali'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Akali'),
           sum(matches$blue_ad_champname=='Jax'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Jax'),
           sum(matches$blue_ad_champname=='Irelia'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Irelia'),
           sum(matches$blue_ad_champname=='Kayn'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Kayn'),
           sum(matches$blue_ad_champname=='Ekko'&matches$blue_win=='1')/sum(matches$blue_ad_champname=='Ekkoc'))
blue_top_champ <- data.frame(champ=as.factor(x_top),winning_rate=y_top)
top_plot <- ggplot(blue_top_champ,aes(x=champ,y=winning_rate))
top_plot+geom_point()
