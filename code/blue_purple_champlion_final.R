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
