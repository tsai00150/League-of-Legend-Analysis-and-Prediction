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
#选取数据的数值型数据列
#KDA比較(沒用用range) blue
matches_blue_kda<-matches[,c('blue_ad_kad','blue_sup_kad','blue_mid_kad','blue_jungle_kad','blue_top_kad'
                        )]
#计算相关系数，方法选择pearson
cor(matches_blue_kda, method="pearson")
#画出相关性矩阵图，标题：相关性矩阵图
pairs(matches_blue_kda,spread = F,lty.smooth=2,main='blue_kda_correlation')

#KDA比較(沒用用range) purple
matches_purple_kda<-matches[,c('purple_ad_kad','purple_sup_kad','purple_mid_kad','purple_jungle_kad','purple_top_kad'
)]

#计算相关系数，方法选择pearson
cor(matches_purple_kda, method="pearson")
#画出相关性矩阵图，标题：相关性矩阵图
pairs(matches_purple_kda,spread = F,lty.smooth=2,main='purple_kda_correlation')

#我用summary看 決定用 2 4 6區分 分成四個區間 發現效果不好 改成大於6的視為6
#blue_ad_kad.fix <- ifelse(matches$blue_ad_kad<2,1,ifelse(matches$blue_ad_kad<4,2,ifelse(matches$blue_ad_kad<6,3,4)))
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
#计算相关系数，方法选择pearson
cor(matches_blue_kda_range, method="pearson")
#画出相关性矩阵图，标题：相关性矩阵图
pairs(matches_blue_kda_range,spread = F,lty.smooth=2,main='blue_kda_range_correlation')

#KDA比較(沒用用range) purple
matches_purple_kda_range<-matches[,c('purple_ad_kad_range','purple_sup_kad_range','purple_mid_kad_range','purple_jungle_kad_range','purple_top_kad_range'
)]

#计算相关系数，方法选择pearson
cor(matches_purple_kda_range, method="pearson")
#画出相关性矩阵图，标题：相关性矩阵图
pairs(matches_purple_kda_range,spread = F,lty.smooth=2,main='purple_kda_range_correlation')


#KDA取ln
matches <- cbind(matches,
                 blue_ad_kad_ln = log(matches$blue_ad_kad+1),
                 blue_sup_kad_ln = log(matches$blue_sup_kad+1),
                 blue_mid_kad_ln = log(matches$blue_mid_kad+1),
                 blue_jungle_kad_ln = log(matches$blue_jungle_kad+1),
                 blue_top_kad_ln = log(matches$blue_top_kad+1),
                 purple_ad_kad_ln = log(matches$purple_ad_kad+1),
                 purple_sup_kad_ln = log(matches$purple_sup_kad+1),
                 purple_mid_kad_ln = log(matches$purple_mid_kad+1),
                 purple_jungle_kad_ln = log(matches$purple_jungle_kad+1),
                 purple_top_kad_ln = log(matches$purple_top_kad+1)
)

matches_blue_kda_ln<-matches[,c('blue_ad_kad_ln','blue_sup_kad_ln','blue_mid_kad_ln','blue_jungle_kad_ln','blue_top_kad_ln'
)]
#计算相关系数，方法选择pearson
cor(matches_blue_kda_ln, method="pearson")
#画出相关性矩阵图，标题：相关性矩阵图
pairs(matches_blue_kda_range,spread = F,lty.smooth=2,main='blue_kda_ln_correlation')

#KDA比較(沒用用range) purple
matches_purple_kda_ln<-matches[,c('purple_ad_kad_ln','purple_sup_kad_ln','purple_mid_kad_ln','purple_jungle_kad_ln','purple_top_kad_ln'
)]

#计算相关系数，方法选择pearson
cor(matches_purple_kda_ln, method="pearson")
#画出相关性矩阵图，标题：相关性矩阵图
pairs(matches_purple_kda_ln,spread = F,lty.smooth=2,main='purple_kda_ln_correlation')



