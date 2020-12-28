library(ggplot2)
matches_data <- read.xlsx('matches.xlsx',sheet = 1)
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

#有些0死的 可能需要把0視為1？

#數據齊全後 用summary看range 大於2？
#畫圖後 看有些做完range後拿去當feature建模

