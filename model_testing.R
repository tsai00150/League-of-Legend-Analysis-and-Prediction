library('rpart')
library('dplyr')
library('tidyr')
library('readxl')
library('openxlsx')
library(e1071)

num_of_folds <- 5

data <- read_excel("dataset/matches_final.xlsx")
sum(is.na(data))

# drop rows with NA values
data <- data %>% drop_na()
sum(is.na(data))

# get rid of player name 
data <- data[-c(3)]
data <- data[-c(6)]
data <- data[-c(299:308)]

set.seed(123)

# split data into 7:3 train/test
new_data <- data[sample(nrow(data)),]
train_file <- new_data[1:6257,]
test_file <- new_data[6258:7821,]
test_ans <- test_file$blue_win
test_file$blue_win <- NA

# randomly shuffle the input data
rand_data <- train_file[sample(nrow(train_file)),]

# create folds according to user input parameter
folds <- cut(seq(1, nrow(rand_data)), breaks = num_of_folds, labels = F)


#####################################################################################
################################## Decision Tree ####################################
#####################################################################################
matches_data <- read.xlsx('dataset/matches_na.xlsx', sheet = 1)
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

data <- matches
# split data into 7:3 train/test
new_data <- data[sample(nrow(data)),]
# train_file <- new_data[1:6257,]
# test_file <- new_data[6258:7821,]
train_file <- new_data
test_ans <- test_file$blue_win
test_file$blue_win <- NA

# randomly shuffle the input data
rand_data <- train_file[sample(nrow(train_file)),]

# create folds according to user input parameter
folds <- cut(seq(1, nrow(rand_data)), breaks = num_of_folds, labels = F)

# create empty report data frame
perform_df <- data.frame(set=0, training=0, validation=0, test=0)[-1,]

# Perform k-fold cross validation
for(i in 1:num_of_folds){
  print(paste('-------------- fold', i),)
  # split data into train, test, validation
  test_index <- which(folds == i, arr.ind= T)
  
  if(i == num_of_folds){
    val_index <- which(folds == 1, arr.ind = T)
  }else{
    val_index <- which(folds == i+1, arr.ind = T)
  }
  
  test_data <- rand_data[test_index,]
  val_data <- rand_data[val_index,]
  train_data <- rand_data[-c(test_index, val_index),]
  
  
  # select best decision tree model
  val_df <- data.frame()
  
  for(j in 4:10){
    model <- rpart(blue_win ~ blue_ad_kad+blue_sup_kad+blue_mid_kad+blue_jungle_kad+blue_top_kad+blue_firstTower+blue_firstInhibitor+blue_firstBaron,
                   data=train_data, control=rpart.control(maxdepth=j, minsplit=2),
                   method = 'class')
    # calibrate model using validation fold
    resultframe <- data.frame(truth=val_data$blue_win,
                              pred=predict(model, val_data, type = 'class'))
    rtab <- table(resultframe)
    accuracy <- sum(diag(rtab))/sum(rtab)
    val_df[j,1] = accuracy
  }
  best_depth <- which.max(val_df[,1])
  
  # select best model
  new_model <- rpart(blue_win ~ blue_ad_kad+blue_sup_kad+blue_mid_kad+blue_jungle_kad+blue_top_kad+blue_firstTower+blue_firstInhibitor+blue_firstBaron,
                     data=train_data, control=rpart.control(maxdepth=best_depth, minsplit=2),
                     method = 'class')
  
  
  # output training, validation, testing performance
  train_res <- data.frame(truth=train_data$blue_win,
                          pred=predict(new_model, train_data, type = 'class'))
  val_res <- data.frame(truth=val_data$blue_win,
                        pred=predict(new_model, val_data, type = 'class'))
  test_res <- data.frame(truth=test_data$blue_win,
                         pred=predict(new_model, test_data, type = 'class'))
  
  train_tab <- table(train_res)
  val_tab <- table(val_res)
  test_tab <- table(test_res)
  
  train_performance <- round(sum(diag(train_tab))/sum(train_tab), 2)
  val_performance <- round(sum(diag(val_tab))/sum(val_tab), 2)
  test_performance <- round(sum(diag(test_tab))/sum(test_tab), 2)
  
  new_row <- data.frame(set=paste('fold',i,sep=''), training=train_performance, validation=val_performance, test=test_performance)
  perform_df <- rbind(perform_df, new_row)
}

# create average row of performance report
avg_row <- data.frame(set='ave.', training=round(mean(perform_df$training),2), validation=round(mean(perform_df$validation),2), test=round(mean(perform_df$test),2))
perform_df <- rbind(perform_df, avg_row)

# write to output file
write.csv(perform_df, 'results/decision_tree_performance.csv', row.names=FALSE, quote=FALSE)


# predict using the test data with model
final_res <- data.frame(blue_win=predict(new_model, test_file))
final_tab <- table(final_res$blue_win.1)
final_performance <- round(sum(diag(final_tab))/sum(final_tab), 2)

predict_df <- data.frame(ID=test_file$index, blue_win=final_res$blue_win.1, ans=test_ans)
predict_df$blue_win <- ifelse(predict_df$blue_win>=0.5, 1, 0)
write.csv(predict_df, 'results/predict.csv', row.names=FALSE, quote=FALSE)

score <- 0
for(i in c(1:length(predict_df$blue_win))){
  if(predict_df$blue_win[i]==predict_df$ans[i]){
    score = score + 1
  }
}

percentage <- score/length(predict_df$blue_win)*100
print(percentage)


#####################################################################################
############################# Logistic Regression ###################################
#####################################################################################

# create empty report data frame
perform_df <- data.frame(set=0, training=0, validation=0, test=0)[-1,]

# Perform k-fold cross validation
for(i in 1:num_of_folds){
  print(paste('-------------- fold', i),)
  # split data into train, test, validation
  test_index <- which(folds == i, arr.ind= T)
  
  if(i == num_of_folds){
    val_index <- which(folds == 1, arr.ind = T)
  }else{
    val_index <- which(folds == i+1, arr.ind = T)
  }
  
  test_data <- rand_data[test_index,]
  val_data <- rand_data[val_index,]
  train_data <- rand_data[-c(test_index, val_index),]
  
  
  # select best decision tree model
  val_df <- data.frame()
  
  # for(j in 4:5){
  #   model = glm(blue_win ~ blue_ad_kad+blue_sup_kad+blue_mid_kad+blue_jungle_kad+blue_top_kad+blue_firstTower+blue_firstInhibitor+blue_firstBaron, family="binomial", data=train_data)
  #   # calibrate model using validation fold
  #   resultframe <- data.frame(truth=val_data$blue_win,
  #                             pred=predict(model, val_data, type='response'))
  #   rtab <- table(resultframe)
  #   accuracy <- sum(diag(rtab))/sum(rtab)
  #   val_df[j,1] = accuracy
  # }
  # best_depth <- which.max(val_df[,1])
  
  # select best model
  new_model <- glm(blue_win~blue_ad_kad+blue_sup_kad+blue_mid_kad+blue_jungle_kad+blue_top_kad+blue_firstTower+blue_firstInhibitor+blue_firstBaron, data=train_data, family="binomial")
  
  
  # output training, validation, testing performance
  train_res <- data.frame(truth=train_data$blue_win,
                          pred=predict(new_model, train_data, type='response'))
  val_res <- data.frame(truth=val_data$blue_win,
                        pred=predict(new_model, val_data, type='response'))
  test_res <- data.frame(truth=test_data$blue_win,
                         pred=predict(new_model, test_data, type='response'))
  
  train_tab <- table(train_res)
  val_tab <- table(val_res)
  test_tab <- table(test_res)
  
  train_performance <- round(sum(diag(train_tab))/sum(train_tab), 2)
  val_performance <- round(sum(diag(val_tab))/sum(val_tab), 2)
  test_performance <- round(sum(diag(test_tab))/sum(test_tab), 2)
  
  new_row <- data.frame(set=paste('fold',i,sep=''), training=train_performance, validation=val_performance, test=test_performance)
  perform_df <- rbind(perform_df, new_row)
}

# create average row of performance report
avg_row <- data.frame(set='ave.', training=round(mean(perform_df$training),2), validation=round(mean(perform_df$validation),2), test=round(mean(perform_df$test),2))
perform_df <- rbind(perform_df, avg_row)

# write to output file
write.csv(perform_df, 'results/logistic_regression_performance.csv', row.names=FALSE, quote=FALSE)


# predict using the test data with model
final_res <- data.frame(blue_win=predict(new_model, test_file, type='response'))
final_tab <- table(final_res$blue_win.1)
final_performance <- round(sum(diag(final_tab))/sum(final_tab), 2)

predict_df <- data.frame(ID=test_file$index, blue_win=final_res$blue_win.1, ans=test_ans)
predict_df$blue_win <- ifelse(predict_df$blue_win>=0.5, 1, 0)
write.csv(predict_df, 'results/predict.csv', row.names=FALSE, quote=FALSE)

score <- 0
for(i in c(1:length(predict_df$blue_win))){
  if(predict_df$blue_win[i]==predict_df$ans[i]){
    score = score + 1
  }
}

percentage <- score/length(predict_df$blue_win)*100
print(percentage)


#####################################################################################
################################### NAIVE BAYES #####################################
#####################################################################################
num_of_folds <- 5
matches_data <- read.xlsx('dataset/matches_na.xlsx', sheet = 1)
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

data <- matches
# split data into 7:3 train/test
new_data <- data[sample(nrow(data)),]
# train_file <- new_data[1:6257,]
# test_file <- new_data[6258:7821,]
train_file <- new_data
test_ans <- test_file$blue_win
test_file$blue_win <- NA

# randomly shuffle the input data
rand_data <- train_file[sample(nrow(train_file)),]

# create folds according to user input parameter
folds <- cut(seq(1, nrow(rand_data)), breaks = num_of_folds, labels = F)

# create empty report data frame
perform_df <- data.frame(set=0, training=0, validation=0, test=0)[-1,]

# Perform k-fold cross validation
for(i in 1:num_of_folds){
  print(paste('-------------- fold', i),)
  # split data into train, test, validation
  test_index <- which(folds == i, arr.ind= T)
  
  if(i == num_of_folds){
    val_index <- which(folds == 1, arr.ind = T)
  }else{
    val_index <- which(folds == i+1, arr.ind = T)
  }
  
  test_data <- rand_data[test_index,]
  val_data <- rand_data[val_index,]
  train_data <- rand_data[-c(test_index, val_index),]
  
  
  # select best decision tree model
  val_df <- data.frame()
  
  # for(j in 4:10){
  #   model <- naiveBayes(blue_win ~ blue_ad_kad+blue_sup_kad+blue_mid_kad+blue_jungle_kad+blue_top_kad+blue_firstTower+blue_firstInhibitor+blue_firstBaron,
  #                  data=train_data)
  #   # calibrate model using validation fold
  #   resultframe <- data.frame(truth=val_data$blue_win,
  #                             pred=predict(model, val_data))
  #   rtab <- table(resultframe)
  #   accuracy <- sum(diag(rtab))/sum(rtab)
  #   val_df[j,1] = accuracy
  # }
  # best_depth <- which.max(val_df[,1])
  
  # select best model
  new_model <- naiveBayes(as.factor(blue_win)~blue_ad_kad+blue_sup_kad+blue_mid_kad+blue_jungle_kad+blue_top_kad+blue_firstTower+blue_firstInhibitor+blue_firstBaron,
                          data=train_data)
  
  summary(new_model)
  # output training, validation, testing performance
  train_res <- data.frame(truth=train_data$blue_win,
                          pred=predict(new_model, train_data))
  val_res <- data.frame(truth=val_data$blue_win,
                        pred=predict(new_model, val_data))
  test_res <- data.frame(truth=test_data$blue_win,
                         pred=predict(new_model, test_data))
  
  train_tab <- table(train_res)
  val_tab <- table(val_res)
  test_tab <- table(test_res)
  
  train_performance <- round(sum(diag(train_tab))/sum(train_tab), 2)
  val_performance <- round(sum(diag(val_tab))/sum(val_tab), 2)
  test_performance <- round(sum(diag(test_tab))/sum(test_tab), 2)
  
  new_row <- data.frame(set=paste('fold',i,sep=''), training=train_performance, validation=val_performance, test=test_performance)
  perform_df <- rbind(perform_df, new_row)
}

# create average row of performance report
avg_row <- data.frame(set='ave.', training=round(mean(perform_df$training),2), validation=round(mean(perform_df$validation),2), test=round(mean(perform_df$test),2))
perform_df <- rbind(perform_df, avg_row)

# write to output file
write.csv(perform_df, 'results/naiveBayes_performance.csv', row.names=FALSE, quote=FALSE)


# predict using the test data with model
final_res <- data.frame(blue_win=predict(new_model, test_file))
final_tab <- table(final_res$blue_win.1)
final_performance <- round(sum(diag(final_tab))/sum(final_tab), 2)

predict_df <- data.frame(ID=test_file$index, blue_win=final_res$blue_win.1, ans=test_ans)
predict_df$blue_win <- ifelse(predict_df$blue_win>=0.5, 1, 0)
write.csv(predict_df, 'results/predict.csv', row.names=FALSE, quote=FALSE)
