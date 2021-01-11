library('rpart')
library('pROC')
library('randomForest')
library('e1071')
library('ROCR')

rm(list = ls())

getauc_acc <- function(model, target_set){
  aucframe <- data.frame(truth=target_set$blue_win,
                            pred=predict(model, newdata=target_set))
  auc_score <- auc(aucframe$truth, aucframe$pred)
  
  accframe <- data.frame(truth=target_set$blue_win,
                         pred=ifelse(predict(model, newdata=target_set) > 0.5, 1, 0))
  accuracy <- sum(diag(table(accframe)))/sum(table(accframe))
  return (c(auc_score, accuracy))
}

getmatrix <- function(model, target_set){
  accframe <- table(data.frame(truth=target_set$blue_win,
                         pred=ifelse(predict(model, newdata=target_set) > 0.5, 1, 0)))
  
  return (accframe)
}

getmatrix1 <- function(model, target_set){
  accframe <- table(data.frame(truth=target_set$blue_win,
                               pred=predict(model, newdata=target_set)))
  
  return (accframe)
}

getauc_acc1 <- function(model, target_set){
  
  aucframe <- data.frame(truth=target_set$blue_win,
                         pred=predict(model, newdata=target_set, type='class'))
  pred_ROCR <- prediction(as.numeric(aucframe$pred), as.numeric(aucframe$truth))
  auc_ROCR <- performance(pred_ROCR, measure = "auc")
  auc_score <- auc_ROCR@y.values[[1]]
  
  accframe <- data.frame(truth=target_set$blue_win,
                         pred=predict(model, newdata=target_set, type='class'))
  accuracy <- sum(diag(table(accframe)))/sum(table(accframe))
  return (c(auc_score, accuracy))
}

# read input data
k <- 5
origin.data <- read.csv("dataset/matches_na.csv", header = T)
predict <- "predict.csv"
report <- "report.csv"

# data preprocessing
data <- origin.data[,-c(1:5)]
data <- cbind(data,
                 blue_ad_kad = ((data$blue_ad_kills+data$blue_ad_assists)/data$blue_ad_deaths),
                 blue_sup_kad = ((data$blue_sup_kills+data$blue_sup_assists)/data$blue_sup_deaths),
                 blue_mid_kad = ((data$blue_mid_kills+data$blue_mid_assists)/data$blue_mid_deaths),
                 blue_jungle_kad = ((data$blue_jungle_kills+data$blue_jungle_assists)/data$blue_jungle_deaths),
                 blue_top_kad = ((data$blue_top_kills+data$blue_top_assists)/data$blue_top_deaths),
                 purple_ad_kad = ((data$purple_ad_kills+data$purple_ad_assists)/data$purple_ad_deaths),
                 purple_sup_kad = ((data$purple_sup_kills+data$purple_sup_assists)/data$purple_sup_deaths),
                 purple_mid_kad = ((data$purple_mid_kills+data$purple_mid_assists)/data$purple_mid_deaths),
                 purple_jungle_kad = ((data$purple_jungle_kills+data$purple_jungle_assists)/data$purple_jungle_deaths),
                 purple_top_kad = ((data$purple_top_kills+data$purple_top_assists)/data$purple_top_deaths))

data <- cbind(data,
                blue_ad_kad_ln = log(data$blue_ad_kad+1),
                blue_sup_kad_ln = log(data$blue_sup_kad+1),
                blue_mid_kad_ln = log(data$blue_mid_kad+1),
                blue_jungle_kad_ln = log(data$blue_jungle_kad+1),
                blue_top_kad_ln = log(data$blue_top_kad+1),
                purple_ad_kad_ln = log(data$purple_ad_kad+1),
                purple_sup_kad_ln = log(data$purple_sup_kad+1),
                purple_mid_kad_ln = log(data$purple_mid_kad+1),
                purple_jungle_kad_ln = log(data$purple_jungle_kad+1),
                purple_top_kad_ln = log(data$purple_top_kad+1))



#shuffle the data
data <- data[sample(nrow(data)),]

#create folds
folds <- cut(seq(1,nrow(data)), breaks = k, labels = F)

output_df <- data.frame()
model_df <- data.frame()

#rf
for(i in 1:k){
  print(paste("start modeling fold", i, ".."))
  fold.name <- paste("fold",i)
  
  test.index <- which(folds == i, arr.ind= T)
  if(i == k){
    valid.index <- which(folds == 1, arr.ind = T)
  }else{
    valid.index <- which(folds == i+1, arr.ind = T)
  }
  test.data <- data[test.index,]
  valid.data <- data[valid.index,]
  train.data <- data[-c(test.index, valid.index),]
  
  depth_df <- data.frame()
  for (ntree in 1:5){
    for (maxnode in 1:5){
      model <- randomForest(blue_win ~ blue_firstBaron+blue_firstTower+blue_firstInhibitor+blue_ad_kad+blue_sup_kad+blue_mid_kad+blue_jungle_kad+blue_top_kad,
                            data=train.data, 
                            control=randomForst.control(ntree=ntree, maxnode=maxnode))
      #calculate the AUC in order to choose the right model
      train.auc <- getauc_acc(model, train.data)[1]
      train.acc <- getauc_acc(model, train.data)[2]
      valid.auc <- getauc_acc(model, valid.data)[1]
      valid.acc <- getauc_acc(model, valid.data)[2]
      
      #write the result to depth_df
      newdepth_df <- data.frame(ntree=ntree,
                                maxnode=maxnode,
                                train_auc=train.auc,
                                train_acc=train.acc,
                                valid_auc=valid.auc,
                                valid_acc=valid.acc)
      depth_df <- rbind(depth_df, newdepth_df)
    }
  }
  
  best_para <- which.max(depth_df$valid_auc)
  best_tree <- depth_df$ntree[best_para]
  best_node <- depth_df$maxnode[best_para]
  
  training_result <- c(depth_df$train_auc[best_para], depth_df$train_auc[best_para])
  validation_result <- c(max(depth_df$valid_auc), max(depth_df$valid_acc))
  
  
  model <- randomForest(blue_win ~ blue_firstBaron+blue_firstTower+blue_firstInhibitor+blue_ad_kad+blue_sup_kad+blue_mid_kad+blue_jungle_kad+blue_top_kad,
                        data=train.data, 
                        control=randomForst.control(ntree=best_tree, maxnode=best_node))
  
  test.auc <- getauc_acc(model, test.data)[1]
  test.acc <- getauc_acc(model, test.data)[2]
  test_result <- c(test.auc, test.acc)
  
  test.matrix <- getmatrix(model, test.data)
  test.matrix <- data.frame(test.matrix)
  TP <- test.matrix$Freq[1] 
  FP <- test.matrix$Freq[2]
  TN <- test.matrix$Freq[4]
  FN <- test.matrix$Freq[3]
  sensitivity <- TP/(TP+FN)
  specificity <- TN/(TN+FP)
  precision <- TP/(TP+FP)
  recall <- TP/(TP+FN)
  F1 = 2*precision*recall/(precision+recall)
  
  row <- data.frame(set=fold.name,
                    training_auc=round(training_result[1],4),
                    training_acc=round(training_result[2],4),
                    validation_auc=round(validation_result[1],4),
                    validation_acc=round(validation_result[2],4),
                    test_auc=round(test_result[1],4),
                    test_acc=round(test_result[2],4),
                    sensitivity=round(sensitivity,4),
                    specificity=round(specificity,4),
                    precision=round(precision,4),
                    recall=round(recall,4),
                    F1=round(F1,4))
  output_df <- rbind(output_df, row)
  
  print(paste("fold",i,"done"))  
}

# SVM
for(i in 1:k){
  print(paste("start modeling fold", i, ".."))
  fold.name <- paste("fold",i)
  
  test.index <- which(folds == i, arr.ind= T)
  if(i == k){
    valid.index <- which(folds == 1, arr.ind = T)
  }else{
    valid.index <- which(folds == i+1, arr.ind = T)
  }
  test.data <- data[test.index,]
  valid.data <- data[valid.index,]
  train.data <- data[-c(test.index, valid.index),]
  
  depth_df <- data.frame()
  for (cost in 10^(-1:2)){
    for (gamma in 0.5:2){
      model <- svm(blue_win ~ blue_firstBaron+blue_firstTower+blue_firstInhibitor+blue_ad_kad+blue_sup_kad+blue_mid_kad+blue_jungle_kad+blue_top_kad,
                   data=train.data, 
                   cost=cost, gamma=gamma)
      #calculate the AUC in order to choose the right model
      train.auc <- getauc_acc(model, train.data)[1]
      train.acc <- getauc_acc(model, train.data)[2]
      valid.auc <- getauc_acc(model, valid.data)[1]
      valid.acc <- getauc_acc(model, valid.data)[2]
      
      #write the result to depth_df
      newdepth_df <- data.frame(cost=cost,
                                gamma=gamma,
                                train_auc=train.auc,
                                train_acc=train.acc,
                                valid_auc=valid.auc,
                                valid_acc=valid.acc)
      depth_df <- rbind(depth_df, newdepth_df)
      train.matrix <- getmatrix(model, train.data)
      valid.matrix <- getmatrix(model, valid.data)
    }
  }
  
  best_para <- which.max(depth_df$valid_auc)
  best_cost <- depth_df$cost[best_para]
  best_gamma <- depth_df$gamma[best_para]
  
  training_result <- c(round(depth_df$train_auc[best_para], 2), round(depth_df$train_auc[best_para], 2))
  validation_result <- c(round(max(depth_df$valid_auc), 2), round(max(depth_df$valid_acc), 2))
  
  
  model <- svm(blue_win ~ blue_firstBaron+blue_firstTower+blue_firstInhibitor+blue_ad_kad+blue_sup_kad+blue_mid_kad+blue_jungle_kad+blue_top_kad,
               data=train.data, 
               cost=best_cost, gamma=best_gamma)  
  test.auc <- getauc_acc(model, test.data)[1]
  test.acc <- getauc_acc(model, test.data)[2]
  test_result <- c(round(test.auc, 2), round(test.acc), 2)
  
  test.matrix <- getmatrix(model, test.data)
  test.matrix <- data.frame(test.matrix)
  TP <- test.matrix$Freq[1] 
  FP <- test.matrix$Freq[2]
  TN <- test.matrix$Freq[4]
  FN <- test.matrix$Freq[3]
  sensitivity <- TP/(TP+FN)
  specificity <- TN/(TN+FP)
  precision <- TP/(TP+FP)
  recall <- TP/(TP+FN)
  F1 = 2*precision*recall/(precision+recall)
  
  row <- data.frame(set=fold.name,
                    training_auc=round(training_result[1],4),
                    training_acc=round(training_result[2],4),
                    validation_auc=round(validation_result[1],4),
                    validation_acc=round(validation_result[2],4),
                    test_auc=round(test_result[1],4),
                    test_acc=round(test_result[2],4),
                    sensitivity=round(sensitivity,4),
                    specificity=round(specificity,4),
                    precision=round(precision,4),
                    recall=round(recall,4),
                    F1=round(F1,4))
  output_df <- rbind(output_df, row)
  
  print(paste("fold",i,"done"))  
}

# rpart
for(i in 1:k){
  print(paste("start modeling fold", i, ".."))
  fold.name <- paste("fold",i)
  
  test.index <- which(folds == i, arr.ind= T)
  if(i == k){
    valid.index <- which(folds == 1, arr.ind = T)
  }else{
    valid.index <- which(folds == i+1, arr.ind = T)
  }
  test.data <- data[test.index,]
  valid.data <- data[valid.index,]
  train.data <- data[-c(test.index, valid.index),]
  
  depth_df <- data.frame()
  for (j in 4:10){
    model <- rpart(blue_win ~ blue_ad_kad+blue_sup_kad+blue_mid_kad+blue_jungle_kad+blue_top_kad+blue_firstTower+blue_firstInhibitor+blue_firstBaron,
                   data=train.data, 
                   control=rpart.control(maxdepth=j, minsplit=2),
                   method = 'class')
    #calculate the AUC in order to choose the right model
    train.auc <- getauc_acc1(model, train.data)[1]
    train.acc <- getauc_acc1(model, train.data)[2]
    valid.auc <- getauc_acc1(model, valid.data)[1]
    valid.acc <- getauc_acc1(model, valid.data)[2]
    
    #write the result to depth_df
    newdepth_df <- data.frame(maxdepth=j,
                              train_auc=train.auc,
                              train_acc=train.acc,
                              valid_auc=valid.auc,
                              valid_acc=valid.acc)
    depth_df <- rbind(depth_df, newdepth_df)
  }
  
  best_para <- which.max(depth_df$valid_auc)
  best_depth <- depth_df$maxdepth[best_para]
  
  training_result <- c(round(depth_df$train_auc[best_para], 2), round(depth_df$train_auc[best_para], 2))
  validation_result <- c(round(max(depth_df$valid_auc), 2), round(max(depth_df$valid_acc), 2))
  
  
  model <- rpart(blue_win ~ blue_ad_kad+blue_sup_kad+blue_mid_kad+blue_jungle_kad+blue_top_kad+blue_firstTower+blue_firstInhibitor+blue_firstBaron,
                 data=train.data, 
                 control=rpart.control(maxdepth=best_depth, minsplit=2),
                 method = 'class') 
  test.auc <- getauc_acc1(model, test.data)[1]
  test.acc <- getauc_acc1(model, test.data)[2]
  test_result <- c(round(test.auc, 2), round(test.acc), 2)
  
  test.matrix <- getmatrix(model, test.data)
  test.matrix <- data.frame(test.matrix)
  TP <- test.matrix$Freq[6] 
  FP <- test.matrix$Freq[5]
  TN <- test.matrix$Freq[3]
  FN <- test.matrix$Freq[4]
  sensitivity <- TP/(TP+FN)
  specificity <- TN/(TN+FP)
  precision <- TP/(TP+FP)
  recall <- TP/(TP+FN)
  F1 = 2*precision*recall/(precision+recall)
  
  row <- data.frame(set=fold.name,
                    training_auc=round(training_result[1],4),
                    training_acc=round(training_result[2],4),
                    validation_auc=round(validation_result[1],4),
                    validation_acc=round(validation_result[2],4),
                    test_auc=round(test_result[1],4),
                    test_acc=round(test_result[2],4),
                    sensitivity=round(sensitivity,4),
                    specificity=round(specificity,4),
                    precision=round(precision,4),
                    recall=round(recall,4),
                    F1=round(F1,4))
  output_df <- rbind(output_df, row)
  
  print(paste("fold",i,"done"))  
}

# naive bayes
for(i in 1:k){
  print(paste("start modeling fold", i, ".."))
  fold.name <- paste("fold",i)
  
  test.index <- which(folds == i, arr.ind= T)
  if(i == k){
    valid.index <- which(folds == 1, arr.ind = T)
  }else{
    valid.index <- which(folds == i+1, arr.ind = T)
  }
  test.data <- data[test.index,]
  valid.data <- data[valid.index,]
  train.data <- data[-c(test.index, valid.index),]
  
  depth_df <- data.frame()
  
  model <- naiveBayes(as.factor(blue_win)~blue_ad_kad+blue_sup_kad+blue_mid_kad+blue_jungle_kad+blue_top_kad+blue_firstTower+blue_firstInhibitor+blue_firstBaron,
                        data=train.data)
 
  #calculate the AUC in order to choose the right model
  train.auc <- getauc_acc1(model, train.data)[1]
  train.acc <- getauc_acc1(model, train.data)[2]
  valid.auc <- getauc_acc1(model, valid.data)[1]
  valid.acc <- getauc_acc1(model, valid.data)[2]
    
  #write the result to depth_df
  newdepth_df <- data.frame(train_auc=train.auc,
                            train_acc=train.acc,
                            valid_auc=valid.auc,
                            valid_acc=valid.acc)
  depth_df <- rbind(depth_df, newdepth_df)
  
  training_result <- c(round(depth_df$train_auc, 2), round(depth_df$train_auc, 2))
  validation_result <- c(round(max(depth_df$valid_auc), 2), round(max(depth_df$valid_acc), 2))
 
  test.auc <- getauc_acc1(model, test.data)[1]
  test.acc <- getauc_acc1(model, test.data)[2]
  test_result <- c(round(test.auc, 2), round(test.acc), 2)
  
  test.matrix <- getmatrix1(model, test.data)
  test.matrix <- data.frame(test.matrix)
  TP <- test.matrix$Freq[1] 
  FP <- test.matrix$Freq[2]
  TN <- test.matrix$Freq[4]
  FN <- test.matrix$Freq[3]
  sensitivity <- TP/(TP+FN)
  specificity <- TN/(TN+FP)
  precision <- TP/(TP+FP)
  recall <- TP/(TP+FN)
  F1 = 2*precision*recall/(precision+recall)
  
  row <- data.frame(set=fold.name,
                    training_auc=round(training_result[1],4),
                    training_acc=round(training_result[2],4),
                    validation_auc=round(validation_result[1],4),
                    validation_acc=round(validation_result[2],4),
                    test_auc=round(test_result[1],4),
                    test_acc=round(test_result[2],4),
                    sensitivity=round(sensitivity,4),
                    specificity=round(specificity,4),
                    precision=round(precision,4),
                    recall=round(recall,4),
                    F1=round(F1,4))
  output_df <- rbind(output_df, row)
  
  print(paste("fold",i,"done"))  
}

ave <- data.frame(set='ave.',
                  training_auc=round(mean(as.numeric(output_df$training_auc)), 2),
                  training_acc=round(mean(as.numeric(output_df$training_acc)), 2),
                  validation_auc=round(mean(as.numeric(output_df$validation_auc)), 2),
                  validation_acc=round(mean(as.numeric(output_df$validation_acc)), 2),
                  test_auc=round(mean(as.numeric(output_df$test_auc)), 2),
                  test_acc=round(mean(as.numeric(output_df$test_acc)), 2),
                  sensitivity=round(mean(output_df$sensitivity), 2),
                  specificity=round(mean(output_df$specificity), 2),
                  precision=round(mean(as.numeric(output_df$precision)), 2),
                  recall=round(mean(output_df$recall), 2),
                  F1=round(mean(as.numeric(output_df$F1)), 2)
)
output_df <- rbind(output_df, ave)
output_df <- format.data.frame(output_df, 2)
write.table(output_df, 'rpart_5.csv', row.names=FALSE, quote=FALSE, sep = ',')
print("done!!!")
