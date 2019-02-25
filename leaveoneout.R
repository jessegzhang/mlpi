require(caret)
require(C50)
require(snow)
require(e1071)

knn_training <- function(removal_point,data_set, predict_pointer, col_name ){
  #doing leave one out
  training_set<-data_set[-removal_point,]
  testing_set<-data_set[removal_point,]
  
  #training on the remaining set
  formula <- as.formula(paste(col_name, ' ~ .' ))
  tr_control<- trainControl(method="none") 
  knn_fit<-train(formula, data=training_set, method="knn", trControl=tr_control,
                 preProcess=c("center","scale"))
  knn_predict<-predict(knn_fit,testing_set)
  return(predict(knn_fit,testing_set)==testing_set[,predict_pointer])
  
}

svm_training <- function(removal_point,data_set, predict_pointer, col_name ){
  #doing leave one out
  training_set<-data_set[-removal_point,]
  testing_set<-data_set[removal_point,]
  
  #training and predicting
  formula <- as.formula(paste(col_name, ' ~ .' ))
  tr_control<- trainControl(method="none") 
  svm_fit<-train(formula, data=training_set, method="svmLinear", trControl=tr_control,
                 preProcess=c("center","scale"))
  svm_predict<-predict(svm_fit,testing_set)
  return(predict(svm_fit,testing_set)==testing_set[,predict_pointer])
}

cfifty_training <- function(removal_point,data_set, predict_pointer, col_name ){
  #doing leave one out
  training_set<-data_set[-removal_point,]
  testing_set<-data_set[removal_point,]
  
  #training and predicting
  formula <- as.formula(paste(col_name, ' ~ .' ))
  tr_control<- trainControl(method="none") 
  cfifty_fit<-train(formula, data=training_set, method="C5.0",
                    preProcess=c("center","scale"))
  cfifty_predict<-predict(cfifty_fit,testing_set)
  return(predict(cfifty_fit,testing_set)==testing_set[,predict_pointer])
}

leaveoneoutCI<- function(data_set, predict_pointer ){
  cl <- makeCluster(16, type = "SOCK", outfile="debug_leaveoneout.txt") 
  clusterEvalQ(cl, {library(caret); library(C50); library(e1071)})  
  set.seed(322)
  
  data_set[,predict_pointer]<-as.factor(data_set[,predict_pointer])
  
  knn_guess<-vector(mode = "list", length = nrow(data_set))
  cfifty_guess<-vector(mode = "list", length = nrow(data_set))
  svm_guess<-vector(mode = "list", length = nrow(data_set))
  col_name <- colnames(data_set)[predict_pointer]
  
  knn_leaveoneout<-parSapply(cl, 1:nrow(data_set), knn_training, data_set=data_set, predict_pointer=predict_pointer, col_name=col_name)
  svm_leaveoneout<-parSapply(cl, 1:nrow(data_set), svm_training, data_set=data_set, predict_pointer=predict_pointer, col_name=col_name)
  cfifty_leaveoneout<-parSapply(cl, 1:nrow(data_set), cfifty_training, data_set=data_set, predict_pointer=predict_pointer, col_name=col_name)
  
  
  # #leave one out cross validation
  # for(i in 1:nrow(data_set)) {
  #   train_set<-data_set[-i,]
  #   test_set<-data_set[i,]
  #   
  #   knn_fit<-train(formula, data=train_set, method="knn", trControl=tr_control,
  #                  preProcess=c("center","scale"))
  #   knn_guess[i]<-(predict(knn_fit,test_set)==test_set[,predict_pointer])
  #   
  #   svm_fit<-train(formula, data=train_set, method="svmLinear", trControl=tr_control,
  #                  preProcess=c("center","scale"))
  #   svm_guess[i]<-(predict(svm_fit,test_set)==test_set[,predict_pointer])
  #   
  #   
  #   cfifty_fit<-train(formula, data=train_set, method="C5.0",
  #                     preProcess=c("center","scale"))
  #   cfifty_guess[i]<-(predict(cfifty_fit,test_set)==test_set[,predict_pointer])
  #   
  # } 
  
  mean_knn<-sum(unlist(knn_leaveoneout))/nrow(data_set)
  mean_cfifty<-sum(unlist(cfifty_leaveoneout))/nrow(data_set)
  mean_svm<-sum(unlist(svm_leaveoneout))/nrow(data_set)
  sd_knn<-sqrt((mean_knn*(1-mean_knn))/nrow(data_set))
  sd_cfifty<-sqrt((mean_cfifty*(1-mean_cfifty))/nrow(data_set))
  sd_svm<-sqrt((mean_svm*(1-mean_svm))/nrow(data_set))
  
  results<-c("mean_knn"=mean_knn, "mean_cfifty"=mean_cfifty, "mean_svm"=mean_svm, "sd_knn"=sd_knn, "sd_cfifty"=sd_cfifty, "sd_svm"=sd_svm)
  return(results)
}

data("iris")
leaveoneout_output<-leaveoneoutCI(iris,5)
write.csv(leaveoneout_output, file="leaveoneout.csv")
