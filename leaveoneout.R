require(ggplot2)
require(adabag)
require(caret)
require(C50)
require(snow)
require(e1071)

adabag_training <- function(removal_point,data_set, predict_pointer, col_name ){
  #doing leave one out
  print(removal_point)
  print(data_set)
  training_set<-data_set[-removal_point,]
  testing_set<-data_set[removal_point,]
  
  #training on the remaining set
  formula <- as.formula(paste(col_name, ' ~ .' ))
  tr_control<- trainControl(method="none") 
  adabag_fit<-train(formula, data=training_set, method="AdaBag", trControl=tr_control,
                 preProcess=c("center","scale"))
  adabag_predict<-predict(adabag_fit,testing_set)
  return(predict(adabag_fit,testing_set)==testing_set[,predict_pointer])
  
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
  
  data_set[,predict_pointer]<-as.factor(data_set[,predict_pointer])
  
  adabag_guess<-vector(mode = "list", length = nrow(data_set))
  cfifty_guess<-vector(mode = "list", length = nrow(data_set))
  svm_guess<-vector(mode = "list", length = nrow(data_set))
  col_name <- colnames(data_set)[predict_pointer]
  
  print("calling adabag")
  adabag_leaveoneout<-parSapply(cl, 1:nrow(data_set), adabag_training, data_set=data_set, predict_pointer=predict_pointer, col_name=col_name)
  mean_adabag<-sum(unlist(adabag_leaveoneout))/nrow(data_set)
  sd_adabag<-sqrt((mean_adabag*(1-mean_adabag))/nrow(data_set))
  rm(adabag_leaveoneout)
  gc()
  print("calling svm")
  
  svm_leaveoneout<-parSapply(cl, 1:nrow(data_set), svm_training, data_set=data_set, predict_pointer=predict_pointer, col_name=col_name)
  mean_cfifty<-sum(unlist(svm_leaveoneout))/nrow(data_set)
  sd_cfifty<-sqrt((mean_cfifty*(1-mean_cfifty))/nrow(data_set))
  rm(svm_leaveoneout)
  gc()
  
  print("calling cfifty")
  cfifty_leaveoneout<-parSapply(cl, 1:nrow(data_set), cfifty_training, data_set=data_set, predict_pointer=predict_pointer, col_name=col_name)
  mean_svm<-sum(unlist(svm_leaveoneout))/nrow(data_set)
  sd_svm<-sqrt((mean_svm*(1-mean_svm))/nrow(data_set))
  rm(cfifty_leaveoneout)
  gc()
  
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
  



  
  results<-c("mean_adabag"=mean_adabag, "mean_cfifty"=mean_cfifty, "mean_svm"=mean_svm, "sd_adabag"=sd_adabag, "sd_cfifty"=sd_cfifty, "sd_svm"=sd_svm)
  return(results)
}

 args <- commandArgs(TRUE)
 data_file<-args[1]
 predict_pointer<-as.numeric(args[2])
 data_set<-read.csv(args[1])
 
 leaveoneout_output<-leaveoneoutCI(data_set,predict_pointer)
 write.csv(leaveoneout_output, file=file.path(".", args[3],"leaveoneout.csv"))
