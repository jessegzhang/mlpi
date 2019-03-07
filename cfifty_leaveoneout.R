require(ggplot2)
require(adabag)
require(caret)
require(C50)
require(snow)
require(e1071)


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
  return(cfifty_predict==testing_set[,predict_pointer])
}

leaveoneoutCI<- function(data_set, predict_pointer ){

  cl <- makeCluster(16, type = "SOCK", outfile="debug_leaveoneout_cfifty.txt") 
  clusterEvalQ(cl, {library(caret); library(C50); library(e1071)})  

  data_set[,predict_pointer]<-as.factor(data_set[,predict_pointer])
  
  cfifty_leaveoneout<-vector(mode = "list", length = nrow(data_set))
  col_name <- colnames(data_set)[predict_pointer]
  
  print("calling cfifty")
  cfifty_leaveoneout<-parSapply(cl, 1:nrow(data_set), cfifty_training, data_set=data_set, predict_pointer=predict_pointer, col_name=col_name)
  mean_cfifty<-sum(unlist(cfifty_leaveoneout))/nrow(data_set)
  sd_cfifty<-sqrt((mean_cfifty*(1-mean_cfifty))/nrow(data_set))
  
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
  
  
  
  
  
  results<-c("mean_cfifty"=mean_cfifty, "sd_cfifty"=sd_cfifty)
  return(results)
}

args <- commandArgs(TRUE)
data_file<-args[1]
predict_pointer<-as.numeric(args[2])
data_set<-read.csv(args[1])

leaveoneout_output<-leaveoneoutCI(data_set,predict_pointer)
write.csv(leaveoneout_output, file=file.path(".", args[3],"cfifty_leaveoneout.csv"))
