require(ggplot2)
require(adabag)
require(caret)
require(snow)
require(e1071)

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

leaveoneoutCI<- function(data_set, predict_pointer ){
  cl <- makeCluster(16, type = "SOCK", outfile="debug_leaveoneout_svm.txt") 
  clusterEvalQ(cl, {library(caret); library(e1071)})  
  
  data_set[,predict_pointer]<-as.factor(data_set[,predict_pointer])
  svm_leaveoneout<-vector(mode = "list", length = nrow(data_set))
  col_name <- colnames(data_set)[predict_pointer]
  

  print("calling svm")
  svm_leaveoneout<-parSapply(cl, 1:nrow(data_set), svm_training, data_set=data_set, predict_pointer=predict_pointer, col_name=col_name)
  mean_svm<-sum(unlist(svm_leaveoneout))/nrow(data_set)
  sd_svm<-sqrt((mean_svm*(1-mean_svm))/nrow(data_set))


  
  
  results<-c("mean_svm"=mean_svm, "sd_svm"=sd_svm)
  return(results)
}

args <- commandArgs(TRUE)
data_file<-args[1]
predict_pointer<-as.numeric(args[2])
data_set<-read.csv(args[1])

leaveoneout_output<-leaveoneoutCI(data_set,predict_pointer)
write.csv(leaveoneout_output, file=file.path(args[3],"svm_leaveoneout.csv"))