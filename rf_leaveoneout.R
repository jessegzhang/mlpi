require(ggplot2)
require(caret)
require(randomForest)
require(snow)
require(e1071)

rf_training <- function(removal_point,data_set, predict_pointer, col_name ){
  #doing leave one out
  training_set<-data_set[-removal_point,]
  testing_set<-data_set[removal_point,]
  
  #training on the remaining set
  formula <- as.formula(paste(col_name, ' ~ .' ))
  tr_control<- trainControl(method="none") 
  rf_fit<-train(formula, data=training_set, method="rf", trControl=tr_control,
                    preProcess=c("center","scale"))
  rf_predict<-predict(rf_fit,testing_set)
  return(predict(rf_fit,testing_set)==testing_set[,predict_pointer])
  
}

leaveoneoutCI<- function(data_set, predict_pointer ){
  cl <- makeCluster(16, type = "SOCK", outfile="debug_leaveoneout_rf.txt") 
  clusterEvalQ(cl, {library(caret); library(randomForest); library(e1071)})  
  
  data_set[,predict_pointer]<-as.factor(data_set[,predict_pointer])
  
  rf_leaveoneout<-vector(mode = "list", length = nrow(data_set))
  col_name <- colnames(data_set)[predict_pointer]
  
  print("calling rf")
  rf_leaveoneout<-parSapply(cl, 1:nrow(data_set), rf_training, data_set=data_set, predict_pointer=predict_pointer, col_name=col_name)
  mean_rf<-sum(unlist(rf_leaveoneout))/nrow(data_set)
  sd_rf<-sqrt((mean_rf*(1-mean_rf))/nrow(data_set))

  
  
  results<-c("mean_rf"=mean_rf, "sd_rf"=sd_rf)
  return(results)
}

args <- commandArgs(TRUE)
data_file<-args[1]
predict_pointer<-as.numeric(args[2])
data_set<-read.csv(args[1])

leaveoneout_output<-leaveoneoutCI(data_set,predict_pointer)
write.csv(leaveoneout_output, file=file.path(args[3],"ada_bagleaveoneout.csv"))
