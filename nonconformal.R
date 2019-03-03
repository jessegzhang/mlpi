require(ggplot2)
require(adabag)
require(caret)
require(C50)
require(e1071)
require(gower)

nonconformalCI<- function(data_set, predict_pointer ){
  
  #setting factor 
  data_set[,predict_pointer]<-as.factor(data_set[,predict_pointer])
  
  #creating data partitions
  data_splits <- createDataPartition(y = data_set[,predict_pointer], p = 0.70,list = FALSE)
  training_set <- data_set[data_splits,]
  #determining training set and calibration set
  data_splits_training<-createDataPartition(y = training_set[,predict_pointer], p = 0.80,list = FALSE)
  calibration_set <- training_set[-data_splits_training,]
  training_set <- training_set[data_splits_training,]
  
  testing_set <- data_set[-data_splits,]
  calib_size<-nrow(calibration_set)
  
  #setting up formula
  #setting up initial variables for train
  col_name <- colnames(data_set)[predict_pointer]
  formula <- as.formula(paste(col_name, ' ~ .' ))
  tr_control<- trainControl(method="none") 
  
  adabag_fit<-train(formula, data=training_set, method="AdaBag", trControl=tr_control,
                 preProcess=c("center","scale"))
  adabag_predict<-predict(adabag_fit,testing_set)
  
  svm_fit<-train(formula, data=training_set, method="svmLinear", trControl=tr_control,
                 preProcess=c("center","scale"))
  svm_predict<-predict(svm_fit,testing_set)
  
  cfifty_fit<-train(formula, data=training_set, method="C5.0",
                    preProcess=c("center","scale"))
  cfifty_predict<-predict(cfifty_fit,testing_set)
  
  #obtaining nonconformity based on the scores given
  nonconform_calib <- rep(NA, length = calib_size)
  for (i in 1:calib_size) {
    distance_list<-gower_dist(calibration_set[i,-predict_pointer],training_set[,-predict_pointer])
    
    #obtaining the nonconformity scores for each machine learning technique
    calib_label <- calibration_set[i,predict_pointer]
    min_same_label <-
      min(distance_list[which(training_set[, predict_pointer] == calib_label)])
    min_diff_label <-
      min(distance_list[which(training_set[, predict_pointer] != calib_label)])
    
    
    nonconform_calib[i] <- min_same_label/min_diff_label
  }
  
  alpha_scores<-data.frame(matrix(ncol=length(unique(data_set[,predict_pointer])),nrow=nrow(testing_set)))
  p_vals<-data.frame(matrix(ncol=length(unique(data_set[,predict_pointer])),nrow=nrow(testing_set)))
  colnames(p_vals)<- unique(data_set[,predict_pointer])
  colnames(alpha_scores)<- unique(data_set[,predict_pointer])
  for (i in 1:nrow(testing_set)) {
    #developing gower distance list for nonconformity scores
    distance_list<-gower_dist(testing_set[i,-predict_pointer],training_set[,-predict_pointer])
    for (j in 1:ncol(alpha_scores)) {
      calib_label <- colnames(alpha_scores)[j]
      min_same_label <-
        min(distance_list[which(training_set[, predict_pointer] == calib_label)])
      min_diff_label <-
        min(distance_list[which(training_set[, predict_pointer] != calib_label)])
      alpha_scores[i,j] <- min_same_label/min_diff_label
    }
  }
  #in some we weight the example itself but in this case we will default to 1
  theta<-1
  #obtaining p values by comparing to the calibration set
  for(i in 1:nrow(testing_set)) {
    count_vals<-lapply(alpha_scores[i,], function(x) sum(nonconform_calib>=x, na.rm = TRUE))
    
    p_vals[i,]<-lapply(count_vals, function(x) (x+theta)/(calib_size+1))
  }
  
  scores_svm<-rep(NA,nrow(testing_set))
  scores_adabag<-rep(NA,nrow(testing_set))
  scores_cfifty<-rep(NA,nrow(testing_set))
  
  for(i in 1:nrow(testing_set)) {
    if(cfifty_predict[i]==testing_set[i,predict_pointer]){
      scores_cfifty[i]<-1-max(p_vals[i,which(colnames(p_vals)!=cfifty_predict[i])])
    } else {
      scores_cfifty[i]<-max(p_vals[i,which(colnames(p_vals)!=cfifty_predict[i])])
    }
    
    if(adabag_predict[i]==testing_set[i,predict_pointer]){
      scores_adabag[i]<-1-max(p_vals[i,which(colnames(p_vals)!=adabag_predict[i])])
    } else {
      scores_adabag[i]<-max(p_vals[i,which(colnames(p_vals)!=adabag_predict[i])])
    }
    
    if(svm_predict[i]==testing_set[i,predict_pointer]){
      scores_svm[i]<-1-max(p_vals[i,which(colnames(p_vals)!=svm_predict[i])])
    } else {
      scores_svm[i]<-max(p_vals[i,which(colnames(p_vals)!=svm_predict[i])])
    }
    
  }
  mean_adabag<-mean(scores_adabag)
  mean_cfifty<-mean(scores_cfifty)
  mean_svm<-mean(scores_svm)
  sd_adabag<-sd(scores_adabag)
  sd_cfifty<-sd(scores_cfifty)
  sd_svm<-sd(scores_svm)
  conform_scores<-data.frame("scores_adabag"=scores_adabag,"scores_svm"=scores_svm, "scores_cfifty"=scores_cfifty)
  return(conform_scores)
}


  args <- commandArgs(TRUE)
  data_file<-args[1]
  predict_pointer<-as.numeric(args[2])
  data_set<-read.csv(args[1])

  nonconformal_output<-nonconformalCI(data_set,predict_pointer)
  write.csv(nonconformal_output, file.path(".", args[3],"nonconformal.csv"))

