require(ggplot2)
require(adabag)
require(caret)
require(randomForest)
require(e1071)
require(gower)

nonconformalCI<- function(data_set, predict_pointer, filepath ){
  
  #setting factor 
  data_set[,predict_pointer]<-as.factor(data_set[,predict_pointer])
  
  #creating data partitions
  data_splits <- createDataPartition(y = data_set[,predict_pointer], p = 0.70,list = FALSE)
  training_set <- data_set[data_splits,]
  #determining training set and calibration set
  data_splits_training<-createDataPartition(y = training_set[,predict_pointer], p = 0.70,list = FALSE)
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
  
  rf_fit<-train(formula, data=training_set, method="rf",
                    preProcess=c("center","scale"))
  rf_predict<-predict(rf_fit,testing_set)
  
  #obtaining nonconformity based on the scores given
  nonconform_calib <- rep(NA, length = calib_size)
  for (i in 1:calib_size) {
    distance_list<-gower_dist(calibration_set[i,-predict_pointer],calibration_set[-i,-predict_pointer])
    #obtaining the nonconformity scores for each machine learning technique
    calib_label <- calibration_set[i,predict_pointer]
    min_same_label <-
      min(distance_list[which(calibration_set[-i, predict_pointer] == calib_label)])
    min_diff_label <-
      min(distance_list[which(calibration_set[-i, predict_pointer] != calib_label)])
    
    
    nonconform_calib[i] <- min_same_label/min_diff_label
  }
  
  alpha_scores<-data.frame(matrix(ncol=length(unique(data_set[,predict_pointer])),nrow=nrow(testing_set)))
  p_vals<-data.frame(matrix(ncol=length(unique(data_set[,predict_pointer])),nrow=nrow(testing_set)))
  colnames(p_vals)<- unique(data_set[,predict_pointer])
  colnames(alpha_scores)<- unique(data_set[,predict_pointer])
  for (i in 1:nrow(testing_set)) {
    #developing gower distance list for nonconformity scores
    distance_list<-gower_dist(testing_set[i,-predict_pointer],calibration_set[,-predict_pointer])
    for (j in 1:ncol(alpha_scores)) {
      calib_label <- colnames(alpha_scores)[j]
      min_same_label <-
        min(distance_list[which(calibration_set[, predict_pointer] == calib_label)])
      min_diff_label <-
        min(distance_list[which(calibration_set[, predict_pointer] != calib_label)])
      alpha_scores[i,j] <- min_same_label/min_diff_label
    }
  }
  #in some we weight the example itself but in this case we will default to 1
  theta<-1
  #obtaining p values by comparing to the calibration set
  for(i in 1:nrow(testing_set)) {
    count_vals<-lapply(alpha_scores[i,], function(x) sum(nonconform_calib>=x, na.rm = TRUE))
    
    p_vals[i,]<-lapply(count_vals, function(x) (x)/(calib_size+1))
  }
  
  scores_svm<-rep(NA,nrow(testing_set))
  scores_adabag<-rep(NA,nrow(testing_set))
  scores_rf<-rep(NA,nrow(testing_set))
  
  for(i in 1:nrow(testing_set)) {
    if(rf_predict[i]==testing_set[i,predict_pointer]){
      scores_rf[i]<-1-max(p_vals[i,which(colnames(p_vals)!=rf_predict[i])])
    } else {
      scores_rf[i]<-max(p_vals[i,which(colnames(p_vals)!=rf_predict[i])])
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
  
  alpha_five_scores_svm<-rep(NA,nrow(testing_set))
  alpha_five_scores_adabag<-rep(NA,nrow(testing_set))
  alpha_five_scores_rf<-rep(NA,nrow(testing_set))
  
  alpha_ten_scores_svm<-rep(NA,nrow(testing_set))
  alpha_ten_scores_adabag<-rep(NA,nrow(testing_set))
  alpha_ten_scores_rf<-rep(NA,nrow(testing_set))
  
  max_scores_svm<-rep(NA,nrow(testing_set))
  max_scores_adabag<-rep(NA,nrow(testing_set))
  max_scores_rf<-rep(NA,nrow(testing_set))
  
  for(i in 1:nrow(testing_set)) {
    #precalculating the conformity scores
    rf_conformity<-1-max(p_vals[i,which(colnames(p_vals)!=rf_predict[i])])
    adabag_conformity<-1-max(p_vals[i,which(colnames(p_vals)!=adabag_predict[i])])
    svm_conformity<-1-max(p_vals[i,which(colnames(p_vals)!=svm_predict[i])])
    
    #calculating alpha_five_scores
    if(rf_conformity>=0.95) {
      if(rf_predict[i]==testing_set[i,predict_pointer]) {
        alpha_five_scores_rf[i]<-1
      } else {
        alpha_five_scores_rf[i]<-0
      } 
    } else {
      alpha_five_scores_rf[i]<-0.5
    }
      
    if(svm_conformity>=0.95) {
      if(svm_predict[i]==testing_set[i,predict_pointer]) {
        alpha_five_scores_svm[i]<-1
      } else {
        alpha_five_scores_svm[i]<-0
      } 
    } else {
      alpha_five_scores_svm[i]<-0.5
    }
    
    if(adabag_conformity>=0.95) {
      if(adabag_predict[i]==testing_set[i,predict_pointer]) {
        alpha_five_scores_adabag[i]<-1
      } else {
        alpha_five_scores_adabag[i]<-0
      } 
    } else {
      alpha_five_scores_adabag[i]<-0.5
    }
    
    #calculating alpha_ten_scores
    if(rf_conformity>=0.9) {
      if(rf_predict[i]==testing_set[i,predict_pointer]) {
        alpha_ten_scores_rf[i]<-1
      } else {
        alpha_ten_scores_rf[i]<-0
      } 
    } else {
      alpha_ten_scores_rf[i]<-0.5
    }
    
    if(svm_conformity>=0.9) {
      if(svm_predict[i]==testing_set[i,predict_pointer]) {
        alpha_ten_scores_svm[i]<-1
      } else {
        alpha_ten_scores_svm[i]<-0
      } 
    } else {
      alpha_ten_scores_svm[i]<-0.5
    }
    
    if(adabag_conformity>=0.9) {
      if(adabag_predict[i]==testing_set[i,predict_pointer]) {
        alpha_ten_scores_adabag[i]<-1
      } else {
        alpha_ten_scores_adabag[i]<-0
      } 
    } else {
      alpha_ten_scores_adabag[i]<-0.5
    }
    
    #calculating the max conformity scores
    if(rf_predict[i]==testing_set[i,predict_pointer]) {
      max_scores_rf[i]<-max(1-p_vals[i,])
    } else {
      max_scores_rf[i]<-min(1-p_vals[i,])
    }
    
    if(svm_predict[i]==testing_set[i,predict_pointer]) {
      max_scores_svm[i]<-max(1-p_vals[i,])
    } else {
      max_scores_svm[i]<-min(1-p_vals[i,])
    }
    
    if(adabag_predict[i]==testing_set[i,predict_pointer]) {
      max_scores_adabag[i]<-max(1-p_vals[i,])
    } else {
      max_scores_adabag[i]<-min(1-p_vals[i,])
    }
  }
  
  

  conform_scores<-data.frame("scores_adabag"=scores_adabag,"scores_svm"=scores_svm, "scores_rf"=scores_rf)
  conform_alpha_ten<-data.frame("scores_adabag"=alpha_ten_scores_adabag,"scores_svm"=alpha_ten_scores_svm, "scores_rf"=alpha_ten_scores_rf)
  conform_alpha_five<-data.frame("scores_adabag"=alpha_five_scores_adabag,"scores_svm"=alpha_five_scores_svm, "scores_rf"=alpha_five_scores_rf)
  conform_max_vals<-data.frame("scores_adabag"=max_scores_adabag,"scores_svm"=max_scores_svm, "scores_rf"=max_scores_rf)
  write.csv(conform_alpha_ten, file.path(".", filepath,"nonconformal_alpha_ten.csv"))
  write.csv(conform_alpha_five, file.path(".", filepath,"nonconformal_alpha_five.csv"))
  write.csv(conform_max_vals, file.path(".", filepath,"nonconformal_max_vals.csv"))
  return(conform_scores)
}


  args <- commandArgs(TRUE)
  data_file<-args[1]
  predict_pointer<-as.numeric(args[2])
  data_set<-read.csv(args[1])

  nonconformal_output<-nonconformalCI(data_set,predict_pointer, args[3])
  write.csv(nonconformal_output, file.path(".", args[3],"nonconformal.csv"))

