require(ggplot2)
require(adabag)
require(caret)
require(C50)
require(snow)
require(e1071)

adabag_training <- function(training_set, testing_set, predict_pointer, col_name ){
  formula <- as.formula(paste(col_name, ' ~ .' ))
  tr_control<- trainControl(method="none")
    adabag_fit<-train(formula, data=training_set, method="AdaBag", trControl=tr_control,
                   preProcess=c("center","scale"))
  adabag_predict<-predict(adabag_fit,testing_set)
  return(mean(adabag_predict==testing_set[,predict_pointer]))
  
}

svm_training <- function(training_set, testing_set, predict_pointer, col_name ){
  formula <- as.formula(paste(col_name, ' ~ .' ))
  tr_control<- trainControl(method="none")
  svm_fit<-train(formula, data=training_set, method="svmLinear", trControl=tr_control,
                 preProcess=c("center","scale"))
  svm_predict<-predict(svm_fit,testing_set)
  return(mean(svm_predict==testing_set[,predict_pointer]))
}

cfifty_training <- function(training_set, testing_set, predict_pointer, col_name ){
  formula <- as.formula(paste(col_name, ' ~ .' ))
  tr_control<- trainControl(method="none")
  cfifty_fit<-train(formula, data=training_set, method="C5.0",
                    preProcess=c("center","scale"))
  cfifty_predict<-predict(cfifty_fit,testing_set)
  return(mean(cfifty_predict==testing_set[,predict_pointer]))
}


bootstrapCI <- function(data_set, predict_pointer ){
  cl <- makeCluster(16, type = "SOCK", outfile="debug_bootstrap.txt") 
  clusterEvalQ(cl, {library(caret); library(C50); library(e1071); library(adabag)})  
  
  #comment out used for initial testing
  
  #splitting my datasets
  data_set[,predict_pointer]<-as.factor(data_set[,predict_pointer])
  data_splits <- createDataPartition(y = data_set[,predict_pointer], p = 0.70,list = FALSE)
  training_set <- data_set[data_splits,]
  testing_set <- data_set[-data_splits,]
  
  #preallocating space
  adabag_bootstrap<-c(rep(NA,1000))
  cfifty_bootstrap<-c(rep(NA,1000))
  svm_bootstrap<-c(rep(NA,1000))
  col_name <- colnames(training_set)[predict_pointer]

  
  boot_strapped_data<- vector(mode = "list", length = 1000)
  print("bootstrapping data")  
  for (i in 1:1000){
    boot_strapped_data[[i]] <- training_set[sample(nrow(training_set), nrow(training_set), replace = TRUE, prob=NULL), ]
  }
  print("running adabag")
  adabag_bootstrap<-parSapply(cl, boot_strapped_data, adabag_training, testing_set=testing_set, predict_pointer=predict_pointer, col_name=col_name)
  print("running svm")
  svm_bootstrap<-parSapply(cl, boot_strapped_data, svm_training, testing_set=testing_set, predict_pointer=predict_pointer, col_name=col_name)
  print("running cfifty")
  cfifty_bootstrap<-parSapply(cl, boot_strapped_data, cfifty_training, testing_set=testing_set, predict_pointer=predict_pointer, col_name=col_name)
  print("writing results")
  # for (j in 1:200) {
  #   
  #   #knn confidence interval
  #   knn_fit<-train(formula, data=boot_strapped_data, method="knn", trControl=tr_control,
  #                  preProcess=c("center","scale"))
  #   knn_predict<-predict(knn_fit,testing_set)
  #   knn_bootstrap[j] <- mean(knn_predict==testing_set[,predict_pointer])
  #   
  #   svm_fit<-train(formula, data=boot_strapped_data, method="svmLinear", trControl=tr_control,
  #                  preProcess=c("center","scale"))
  #   svm_predict<-predict(svm_fit,testing_set)
  #   svm_bootstrap[j] <- mean(svm_predict==testing_set[,predict_pointer])
  #   
  #   cfifty_fit<-train(formula, data=boot_strapped_data, method="C5.0",
  #                  preProcess=c("center","scale"))
  #   cfifty_predict<-predict(cfifty_fit,testing_set)
  #   cfifty_bootstrap[j] <- mean(cfifty_predict==testing_set[,predict_pointer])
  #   
  #   
  # }
  
  #calculating confidence intervals
  adabag_ci<-quantile(adabag_bootstrap,c(0.025,0.975))
  svm_ci<-quantile(svm_bootstrap,c(0.025,0.975))
  cfifty_ci<-quantile(cfifty_bootstrap,c(0.025,0.975))
  
  results<-c("adabag_ci"=adabag_ci, "svm_ci"=svm_ci, "cfifty_ci"=cfifty_ci)
  
  ada_plot<-qplot(adabag_bootstrap, geom="histogram")
  svm_plot<-qplot(svm_bootstrap, geom="histogram")
  cfifty_plot<-qplot(cfifty_bootstrap, geom="histogram")
  ggsave(ada_plot, file="ada_hist.pdf")
  ggsave(svm_plot, file="svm_hist.pdf")
  ggsave(cfifty_plot, file="cfifty_hist.pdf")
  return(results)
}


 args <- commandArgs(TRUE)
 data_file<-args[1]
 predict_pointer<-as.numeric(args[2])
 data_set<-read.csv(args[1])
 
 bootstrap_output<-bootstrapCI(data_set,predict_pointer)
 write.csv(bootstrap_output, file.path(".", args[3],"bootstrap.csv"))
