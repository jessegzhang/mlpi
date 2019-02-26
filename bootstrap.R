require(caret)
require(C50)
require(snow)
require(e1071)

knn_training <- function(training_set, testing_set, predict_pointer, col_name ){
  formula <- as.formula(paste(col_name, ' ~ .' ))
  tr_control<- trainControl(method="none") 
  tryCatch({
    knn_fit<-train(formula, data=training_set, method="knn", trControl=tr_control,
                   preProcess=c("center","scale"))
  }, error = function(error_condition) {
    print(error_condition)
    print("adding more noise to the data to attempt to fix")
    for(i in 1:ncol(training_set)) {
      if(is.numreic(training_set[,1])){
        sd<-min(1,training_set[which.max(training_set[,1]),1]/10)
        training_set[,1]<-training_set[,1]+rnorm(nrow(training_set),0,sd)
      }
    }
    knn_fit<-train(formula, data=training_set, method="knn", trControl=tr_control,
                   preProcess=c("center","scale"))
  })
  knn_predict<-predict(knn_fit,testing_set)
  return(mean(knn_predict==testing_set[,predict_pointer]))
  
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
  clusterEvalQ(cl, {library(caret); library(C50); library(e1071)})  
  set.seed(322)
  
  #comment out used for initial testing
  
  #splitting my datasets
  data_set[,predict_pointer]<-as.factor(data_set[,predict_pointer])
  data_splits <- createDataPartition(y = data_set[,predict_pointer], p = 0.66,list = FALSE)
  training_set <- data_set[data_splits,]
  testing_set <- data_set[-data_splits,]
  testing_chop <- testing_set[,-predict_pointer]
  knn_bootstrap<-c(rep(NA,1000))
  cfifty_bootstrap<-c(rep(NA,1000))
  svm_bootstrap<-c(rep(NA,1000))
  col_name <- colnames(training_set)[predict_pointer]

  boot_strapped_data<- vector(mode = "list", length = 1000)
  for (i in 1:1000){
    boot_strapped_data[[i]] <- training_set[sample(nrow(training_set), nrow(training_set), replace = TRUE, prob=NULL), ]
  }
  
  knn_bootstrap<-parSapply(cl, boot_strapped_data, knn_training, testing_set=testing_set, predict_pointer=predict_pointer, col_name=col_name)
  svm_bootstrap<-parSapply(cl, boot_strapped_data, svm_training, testing_set=testing_set, predict_pointer=predict_pointer, col_name=col_name)
  cfifty_bootstrap<-parSapply(cl, boot_strapped_data, cfifty_training, testing_set=testing_set, predict_pointer=predict_pointer, col_name=col_name)
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
  
  
  knn_ci<-quantile(knn_bootstrap,c(0.025,0.975))
  svm_ci<-quantile(svm_bootstrap,c(0.025,0.975))
  cfifty_ci<-quantile(cfifty_bootstrap,c(0.025,0.975))
  results<-c("knn_ci"=knn_ci, "svm_ci"=svm_ci, "cfifty_ci"=cfifty_ci)
  
  return(results)
}


args <- commandArgs(TRUE)
data_file<-args[1]
predict_pointer<-as.numeric(args[2])
data_set<-read.csv(args[1])

bootstrap_output<-bootstrapCI(data_set,predict_pointer)
write.csv(bootstrap_output, file="bootstrap.csv")
