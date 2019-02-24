require(caret)
require(C50)
require(snow)
require(e1071)

bootstrapCI <- function(data_set, predict_pointer ){
  cl <- makeCluster(8, type = "SOCK") 
  clusterEvalQ(cl, {library(caret); library(C50); library(e1071)})  
  set.seed(322)
  
  #comment out used for initial testing
  data_set<-iris
  predict_pointer<-5
  
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

knn_training <- function(training_set, testing_set, predict_pointer, col_name ){
  formula <- as.formula(paste(col_name, ' ~ .' ))
  tr_control<- trainControl(method="none") 
  knn_fit<-train(formula, data=training_set, method="knn", trControl=tr_control,
                 preProcess=c("center","scale"))
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

data("iris")
bootstrap_output<-bootstrapCI(iris,5)
fileConn<-file("bootstrap_output.txt")
writeLines(bootstrap_output, fileConn)
close(fileConn)
