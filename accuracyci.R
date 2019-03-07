require(ggplot2)
require(adabag)
require(caret)
require(randomForest)
require(snow)
require(e1071)
require(gower)


#take in data
args <- commandArgs(TRUE)
data_file<-args[1]
predict_pointer<-as.numeric(args[2])
data_set<-read.csv(args[1])



#split the data
data_set[,predict_pointer]<-as.factor(data_set[,predict_pointer])
data_splits <- createDataPartition(y = data_set[,predict_pointer], p = 0.70,list = FALSE)
training_set <- data_set[data_splits,]
testing_set <- data_set[-data_splits,]

write.csv(training_set, file=args[4], row.names=FALSE)

#train yourself and report population accuracy

col_name <- colnames(data_set)[predict_pointer]
formula <- as.formula(paste(col_name, ' ~ .' ))
tr_control<- trainControl(method="none",allowParallel=TRUE)
rf_fit<-train(formula, data=training_set, method="rf", trControl=tr_control,
                  preProcess=c("center","scale"))
svm_fit<-train(formula, data=training_set, method="svmLinear", trControl=tr_control,
               preProcess=c("center","scale"))
adabag_fit<-train(formula, data=training_set, method="AdaBag", trControl=tr_control,
                  preProcess=c("center","scale"))

rf_predict<-predict(rf_fit,testing_set)
svm_predict<-predict(svm_fit,testing_set)
adabag_predict<-predict(adabag_fit,testing_set)

#write population accuracy into a file

svm_mean<-mean(svm_predict==testing_set[,predict_pointer])
adabag_mean<-mean(adabag_predict==testing_set[,predict_pointer])
rf_mean<-mean(rf_predict==testing_set[,predict_pointer])

final_data<-c("svm_accuracy"=svm_mean, "adabag_accuracy"=adabag_mean, "rf_accuracy"=rf_mean)
dir.create(args[3])
write.csv(final_data, file.path(".", args[3],"true_accuracy.csv"))
