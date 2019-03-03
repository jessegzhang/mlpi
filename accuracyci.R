require(ggplot2)
require(adabag)
require(caret)
require(C50)
require(snow)
require(e1071)
require(gower)

source("leaveoneout.R")
source("nonconformal.R")
source("bootstrap.R")

print("reading data in")
#take in data
args <- commandArgs(TRUE)
data_file<-args[1]
predict_pointer<-as.numeric(args[2])
data_set<-read.csv(args[1])

print("splitting data")
#split the data
data_set[,predict_pointer]<-as.factor(data_set[,predict_pointer])
data_splits <- createDataPartition(y = data_set[,predict_pointer], p = 0.70,list = FALSE)
training_set <- data_set[data_splits,]
testing_set <- data_set[-data_splits,]

#spool up threads
cl <- makeCluster(16, type = "SOCK", outfile="debug_bootstrap.txt") 
clusterEvalQ(cl, {library(caret); library(C50); library(e1071); library(adabag)})  

#call the functions
print("running leave one out")
leaveoneout<-leaveoneoutCI(training_set,predict_pointer, cl)
print("running non conformal")
non_conformal<-nonconformalCI(training_set,predict_pointer, cl)
print("running bootstrap")
boot_strap<-bootstrapCI(training_set,predict_pointer)

print("writting info")
#write everything
file_dir<-sub(".csv$","",args[1])
dir.create(file_dir)

write.csv(leaveoneout, file=file.path(".",file_dir,"leaveoneout.csv"))
write.csv(non_conformal, file=file.path(".",file_dir,"nonconformal.csv"))
write.csv(boot_strap, file=file.path(".",file_dir,"boot_strap.csv"))

#train yourself and report population accuracy

col_name <- colnames(data_set)[predict_pointer]
formula <- as.formula(paste(col_name, ' ~ .' ))
tr_control<- trainControl(method="none",allowParallel=TRUE)
cfifty_fit<-train(formula, data=training_set, method="C5.0",
                  preProcess=c("center","scale"))
svm_fit<-train(formula, data=training_set, method="svmLinear", trControl=tr_control,
               preProcess=c("center","scale"))
adabag_fit<-train(formula, data=training_set, method="AdaBag", trControl=tr_control,
                  preProcess=c("center","scale"))

cfifty_predict<-predict(cfifty_fit,testing_set)
svm_predict<-predict(svm_fit,testing_set)
adabag_predict<-predict(adabag_fit,testing_set)

#write population accuracy into a file

svm_mean<-mean(svm_predict==testing_set[,predict_pointer])
adabag_mean<-mean(adabag_predict==testing_set[,predict_pointer])
cfifty_mean<-mean(cfifty_predict==testing_set[,predict_pointer])

data<-c("svm_accuracy"=svm_mean, "adabag_accuracy"=adabag_mean, "cfifty_accuracy"=cfifty_mean)