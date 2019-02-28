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

args <- commandArgs(TRUE)
data_file<-args[1]
predict_pointer<-as.numeric(args[2])
data_set<-read.csv(args[1])

#take in data
#split the data
#call the functions
#write everything
#train yourself and report population accuracy