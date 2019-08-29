for(i in 1:100){
  csv_path <- file.path(".","magic_data", paste0("ci_train_",i), "citraining_magic.csv")
  f_path <- paste0("./magic_data/ci_train_",i)
  system(paste('Rscript','svm_leaveoneout.R',csv_path,'11',f_path))
  system(paste('Rscript','bootstrap.R',csv_path,'11',f_path))
  system(paste('Rscript','adabag_leaveoneout.R',csv_path,'11',f_path))
  system(paste('Rscript','rf_leaveoneout.R',csv_path,'11',f_path))
}
