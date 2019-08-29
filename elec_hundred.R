for(i in 1:100){
  csv_path <- file.path(".","elec_data", paste0("ci_train_",i), "citraining_elec.csv")
  f_path <- paste0("./elec_data/ci_train_",i)
  print(f_path)
  system(paste('Rscript','svm_leaveoneout.R',csv_path,'9',f_path))
  system(paste('Rscript','bootstrap.R',csv_path,'9',f_path))
  system(paste('Rscript','adabag_leaveoneout.R',csv_path,'9',f_path))
  system(paste('Rscript','rf_leaveoneout.R',csv_path,'9',f_path))
}
