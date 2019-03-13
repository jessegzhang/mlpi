#!/bin/bash
#SBATCH --partition=t1small
#SBATCH --ntasks=2
#SBATCH --tasks-per-node=24

Rscript accuracyci.R n_medium_no_corr.csv 8 nmediumnc citrain_medium_no_corr.csv
Rscript bootstrap.R citrain_medium_no_corr.csv 8 nmediumnc
Rscript adabag_leaveoneout.R citrain_medium_no_corr.csv 8 nmediumnc
Rscript svm_leaveoneout.R citrain_medium_no_corr.csv 8 nmediumnc
Rscript rf_leaveoneout.R citrain_medium_no_corr.csv 8 nmediumnc
Rscript nonconformal.R citrain_medium_no_corr.csv 8 nmediumnc
