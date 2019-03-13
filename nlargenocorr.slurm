#!/bin/bash
#SBATCH --partition=t1small
#SBATCH --ntasks=2
#SBATCH --tasks-per-node=24

Rscript accuracyci.R n_large_no_corr.csv 8 nlargenc citrain_large_no_corr.csv
Rscript bootstrap.R citrain_large_no_corr.csv 8 nlargenc
Rscript adabag_leaveoneout.R citrain_large_no_corr.csv 8 nlargenc
Rscript svm_leaveoneout.R citrain_large_no_corr.csv 8 nlargenc
Rscript rf_leaveoneout.R citrain_large_no_corr.csv 8 nlargenc
Rscript nonconformal.R citrain_large_no_corr.csv 8 nlargenc
