#!/bin/bash
#SBATCH --partition=t1small
#SBATCH --ntasks=2
#SBATCH --tasks-per-node=24

Rscript accuracyci.R n_large_corr.csv 13 nlargecorr citrain_large_corr.csv
Rscript bootstrap.R citrain_large_corr.csv 13 nlargecorr
Rscript adabag_leaveoneout.R citrain_large_corr.csv 13 nlargecorr
Rscript svm_leaveoneout.R citrain_large_corr.csv 13 nlargecorr
Rscript rf_leaveoneout.R citrain_large_corr.csv 13 nlargecorr
Rscript nonconformal.R citrain_large_corr.csv 13 nlargecorr
