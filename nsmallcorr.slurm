#!/bin/bash
#SBATCH --partition=t1small
#SBATCH --ntasks=2
#SBATCH --tasks-per-node=24

Rscript accuracyci.R n_small_corr.csv 13 nsmallcorr citrain_small_corr.csv
Rscript bootstrap.R citrain_small_corr.csv 13 nsmallcorr
Rscript adabag_leaveoneout.R citrain_small_corr.csv 13 nsmallcorr
Rscript svm_leaveoneout.R citrain_small_corr.csv 13 nsmallcorr
Rscript rf_leaveoneout.R citrain_small_corr.csv 13 nsmallcorr
Rscript nonconformal.R citrain_small_corr.csv 13 nsmallcorr
