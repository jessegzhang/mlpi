#!/bin/bash
#SBATCH --partition=t1small
#SBATCH --ntasks=2
#SBATCH --tasks-per-node=24

Rscript accuracyci.R n_small_no_corr.csv 8 nsmallnc citrain_small_no_corr.csv
Rscript bootstrap.R citrain_small_no_corr.csv 8 nsmallnc
Rscript adabag_leaveoneout.R citrain_small_no_corr.csv 8 nsmallnc
Rscript svm_leaveoneout.R citrain_small_no_corr.csv 8 nsmallnc
Rscript rf_leaveoneout.R citrain_small_no_corr.csv 8 nsmallnc
Rscript nonconformal.R citrain_small_no_corr.csv 8 nsmallnc
