#!/bin/bash
#SBATCH --partition=t1small
#SBATCH --ntasks=2
#SBATCH --tasks-per-node=24

Rscript accuracyci.R n_medium_corr.csv 13 nmediumcorr citrain_medium_corr.csv
Rscript bootstrap.R citrain_medium_corr.csv 13 nmediumcorr
Rscript adabag_leaveoneout.R citrain_medium_corr.csv 13 nmediumcorr
Rscript svm_leaveoneout.R citrain_medium_corr.csv 13 nmediumcorr
Rscript rf_leaveoneout.R citrain_medium_corr.csv 13 nmediumcorr
Rscript nonconformal.R citrain_medium_corr.csv 13 nmediumcorr
