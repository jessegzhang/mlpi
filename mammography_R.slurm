#!/bin/bash
#SBATCH --partition=t1small
#SBATCH --ntasks=2
#SBATCH --tasks-per-node=24

Rscript accuracyci.R mammography.csv 7 mammo citraining_mammo.csv
Rscript bootstrap.R citraining_mammo.csv 7 mammo
Rscript adabag_leaveoneout.R citraining_mammo.csv 7 mammo
Rscript svm_leaveoneout.R citraining_mammo.csv 7 mammo
Rscript rf_leaveoneout.R citraining_mammo.csv 7 mammo
Rscript nonconformal.R citraining_mammo.csv 7 mammo
