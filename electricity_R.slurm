#!/bin/bash
#SBATCH --partition=t1small
#SBATCH --ntasks=2
#SBATCH --tasks-per-node=24

Rscript accuracyci.R electricity-normalized.csv 9 elec citraining_elec.csv
Rscript bootstrap.R citraining_elec.csv 9 elec
Rscript adabag_leaveoneout.R citraining_elec.csv 9 elec
Rscript svm_leaveoneout.R citraining_elec.csv 9 elec
Rscript rf_leaveoneout.R citraining_elec.csv 9 elec
Rscript nonconformal.R citraining_elec.csv 9 elec
