#!/bin/bash
#SBATCH --partition=t1small
#SBATCH --ntasks=2
#SBATCH --tasks-per-node=24

Rscript nonconformal.R citraining_mammo.csv 7 mammo
Rscript nonconformal.R citrain_large_corr.csv 13 nlargecorr
Rscript nonconformal.R citrain_large_no_corr.csv 8 nlargenc
Rscript nonconformal.R citrain_medium_corr.csv 13 nmediumcorr
Rscript nonconformal.R citrain_small_corr.csv 13 nsmallcorr
Rscript nonconformal.R citrain_medium_no_corr.csv 8 nmediumnc
Rscript nonconformal.R citrain_small_no_corr.csv 8 nsmallnc
Rscript nonconformal.R citraining_elec.csv 9 elec
Rscript nonconformal.R citraining_eeg_eye.csv 15 eeg_eye
