#!/bin/bash
#SBATCH --partition=t1small
#SBATCH --ntasks=2
#SBATCH --tasks-per-node=24

Rscript accuracyci.R eeg-eye-state.csv 15 eeg_eye citraining_eeg_eye.csv
Rscript bootstrap.R citraining_eeg_eye.csv 15 eeg_eye
Rscript adabag_leaveoneout.R citraining_eeg_eye.csv 15 eeg_eye
Rscript svm_leaveoneout.R citraining_eeg_eye.csv 15 eeg_eye
Rscript rf_leaveoneout.R citraining_eeg_eye.csv 15 eeg_eye
Rscript nonconformal.R citraining_eeg_eye.csv 15 eeg_eye
