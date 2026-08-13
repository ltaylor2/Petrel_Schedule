#!/bin/bash
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH -N 1
#SBATCH -n 15
#SBATCH --mem 200G

make clean
make
./lhsp

Rscript --slave R/process_simulation_results.r
Rscript --slave R/analysis.r
