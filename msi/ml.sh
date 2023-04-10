#!/bin/bash -l        

#SBATCH --nodes=7

#SBATCH --ntasks=7

#SBATCH --mem=10gb

#SBATCH --time 00:30:00

#SBATCH --mail-type=ALL  

#SBATCH --mail-user=chen6496@umn.edu 

cd ~/psy8960-week11/R

module load R/4.2.2-openblas

Rscript week11-cluster.R
