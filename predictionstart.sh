#!/bin/bash
 
#SBATCH --job-name=prediction_logging
#SBATCH --chdir=/work/matheis
#SBATCH --output=/work/%u/%x-%A-%a.log
#SBATCH --time=0-00:40:00
#SBATCH --mem-per-cpu=32G
#SBATCH --cpus-per-task=1
 
module load foss/2022b GCC/12.2.0 OpenMPI/4.1.4 R/4.2.2

export MC_CORES=${SLURM_CPUS_PER_TASK:-1}
#echo $MC_CORES 

Rscript --vanilla /home/matheis/workspace/modellierung/SDM_Modellierung/prediction.R
