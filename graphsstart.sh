#!/bin/bash
 
#SBATCH --job-name=graphs_logging
#SBATCH --chdir=/work/matheis
#SBATCH --output=/work/%u/%x-%A-%a.log
#SBATCH --time=0-00:45:00
#SBATCH --mem-per-cpu=32G
#SBATCH --cpus-per-task=1
 
module load foss/2022b GCC/12.2.0 OpenMPI/4.1.4 R/4.2.2

export MC_CORES=${SLURM_CPUS_PER_TASK:-1}

Rscript --vanilla /home/matheis/workspace/modellierung/SDM_Modellierung/graphs.R
