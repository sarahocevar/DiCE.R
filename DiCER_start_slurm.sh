#!/bin/bash
#SBATCH --job-name=dicer
#SBATCH --account=[addaccountid]
#SBATCH --time=12:00:00
#SBATCH --mem=1G
#SBATCH --partition=small
#SBATCH --cpus-per-task=1
#SBATCH --array=1-40  


module load r-env-singularity

mkdir -p slurm_DiCE_out

R CMD BATCH --no-save --no-restore DiCE.R slurm_DiCE_out/${SLURM_JOB_ID}_${SLURM_ARRAY_TASK_ID}.out
