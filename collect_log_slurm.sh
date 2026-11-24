#!/bin/bash
#SBATCH --job-name=dicelog
#SBATCH --account=project_2005983
#SBATCH --time=4:00:00
#SBATCH --mem=1G
#SBATCH --partition=small
#SBATCH --cpus-per-task=1

module load r-env-singularity

dir=$1

Rscript Utils/collect_log.R ${dir} 2> collect_log_err.txt
