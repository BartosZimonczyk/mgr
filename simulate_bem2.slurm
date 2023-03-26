#!/bin/bash
#SBATCH --time=4:00:00
#SBATCH --job-name="mgr_$2"
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=1GB
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=298754@uwr.edu.pl
#SBATCH --out="outscript_mgr_$2.out"

# load modules
source /usr/local/sbin/modules.sh

# load R version
module load R/4.1.0-foss-2021a

# execute given R script
Rscript $1 >& Rscript_output.txt
