#!/bin/bash
#SBATCH --time=24:00:00
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4GB
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=298754@uwr.edu.pl

# load modules
source /usr/local/sbin/modules.sh

# load R version
module load R/4.1.0-foss-2021a

# execute given R script
Rscript $1 >& Rscript_output.txt
