#!/bin/bash
#SBATCH --time=4:00:00
#SBATCH --job-name="mgr_$2"
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4GB
#SBATCH --mail-user=bartosz.zimonczyk@gmail.com

# load modules
source /usr/local/sbin/modules.sh >& load_modules.txt

# load R version
module load "/usr/local/easybuild/python-2.7.15-gcc7.4.0/modules/all/R/3.6.2-foss-2019b" >& load_output.txt

# execute given R script
Rscript $1 >& Rscript_output.txt
