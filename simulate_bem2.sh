cat << EOF | sbatch
#!/bin/bash
#SBATCH --time=4:00:00
#SBATCH --job-name="mgr_$2"
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4GB

# go to the mgr directory to execute Rscript properly
cd mgr

# load modules
source /usr/local/sbin/modules.sh

# load R version
module load R/3.6.2-foss-2019b >& load_output.txt

# execute given R script
Rscript $1 >& Rscript_output.txt
EOF
