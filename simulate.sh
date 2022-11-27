cat << EOF | qsub
#!/bin/bash
#PBS -l walltime=10:00:00
#PBS -l select=1:ncpus=1:mem=8192MB
#PBS -l software=qsub_stdin
#PBS -N "$2"
#PBS -m n

# go to the mgr directory to execute Rscript properly
cd mgr

# load R version
module load R/3.6.2-foss-2019b >& load_output.txt

# execute given R script
Rscript $1 >& Rscript_output.txt
EOF
