cat << EOF | qsub
#!/bin/bash
#PBS -q main
#PBS -l walltime=2:00:00
#PBS -l select=1:ncpus=1:mem=2048MB
#PBS -l software=qsub_stdin
#PBS -m be

# load R version
module load R/3.6.2-foss-2019b >& output1.txt

# execute given R script
Rscript $1 >& output2.txt
EOF
