#!/bin/bash
for i in "B3" "ENB" "N2B2" "NC2" "Chi2" "LC" "NC" "Sin" "Cauchy" "Logis" "Norm" "NormShift1" "NormShift2" "NormShift3" "Chi" "EV" "Lehm" "Tuk01" "Tuk7" "HO"
  do
    sbatch -J d$i$2 -o outscript_mgr_$2.out simulate_bem2.slurm ~/mgr/Simulations/MC_powers/$1/Scripts/Q_powers_c230_d$i.R
  done
