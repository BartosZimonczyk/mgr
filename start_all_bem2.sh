#!/bin/bash
for i in 100 105 110 115 120 125 130 135 140 145 150 155 160 165 170 175 180 185 190 195 200 205 210 215 220 225 230 235 240 245 250
  do
    sbatch simulate_bem2.slurm "~/mgr/Simulations/MC_quantiles/$1/Scripts/Q_quantiles_c2$i.R" "c$i$2"
  done
