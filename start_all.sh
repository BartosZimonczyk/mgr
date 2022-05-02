#!/bin/bash
for i in {00 05 10 15 20 25 30 35 40 45 50}
  do
    ./simulate.sh "Simulations/MC_quantiles/$1/Scripts/Q_quantiles_c2$i.R" "Qc2$i"
  done

