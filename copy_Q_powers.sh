#!/bin/bash
for i in "ENB" "N2B2" "NC2" "Chi2" "LC" "NC" "Sin" "Cauchy" "Logis" "Norm" "NormShift1" "NormShift2" "NormShift3" "Chi" "EV" "Lehm" "Tuk01" "Tuk7"
  do
    echo "Copying $i..."
    cp ~/Repos/mgr/Simulations/MC_powers/$1/Scripts/Q_powers_c230_dB3.R ~/Repos/mgr/Simulations/MC_powers/$1/Scripts/Q_powers_c230_d$i.R
  done
