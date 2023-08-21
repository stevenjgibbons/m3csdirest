#!/bin/sh
# Note that the KEV data collected have 40 Hz sampling
# so 4 seconds is 160 samples.
# This script should calculate the output for the files
# generated in the directory KEV_NZ20101011
#
inputfile=KEV_m3csdirest.input
cat << EOF > ${inputfile}
KEV_NZ20101011/IU_KEV_BHZ_10.sac   KEV
KEV_NZ20101011/IU_KEV_BHE_10.sac   90.0
KEV_NZ20101011/IU_KEV_BHN_10.sac    0.0
EOF
NSTAT=1
NCCLEN=160
NSKIP=40
NAZIB2=36
ICFLAG=1
OUTFLAG=1111
m3csdirest $NSTAT $NCCLEN $NSKIP $NAZIB2 $ICFLAG $OUTFLAG < $inputfile > KEV.out
