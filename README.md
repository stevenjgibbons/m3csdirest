# m3csdirest  
Multiple 3-Component Station DIRection ESTimation (m3csdirest) 

Release v1.0.0 is permanently stored on Zenodo with DOI 10.5281/zenodo.8270726  

https://doi.org/10.5281/zenodo.8270726  

[![DOI](https://zenodo.org/badge/681303774.svg)](https://zenodo.org/badge/latestdoi/681303774)  

[![SQAaaS badge](https://github.com/EOSC-synergy/SQAaaS/raw/master/badges/badges_150x116/badge_software_bronze.png)](https://api.eu.badgr.io/public/assertions/DmOJYacOTASd09xnXedYwQ "SQAaaS bronze badge achieved")

[![SQAaaS badge shields.io](https://img.shields.io/badge/sqaaas%20software-bronze-e6ae77)](https://api.eu.badgr.io/public/assertions/DmOJYacOTASd09xnXedYwQ "SQAaaS bronze badge achieved")


m3csdirest is a FORTRAN77 program that tries to estimate the direction of incoming P-waves on 3-component stations.
It reads in SAC-format files and so you need to have SAC available in order to compile it in its current form.
If you are not able to obtain SAC for whatever reason, you will need to replace the lines which call
the subroutine RSAC1 with some other routine which fills the variables RZVALS, NPTS, BEG, and DT.
It also requires the LAPACK and BLAS libraries for linear algebra routines that interpolate to find the
optimal backazimuth.  

The program should compile by typing   

```
make m3csdirest
```  

in the directory created once you have edited the makefile.  

This program writes out ASCII output and is not intended for operational use in any routine system.
It is designed as a proof of concept program.  

# Example  

(1) Go into the directory KEV_NZ20101011 and run the python script  get_KEV.py  
    This should generate 3 SAC-files: IU_KEV_BHE_10.sac  IU_KEV_BHN_10.sac  IU_KEV_BHZ_10.sac  

(2) Assuming that you have managed to compile the program m3csdirest and have placed it in your path, run the script run_KEV_example.sh

```
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
```
# Auxiliary program - aziwsa  

This program just tries to make a weighted average of azimuith values.  
An example is done by running  

```
sh run_aziwsa.sh
python plot_aziwsa_out.py
```



Relevant paper  
==============

If you use the code, I would like you to refer to the following publication:  

Gibbons, S.J. (2023),  
Direction Estimates for Short‐Period P‐Waves on Three‐Component Stations and Arrays,  
*The Seismic Record*. 3(4), pp. 299–310
https://doi.org/10.1785/0320230036

![promotional image for https://doi.org/10.1785/0320230036](array3c_promo_image.png)!
