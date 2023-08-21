# m3csdirest  
Multiple 3-Component Station DIRection ESTimation (m3csdirest) 

Release v1.0.1 is permanently stored on Zenodo with DOI 10.5281/zenodo.8270726  

https://doi.org/10.5281/zenodo.8270726  

[![DOI](https://zenodo.org/badge/681303774.svg)](https://zenodo.org/badge/latestdoi/681303774)  

m3csdirest is a FORTRAN77 program that tries to estimate the direction of incoming P-waves on 3-component stations.
It reads in SAC-format files and so you need to have SAC available in order to compile it in its current form.
If you are not able to obtain SAC for whatever reason, you will need to replace the lines which call
the subroutine RSAC1 with some other routine which fills the variables RZVALS, NPTS, BEG, and DT.
It also requires the LAPACK and BLAS libraries for linear algebra routines that interpolate to find the
optimal backazimuth.  

The program should compile by typing   

make m3csdirest  

in the directory created once you have edited the makefile.  

This program writes out ASCII output and is not intended for operational use in any routine system.
It is designed as a proof of concept program.  

# Example  

(1) Go into the directory KEV_NZ20101011 and run the python script  get_KEV.py  
    This should generate 3 SAC-files: IU_KEV_BHE_10.sac  IU_KEV_BHN_10.sac  IU_KEV_BHZ_10.sac  

(2) Assuming that you have managed to compile the program m3csdirest and have placed it in your path, run the script run_KEV_example.sh
