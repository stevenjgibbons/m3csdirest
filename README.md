# m3csdirest  
Multiple 3-Component Station DIRection ESTimation (m3csdirest)  

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
