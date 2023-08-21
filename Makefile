#
# Makefile for m3csdirest.
# Steven J Gibbons 2023/08/21 (Oslo)
# Before trying to compile, please ensure that the following
# lines have the correct locations of SACLIB, LAPACK, and BLAS
# BINDIR must be set to the directory in which you want the executable to reside
#
SACHOME = /home/stg/ext_programs/SAC/sac-102.0
SACLIB  = ${SACHOME}/lib/libsacio.a
LAPACK= /home/stg/ext_programs/LEOPACK-2022-revision/lib/lalib.a
BLAS= /home/stg/ext_programs/LEOPACK-2022-revision/lib/bllib.a
TOPDIR=   /home/stg/SRC
BINDIR=  $(TOPDIR)/BIN
#
# PLEASE CHECK ALL THE ABOVE LINES FOR YOUR SYSTEM ----
#
PROGNAME= m3csdirest
ALLSOURCECODE=  \
   $(PROGNAME).f  
#
SOURCES= \
        $(ALLSOURCECODE)
#
OPTIM=	  -O3
EXEFILE= $(BINDIR)/$(PROGNAME)
FORTRAN= gfortran
#
LIBS=    $(LAPACK) $(BLAS) $(SACLIB) 
#
backup:
	cp -ip $(ALLSOURCECODE) ./BACKUP ; \
	cd ./BACKUP ; \
	\rm -f *.gz ; \
	gzip $(ALLSOURCECODE) ; \
	cd ../
#
$(PROGNAME):	$(ALLSOURCECODE) $(LIBS)
	$(FORTRAN) -o $(EXEFILE) $(ALLSOURCECODE) $(LIBS) $(OPTIM)
