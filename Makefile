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
PROGNAME1= m3csdirest
PROGNAME2= aziwsa
ALLSOURCECODE=  \
   $(PROGNAME1).f   \
   $(PROGNAME2).f  
#
SOURCES= \
        $(ALLSOURCECODE)
#
OPTIM=	  -O3
EXEFILE1= $(BINDIR)/$(PROGNAME1)
EXEFILE2= $(BINDIR)/$(PROGNAME2)
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
$(PROGNAME1):	$(PROGNAME1).f $(LIBS)
	$(FORTRAN) -o $(EXEFILE1) $(PROGNAME1).f $(LIBS) $(OPTIM)
$(PROGNAME2):	$(PROGNAME2).f $(LIBS)
	$(FORTRAN) -o $(EXEFILE2) $(PROGNAME2).f $(LIBS) $(OPTIM)
