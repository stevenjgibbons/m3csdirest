C
C azimuth weighted stack average
C StG 2024-05-14
C
C We read in a list of NAZVAL values
C   azimuth  weight
C
C and make a stack of curves
C  dcos( azi - azimuth_i )*DABS( dcos( azi - azimuth_i ) )
C
C
C DWAZSF - Double Precision Weighted stack Azimuth Sum Form
C
      PROGRAM aziwsa
      IMPLICIT NONE
C
      INTEGER     NAZIB2
      PARAMETER ( NAZIB2 = 180 )
      INTEGER     IERR
      INTEGER     IAZ
      INTEGER     NAZMAX
      PARAMETER ( NAZMAX = 1000 )
      INTEGER     NAZVAL
      REAL*8      DAZVLS( NAZMAX )
      REAL*8      DWGVLS( NAZMAX )
      REAL*8      DWGSTK( NAZIB2*2 )
      REAL*8      DWORK( NAZIB2*2 )
      REAL*8      D1
      REAL*8      D2
      CHARACTER*(80) CHLINE
C
      NAZVAL  = 0
 50   CONTINUE
      CHLINE  = ' '
      READ (5,'(A)',END=60,ERR=50) CHLINE
      IF ( CHLINE(1:1).EQ.'*' ) GOTO 50
      IF ( CHLINE(1:1).EQ.'#' ) GOTO 50
      READ ( CHLINE, *, END=60,ERR=50) D1, D2
      NAZVAL = NAZVAL + 1
      IF ( NAZVAL.GT.NAZMAX ) THEN
        WRITE (6,*) 'NAZMAX about to be exceeded '
        GOTO 999
      ENDIF
      DAZVLS( NAZVAL ) = D1
      DWGVLS( NAZVAL ) = D2
      GOTO 50
 60   CONTINUE
      CALL DWAZSF( IERR, NAZIB2, NAZVAL, DAZVLS, DWGVLS,
     1             DWGSTK, DWORK )
C
      DO IAZ = 1, NAZIB2*2
        WRITE (6,71) IAZ-1, DWGSTK( IAZ )
      ENDDO
 71   FORMAT(I10,1X,f16.8)
C
      CALL EXIT(0)
 999  CONTINUE
      CALL EXIT(1)
      END
C
      SUBROUTINE DWAZSF( IERR, NAZIB2, NAZVAL, DAZVLS, DWGVLS, 
     1                   DWGSTK, DWORK )
      IMPLICIT NONE
C
      INTEGER     IERR
      INTEGER     NAZIB2
      INTEGER     NAZVAL
      REAL*8      DAZVLS( NAZVAL )
      REAL*8      DWGVLS( NAZVAL )
      REAL*8      DWGSTK( NAZIB2*2 )
      REAL*8      DWORK( NAZIB2*2 )
C
      INTEGER     IAZ
      INTEGER     IAZVAL
      REAL*8      DLOW
      PARAMETER ( DLOW = 1.0d-6 )
      REAL*8      DTOTWT
      REAL*8      DFAC
      REAL*8      DAZREF
C
      IERR   = 0
      IF ( NAZIB2.LT.4 ) THEN
        WRITE (6,*) 'Subroutine DWAZSF: NAZIB2 = ', NAZIB2
        IERR = 1
        RETURN
      ENDIF
C
C Calculate total weight ...
C
      DTOTWT = 0.0d0
      DO IAZVAL = 1, NAZVAL
        DFAC   = DWGVLS( IAZVAL )
        IF ( DFAC.LT.DLOW ) THEN
          WRITE (6,*) 'Subroutine DWAZSF: weight ',IAZVAL,' = ',DFAC
          IERR = 1
          RETURN
        ENDIF
        DTOTWT = DTOTWT + DFAC
      ENDDO
C
C Now zero the weighted stack ...
C
      DO IAZ = 1, NAZIB2
        DWGSTK(  IAZ  ) = 0.0d0
      ENDDO
C
C Now loop around the azimuth values and calculate the unit norm function
C then add the weighted function to the stack.
C
      DO IAZVAL = 1, NAZVAL
        DFAC   = DWGVLS( IAZVAL )/DTOTWT
        DAZREF = DAZVLS( IAZVAL )
c       print *, IAZVAL, DAZREF, DFAC
        CALL ACNCF2( IERR, NAZIB2, DAZREF, DWORK )
        DO IAZ = 1, NAZIB2*2
          DWGSTK(  IAZ  ) = DWGSTK(  IAZ  ) + DFAC*DWORK(  IAZ  )
        ENDDO
      ENDDO
C
      RETURN
      END
C

C
C Azimuth Centered Normalized Cosine Function calculate 2
C ACNCF2 makes curve with unit norm for azimuith DAZREF
C
      SUBROUTINE ACNCF2( IERR, NAZIB2, DAZREF, DACNCF )
      IMPLICIT NONE
C
      INTEGER IERR
      INTEGER NAZIB2
      REAL*8  DAZREF
      REAL*8  DACNCF( 2*NAZIB2 )
C
      REAL*8             DPI
      PARAMETER        ( DPI    = 3.14159265358979d0 )
      REAL*8             DEGRAD
      PARAMETER        ( DEGRAD = DPI/180.0d0 )
C
      INTEGER IAZ
      REAL*8  RDAZIM
      REAL*8  AZII
      REAL*8  AZIDEG
      REAL*8  DSUM
      REAL*8  DSCALE
      REAL*8  AZIRAD
      REAL*8  DVALUE
C
      IERR   = 0
C return if NAZIB2 is invalid
      IF ( NAZIB2.LT.4 ) THEN
        WRITE (6,*) 'Subroutine ACNCF2: NAZIB2 = ', NAZIB2
        IERR = 1
        RETURN
      ENDIF
C
      RDAZIM = 180.0d0/DBLE( NAZIB2 )
      AZII   = 0.0d0
      DO IAZ = 1, NAZIB2*2
        AZIDEG = AZII - DAZREF
        AZIRAD = AZIDEG*DEGRAD
        DVALUE = DCOS( AZIRAD )
        DACNCF( IAZ ) = DVALUE*DABS( DVALUE )
        AZII   = AZII + RDAZIM
      ENDDO
C
C We now want to ensure that DACNCF has unit norm
C
      DSUM   = 0.0d0
      DO IAZ = 1, NAZIB2*2
        DSUM   = DSUM + DACNCF( IAZ )*DACNCF( IAZ )
      ENDDO
      DSCALE = 1.0d0/DSQRT( DSUM )
      DO IAZ = 1, NAZIB2*2
        DACNCF( IAZ ) = DACNCF( IAZ )*DSCALE
      ENDDO
C
      RETURN
      END
C

