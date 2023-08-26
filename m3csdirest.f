C
C Steven J. Gibbons
C NGI 2023/03/25
C m3csdirest.f
C
C Multiple 3-component Station DIRection ESTimate
C
C Takes 5 arguments (integers)
C         NSTAT  - number of stations
C         NCCLEN - number of samples for cross-correlation
C         NSKIP  - number of samples to skip between CC windows
C                  (i.e. this determines the sampling of the output)
C         NAZIB2 - half the number of azimuths to search
C                  (so delazi = 180.0/FLOAT( NAZIB2 ) )
C         ICFLAG - 1 to use CC, 2 to use CC * | CC |
C
C plus (optionally) a sixth argument (character string)
C specifying the output required.
C
C     Set CHOUTR(1:1) to '1' if we wish to write out the values
C                            for the time-step or box with the highest values
C                            for the average over all stations.
C     Set CHOUTR(2:2) to '1' if we wish to write out the values
C                            for all time-steps for the average over all 
C                            stations.
C     Set CHOUTR(3:3) to '1' if we wish to write out the values
C                            for all time-steps for all individual stations.
C     Set CHOUTR(4:4) to '1' if we wish to write out the values of the grids
C                            for all time-steps for all individual stations.
C
      PROGRAM m3csdirest
      IMPLICIT NONE
C
C First define parameters
C
      INTEGER     NSTATM
      PARAMETER ( NSTATM = 25 )
C  nb a value of 360000 will allow an hour of data at 100 Hz sampling
C  NMXSMP is maximum sampling rate (samples per second)
C  We allow up to 75 minutes so that we can take a full hour + a generous
C  body-wave travel-time.
      INTEGER     NMXSMP
      PARAMETER ( NMXSMP = 100 )
      INTEGER     NPTSMX
      PARAMETER ( NPTSMX = 75*60*NMXSMP )
c     PARAMETER ( NPTSMX = 360000 )
      INTEGER     NBOXMX
      PARAMETER ( NBOXMX = 3600 )
      INTEGER     NAZ2MX
      PARAMETER ( NAZ2MX = 180 )
      INTEGER     NAZIMX
      PARAMETER ( NAZIMX = 2*NAZ2MX )
      REAL        RCCVLS( NAZIMX, NBOXMX, NSTATM )
      REAL        RCCIDM( NAZIMX, NBOXMX, NSTATM )
      REAL        RACNCF( NAZIMX )
      REAL        RCCVLS_avg( NAZIMX, NBOXMX )
      REAL        RCCIDM_avg( NAZIMX, NBOXMX )
      INTEGER     NPTS
C
      CHARACTER*(5) CSTATN( NSTATM )
C
      REAL*4      RZVALS( NPTSMX )
      REAL*4      R1VALS( NPTSMX )
      REAL*4      R2VALS( NPTSMX )
      REAL*4      RRVALS( NPTSMX )
      REAL*4      RZCOMP( NPTSMX, NSTATM )
      REAL*4      RNCOMP( NPTSMX, NSTATM )
      REAL*4      RECOMP( NPTSMX, NSTATM )
C
C For each "box" i.e. time-point
C OPACVL( ibox, istat ) will contain the optimal azimuth for that station
C and OPACVL_avg( ibox ) will contain the optimal azimuth for the sum.
C OPVCVL( ibox, istat ) will contain the corresponding value for that station
C and OPVCVL_avg( ibox ) will contain the corresponding value for the sum.
C These are calculated from RCCVLS and RCCVLS_avg, the raw correlation values.
C
      REAL*4      OPACVL( NBOXMX, NSTATM )
      REAL*4      OPACVL_avg( NBOXMX )
      REAL*4      OPVCVL( NBOXMX, NSTATM )
      REAL*4      OPVCVL_avg( NBOXMX )
C
C For each "box" i.e. time-point
C OPAIDM( ibox, istat ) will contain the optimal azimuth for that station
C and OPAIDM_avg( ibox ) will contain the optimal azimuth for the sum.
C OPVIDM( ibox, istat ) will contain the corresponding value for that station
C and OPVIDM_avg( ibox ) will contain the corresponding value for the sum.
C These are calculated from RCCIDM and RCCIDM_avg, the best matches with
C the ideal functions.
C
      REAL*4      OPAIDM( NBOXMX, NSTATM )
      REAL*4      OPAIDM_avg( NBOXMX )
      REAL*4      OPVIDM( NBOXMX, NSTATM )
      REAL*4      OPVIDM_avg( NBOXMX )
C
      CHARACTER*(32)  CHARG
      CHARACTER*(250) CLARG
      CHARACTER*(250) CFNAME
C
C Arguments for the routine CSARGM
C
      INTEGER     IERR
      INTEGER     NARGM
      PARAMETER ( NARGM = 2 )
      INTEGER     NARGS, CSLEN, IARGL( NARGM, 2 )
C
C Now define non-fixed variables
C
      INTEGER     NBOX 
      INTEGER     NLONG
      INTEGER     NSTAT
      INTEGER     NCCLEN
      INTEGER     NSKIP 
      INTEGER     NAZIB2
      INTEGER     ICFLAG
      INTEGER     ISTAT
      REAL*4      RAZIH1, RAZIH2
      REAL*4      BEG, DT
      REAL*4      DTBOX
c     REAL*4      DTV
      INTEGER     I1
      INTEGER     I2
      INTEGER     ILEN
C
      INTEGER     NIARGS
      INTEGER     IARGC
      INTEGER     IARG
C
C NPTSV is the "valid" number of points read in
C It is initialized to the first value of NPTS read
C and if subsequent values are read that are different
C then we abort.
C
      INTEGER     NPTSV
C
      CHARACTER*(12) CHOUTR
C
C Read the input parameters ...
C
      NPTSV   = 0
      NIARGS  = IARGC()
      IF ( NIARGS.LT.5 ) THEN
        WRITE ( 6, * ) 'Usage: m3csdirest NS NCC NSKIP NAZIB2 ICC'
        WRITE ( 6, * ) '                  [output_speciier]      '
        CALL EXIT(1)
      ENDIF
C
C Read in the output specifier if provided.
C If it is not provided, we assume that we only want to output
C the values for the box with the greatest coherence.
C
      CHOUTR = ' '
      IF ( NIARGS.EQ.6 ) THEN
        CHARG  = ' '
        IARG   = 6
        CALL GETARG( IARG, CHARG )
        CHOUTR(1:12) = CHARG(1:12)
      ELSE
        CHOUTR(1:12) = '1xxxxxxxxxxx'
      ENDIF
C
C Read NSTAT
C
      CHARG  = ' '
      IARG   = 1
      CALL GETARG( IARG, CHARG )
      READ ( CHARG, *, END=99, ERR=99 ) NSTAT
      IF ( NSTAT.LT.1 .OR. NSTAT.GT.NSTATM ) THEN
        WRITE ( 6, * ) 'NSTAT  = ', NSTAT
        WRITE ( 6, * ) 'NSTATM = ', NSTATM
        GOTO 99
      ENDIF
      PRINT *,' NSTAT = ', NSTAT
C
C Read NCCLEN
C
      CHARG  = ' '
      IARG   = 2
      CALL GETARG( IARG, CHARG )
      READ ( CHARG, *, END=99, ERR=99 ) NCCLEN
      IF ( NCCLEN.LT.2 ) THEN
        WRITE ( 6, * ) 'NCCLEN = ', NCCLEN
        GOTO 99
      ENDIF
      PRINT *,' NCCLEN = ', NCCLEN
C
C Read NSKIP
C
      CHARG  = ' '
      IARG   = 3
      CALL GETARG( IARG, CHARG )
      READ ( CHARG, *, END=99, ERR=99 ) NSKIP
      IF ( NSKIP.LT.2 ) THEN 
        WRITE ( 6, * ) 'NSKIP = ', NSKIP
        GOTO 99
      ENDIF
      PRINT *,' NSKIP = ', NSKIP
C
C Read NAZIB2
C
      CHARG  = ' '
      IARG   = 4
      CALL GETARG( IARG, CHARG )
      READ ( CHARG, *, END=99, ERR=99 ) NAZIB2
      IF ( NAZIB2.GT.NAZ2MX ) THEN
        WRITE ( 6, * ) 'NAZIB2 = ', NAZIB2
        GOTO 99
      ENDIF
      PRINT *,' NAZIB2 = ', NAZIB2
C
C Read ICFLAG
C
      CHARG  = ' '
      IARG   = 5
      CALL GETARG( IARG, CHARG )
      READ ( CHARG, *, END=99, ERR=99 ) ICFLAG
      IF ( ICFLAG.LT.1 .OR. ICFLAG.GT.2 ) THEN
        WRITE ( 6, * ) 'ICFLAG = ', ICFLAG
        GOTO 99
      ENDIF
      PRINT *,' ICFLAG = ', ICFLAG
C
C
c     INTEGER     NCCLEN
c     INTEGER     NSKIP 
c     INTEGER     NAZIB2
c     INTEGER     ICFLAG
C
C Now need to loop around the NSTAT stations and read
C them in from standard input. We read in the H1 and H2
C components and rotate them into North-South and East-West
C components
C
      DO ISTAT = 1, NSTAT
C       .
        WRITE (6,*) 'Enter Filename for station ', ISTAT,' Z'
        WRITE (6,*) 'followed by the station name.'
C       .
 51     CONTINUE
        CLARG  = ' '
        READ (5,'(A)',END=99,ERR=99) CLARG
        IF ( CLARG(1:1).EQ.'*' ) GOTO 51
C       .
C       . The character string CLARG contains a filename and a station name
C       .
        NARGS  = 2
        CSLEN  = 250
        CALL CSARGM( CLARG, NARGS, NARGM, CSLEN, IARGL, IERR )
        IF ( IERR.NE.0 ) THEN
          WRITE (6,*) 'CSARGM returned IERR = ', IERR
          GOTO 99
        ENDIF
        IARG   = 1
        I1     = IARGL( IARG, 1 )
        I2     = IARGL( IARG, 2 )
        ILEN   = I2 - I1 + 1
        CFNAME = ' '
        CFNAME(1:ILEN) = CLARG(I1:I2)
C       .
C       . Extract station name
C       .
        IARG   = 2
        I1     = IARGL( IARG, 1 )
        I2     = IARGL( IARG, 2 )
        ILEN   = I2 - I1 + 1
        IF ( ILEN.GT.5 ) THEN
          ILEN  = 5
          I2    = I1 + 4
        ENDIF
        CSTATN( ISTAT ) = '     '
        CSTATN( ISTAT )(1:ILEN) = CLARG(I1:I2)
C       .
C       . We have now a filename for the Z-component
C       .
        CALL RSAC1( CFNAME, RZVALS, NPTS, BEG, DT, NPTSMX, IERR ) 
        IF ( IERR.NE.0 ) THEN
          WRITE (6,*) 'RSAC1 returned IERR = ', IERR,' for file'
          WRITE (6,*) CFNAME
          GOTO 99
        ENDIF
        IF ( NPTSV.EQ.0 ) THEN
          NPTSV = NPTS
c         DTV   = DT
          PRINT *,' Using NPTS = ', NPTS,' DT = ', DT
        ELSE
          IF ( NPTS.NE.NPTSV ) THEN
            WRITE (6,*) 'Read trace with ', NPTS,' samples'
            WRITE (6,*) 'Also read trace with ', NPTSV,' samples'
            GOTO 99
          ENDIF
        ENDIF
C       .
        WRITE (6,*) 'Enter Filename and azi for station ',ISTAT,' H1'
 52     CONTINUE
        CLARG  = ' '
        READ (5,'(A)',END=99,ERR=99) CLARG 
        IF ( CLARG(1:1).EQ.'*' ) GOTO 52
C       .
C       . The character string CLARG contains a filename and an azi
C       .
        NARGS  = 2
        CSLEN  = 250
        CALL CSARGM( CLARG, NARGS, NARGM, CSLEN, IARGL, IERR )
        IF ( IERR.NE.0 ) THEN
          WRITE (6,*) 'CSARGM returned IERR = ', IERR
          GOTO 99
        ENDIF
        IARG   = 1
        I1     = IARGL( IARG, 1 )
        I2     = IARGL( IARG, 2 )
        ILEN   = I2 - I1 + 1
        CFNAME = ' '
        CFNAME(1:ILEN) = CLARG(I1:I2)
C       .
C       . We now have a filename for the H1 component
C       .
        CALL RSAC1( CFNAME, R1VALS, NPTS, BEG, DT, NPTSMX, IERR )
        IF ( IERR.NE.0 ) THEN
          WRITE (6,*) 'RSAC1 returned IERR = ', IERR,' for file'
          WRITE (6,*) CFNAME
          GOTO 99
        ENDIF
C       . We do not check DT. NPTS should already be set
        IF ( NPTS.NE.NPTSV ) THEN
          WRITE (6,*) 'Read trace with ', NPTS,' samples'
          WRITE (6,*) 'Also read trace with ', NPTSV,' samples'
          GOTO 99
        ENDIF
C       .
C       . Now we need to read the azimuth of channel H1
C       .
        IARG   = 2
        I1     = IARGL( IARG, 1 )
        I2     = IARGL( IARG, 2 )
        READ ( CLARG(I1:I2), *, ERR=99, END=99 ) RAZIH1
        PRINT *,' Azimuth for channel H1 = ', RAZIH1
C       .
        WRITE (6,*) 'Enter Filename and azi for station ',ISTAT,' H2'
 53     CONTINUE
        CLARG  = ' '
        READ (5,'(A)',END=99,ERR=99) CLARG
        IF ( CLARG(1:1).EQ.'*' ) GOTO 53
C       .
C       . The character string CLARG contains a filename and an azi
C       .
        NARGS  = 2
        CSLEN  = 250
        CALL CSARGM( CLARG, NARGS, NARGM, CSLEN, IARGL, IERR )
        IF ( IERR.NE.0 ) THEN
          WRITE (6,*) 'CSARGM returned IERR = ', IERR
          GOTO 99
        ENDIF
        IARG   = 1
        I1     = IARGL( IARG, 1 )
        I2     = IARGL( IARG, 2 )
        ILEN   = I2 - I1 + 1
        CFNAME = ' '
        CFNAME(1:ILEN) = CLARG(I1:I2)
C       .
C       . We now have a filename for the H2 component
C       .
        CALL RSAC1( CFNAME, R2VALS, NPTS, BEG, DT, NPTSMX, IERR )
        IF ( IERR.NE.0 ) THEN
          WRITE (6,*) 'RSAC1 returned IERR = ', IERR,' for file'
          WRITE (6,*) CFNAME
          GOTO 99
        ENDIF
C       . We do not check DT. NPTS should already be set
        IF ( NPTS.NE.NPTSV ) THEN
          WRITE (6,*) 'Read trace with ', NPTS,' samples'
          WRITE (6,*) 'Also read trace with ', NPTSV,' samples'
          GOTO 99
        ENDIF
C       .
C       . Now we need to read the azimuth of channel H2
C       .
        IARG   = 2
        I1     = IARGL( IARG, 1 )
        I2     = IARGL( IARG, 2 )
        READ ( CLARG(I1:I2), *, ERR=99, END=99 ) RAZIH2
        PRINT *,' Azimuth for channel H2 = ', RAZIH2
C       .
C       . We are now ready to transfer
C       - RZVALS, R1VALS, R2VALS into the appropriate
C       . elements of RZCOMP, RNCOMP, RECOMP
C       .
        CALL ADD2ST( IERR, NPTS, NSTAT, ISTAT, RAZIH1, RAZIH2,
     1               RZVALS, R1VALS, R2VALS,
     2               RZCOMP, RNCOMP, RECOMP )
        IF ( IERR.NE.0 ) THEN
          WRITE (6,*) 'ADD2ST returned IERR = ', IERR
          GOTO 99
        ENDIF
C       .
      ENDDO
C
C Now we have filled arrays RZCOMP, RNCOMP, RECOMP
C with the Z, N and E components for all stations.
C We also know NPTSV, NCCLEN, NSKIP so we can work out NBOX
C
      NLONG  = NPTS  - NCCLEN + NSKIP
      NBOX   = NLONG/NSKIP
      IF ( NBOX.LT.1 .OR. NBOX.GT.NBOXMX ) THEN
        WRITE (6,*) 'NBOX   = ', NBOX
        GOTO 99
      ENDIF
      DTBOX  = DT*REAL( NSKIP )
C
C Now call the correlation routine
C
      CALL CCTCAL( IERR, NSTAT, NAZIB2, NBOX, NPTS, ICFLAG,
     1             NSKIP, NCCLEN, RCCVLS,
     2             RZCOMP, RNCOMP, RECOMP, RRVALS )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Subroutine CCTCAL returned IERR = ', IERR
        GOTO 99
      ENDIF
C
C Now we calculate RCCIDM - how well the calculated values
C match the ideal values.
C
      CALL CCCOSC( IERR, NAZIB2, NBOX, NSTAT,
     1             RCCVLS, RCCIDM, RACNCF )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Subroutine CCCOSC returned IERR = ', IERR
        GOTO 99
      ENDIF
C     .
C     . Now calculate the averages
C     .
      CALL MSTAVG( IERR, NAZIB2, NBOX, NSTAT, RCCVLS, RCCVLS_avg )
      CALL MSTAVG( IERR, NAZIB2, NBOX, NSTAT, RCCIDM, RCCIDM_avg )
C     .
C     . Now need to calculate as accurately as we can the
C     . optimal backazimuths for all stations and sums
C     . for both CCV and ICM variables.
C     .
      CALL OPAZIF( IERR, NAZIB2, NBOX, NSTAT, RCCVLS, RCCVLS_avg,
     1             OPACVL, OPACVL_avg, OPVCVL, OPVCVL_avg, RACNCF )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Subroutine OPAZIF returned IERR = ', IERR
        GOTO 99
      ENDIF
      CALL OPAZIF( IERR, NAZIB2, NBOX, NSTAT, RCCIDM, RCCIDM_avg,
     1             OPAIDM, OPAIDM_avg, OPVIDM, OPVIDM_avg, RACNCF )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Subroutine OPAZIF returned IERR = ', IERR
        GOTO 99
      ENDIF
C     .
C     . Now write out all values
C     .
      CALL VALWRT( IERR, NAZIB2, NBOX, NSTAT, CHOUTR, CSTATN,
     1             RCCVLS, RCCVLS_avg, 
     2             RCCIDM, RCCIDM_avg,
     3             OPACVL, OPACVL_avg, OPVCVL, OPVCVL_avg,
     4             OPAIDM, OPAIDM_avg, OPVIDM, OPVIDM_avg,
     5             DTBOX )
C     .
      CALL EXIT(0)
C     Normal exit
C
 99   CONTINUE
      WRITE (6,*) 'Abnormal exit.'
      CALL EXIT(1)
      END
C=====================================================================
C
      SUBROUTINE VALWRT( IERR, NAZIB2, NBOX, NSTAT, CHOUTR, CSTATN,
     1                   RCCVLS, RCCVLS_avg, 
     2                   RCCIDM, RCCIDM_avg,
     3                   OPACVL, OPACVL_avg, OPVCVL, OPVCVL_avg,
     4                   OPAIDM, OPAIDM_avg, OPVIDM, OPVIDM_avg,
     5                   DTBOX )
      IMPLICIT NONE
C
      INTEGER            IERR, NAZIB2, NBOX, NSTAT
      CHARACTER*(12)     CHOUTR
      CHARACTER*(5)      CSTATN( NSTAT )
      REAL               RCCVLS( NAZIB2*2, NBOX, NSTAT )
      REAL               RCCVLS_avg( NAZIB2*2, NBOX )
      REAL               RCCIDM( NAZIB2*2, NBOX, NSTAT )
      REAL               RCCIDM_avg( NAZIB2*2, NBOX )
      REAL*4             OPACVL( NBOX, NSTAT )
      REAL*4             OPACVL_avg( NBOX )
      REAL*4             OPVCVL( NBOX, NSTAT )
      REAL*4             OPVCVL_avg( NBOX )
      REAL*4             OPAIDM( NBOX, NSTAT )
      REAL*4             OPAIDM_avg( NBOX )
      REAL*4             OPVIDM( NBOX, NSTAT )
      REAL*4             OPVIDM_avg( NBOX )
      REAL*4             DTBOX
C
      INTEGER IBOX, ISTAT, IAZ
      REAL*4  RDAZIM
      REAL*4  RAZI
      INTEGER IBOXOP
      REAL*4  RVALOP
      REAL*4  CENAZ1
      REAL*4  CENAZ2
C
C First find the "optimal box" - that is the box with the best coherence
C
      RVALOP = OPVIDM_avg( 1 )
      IBOXOP = 1
      DO IBOX = 2, NBOX
        IF ( OPVIDM_avg( IBOX ).GT.RVALOP ) THEN
          IBOXOP = IBOX
          RVALOP = OPVIDM_avg( IBOX )
        ENDIF
      ENDDO
C
      RDAZIM = 180.0/REAL( NAZIB2 )
      RAZI   = 0.0
      IF ( CHOUTR( 4: 4).EQ.'1' ) THEN
      DO IAZ = 1, NAZIB2*2
        CENAZ1 = RAZI
        IF ( CENAZ1.GT.180.0 ) CENAZ1 = CENAZ1 - 360.0
        DO IBOX = 1, NBOX
          WRITE (6,81) 'IDMMEAN', IBOX, RAZI, CENAZ1,
     1                     RCCIDM_avg( IAZ, IBOX )
          WRITE (6,81) 'CCVMEAN', IBOX, RAZI, CENAZ1,
     1                     RCCVLS_avg( IAZ, IBOX )
          DO ISTAT = 1, NSTAT
            WRITE (6,82) 'IDM',ISTAT,IBOX,RAZI,CENAZ1,
     1                     RCCIDM(IAZ, IBOX, ISTAT )
            WRITE (6,82) 'CCV',ISTAT,IBOX,RAZI,CENAZ1,
     1                     RCCVLS(IAZ, IBOX, ISTAT )
          ENDDO
        ENDDO
        RAZI = RAZI + RDAZIM
      ENDDO
      ENDIF
 81   FORMAT(A7,1X,I4,1X,f6.2,1X,f7.2,1X,f7.4)
 82   FORMAT(A3,I4.4,1X,I4,1X,f6.2,1X,f7.2,1X,f7.4)
C
C This is the option where we just want to write out the 
C optimal azimuth for box with optimal coherence
C We want to write the azimuth both in the interval [0,360] and in
C the interval [-180,180]
C
      IF ( CHOUTR( 1: 1).EQ.'1' ) THEN
        CENAZ1 = OPACVL_avg( IBOXOP )
        IF ( CENAZ1.GT.180.0 ) CENAZ1 = CENAZ1 - 360.0
        CENAZ2 = OPAIDM_avg( IBOXOP )
        IF ( CENAZ2.GT.180.0 ) CENAZ2 = CENAZ2 - 360.0
        WRITE (6,71) IBOXOP, REAL(IBOXOP-1)*DTBOX,
     1               OPACVL_avg( IBOXOP ), CENAZ1, OPVCVL_avg( IBOXOP ),
     2               OPAIDM_avg( IBOXOP ), CENAZ2, OPVIDM_avg( IBOXOP )
 71   FORMAT('OptBOX ',I4,' t ',f9.3,1X,f7.3,1X,f8.3,1X,
     1                          f5.3,1X,f7.3,1X,f8.3,1X,f5.3)
      ENDIF
C
      IF ( CHOUTR( 2: 2).EQ.'1' ) THEN
        DO IBOX = 1, NBOX
          CENAZ1 = OPACVL_avg( IBOX )
          IF ( CENAZ1.GT.180.0 ) CENAZ1 = CENAZ1 - 360.0
          CENAZ2 = OPAIDM_avg( IBOX )
          IF ( CENAZ2.GT.180.0 ) CENAZ2 = CENAZ2 - 360.0
          WRITE (6,72) IBOX, REAL(IBOX-1)*DTBOX,
     1               OPACVL_avg( IBOX ), CENAZ1, OPVCVL_avg( IBOX ),
     2               OPAIDM_avg( IBOX ), CENAZ2, OPVIDM_avg( IBOX )
        ENDDO
 72   FORMAT('OptAZI ',I4,' t ',f9.3,1X,f7.3,1X,f8.3,1X,
     1                          f5.3,1X,f7.3,1X,f8.3,1X,f5.3)
      ENDIF
C
C If CHOUTR( 3: 3).EQ.'1' then we want to write out the optimal
C azimuth values for the individual stations ...
C
      IF ( CHOUTR( 3: 3).EQ.'1' ) THEN
        DO ISTAT = 1, NSTAT
          DO IBOX = 1, NBOX
            CENAZ1 = OPACVL( IBOX, ISTAT )
            IF ( CENAZ1.GT.180.0 ) CENAZ1 = CENAZ1 - 360.0
            CENAZ2 = OPAIDM( IBOX, ISTAT )
            IF ( CENAZ2.GT.180.0 ) CENAZ2 = CENAZ2 - 360.0
            WRITE (6,73) ISTAT, IBOX, REAL(IBOX-1)*DTBOX,
     1             OPACVL( IBOX, ISTAT ), CENAZ1, OPVCVL( IBOX, ISTAT ),
     2             OPAIDM( IBOX, ISTAT ), CENAZ2, OPVIDM( IBOX, ISTAT ),
     3             CSTATN( ISTAT )
          ENDDO
        ENDDO
 73   FORMAT('OAZStat ',I4,1X,I4,' t ',f9.3,1X,f7.3,1X,f8.3,1X,
     1                  f5.3,1X,f7.3,1X,f8.3,1X,f5.3,1X,A5)
      ENDIF
C
      RETURN
      END
C
C=====================================================================
C MSTAVG - Multiple station average calculate
C Does the trivial job of summing up the values over multiple
C stations.
C  RFMULS for multiple stations has dimensions ( NAZIB2*2, NBOX, NSTAT )
C  RFONES for one station has dimensions ( NAZIB2*2, NBOX )
C
      SUBROUTINE MSTAVG( IERR, NAZIB2, NBOX, NSTAT,
     1                   RFMULS, RFONES )
      IMPLICIT NONE
C
      INTEGER   IERR
      INTEGER   NAZIB2
      INTEGER   NBOX
      INTEGER   NSTAT
      REAL*4    RFMULS( NAZIB2*2, NBOX, NSTAT )
      REAL*4    RFONES( NAZIB2*2, NBOX )
C
      INTEGER   IAZ
      INTEGER   IBOX
      INTEGER   ISTAT
      REAL*4    RSCALE
C
      IERR   = 0
      IF ( NSTAT.LT.1 ) THEN
        IERR   = 1
        RETURN
      ENDIF
      RSCALE = 1.0/REAL( NSTAT )
      DO IAZ = 1, NAZIB2*2
        DO IBOX = 1, NBOX
          RFONES( IAZ, IBOX ) = 0.0
          DO ISTAT = 1, NSTAT
            RFONES( IAZ, IBOX ) = RFONES( IAZ, IBOX ) + 
     1         RFMULS( IAZ, IBOX, ISTAT )*RSCALE
          ENDDO
        ENDDO
      ENDDO
C
      RETURN
      END
C

C=====================================================================
C Cross-correlation terms Cosine Correlate
C
C We have a real*4 array with dimensions ( NAZIB2*2, NBOX, NSTAT )
C called RCCVLS which contains for each backazimuth i = 1, NAZIB2*2
C the correlation coefficient ( or C|C| value ) between the vertical
C sensor and the radial rotation.
C
C If the ideal relation is a cosine function (as calculated in the
C routine ACNCFC) then CCCOSC calculates for each azimuth, the 
C match between the actual values recorded and the ideal values.
C For this we use a working array RACNCF( NAZIB2*2 )
C This is stored in an array RCCIDM with the same dimensions as
C RCCVLS.
C
      SUBROUTINE CCCOSC( IERR, NAZIB2, NBOX, NSTAT,
     1                   RCCVLS, RCCIDM, RACNCF )
      IMPLICIT NONE
C
      INTEGER            IERR
      INTEGER            NAZIB2
      INTEGER            NBOX
      INTEGER            NSTAT
      REAL               RCCVLS( NAZIB2*2, NBOX, NSTAT )
      REAL               RCCIDM( NAZIB2*2, NBOX, NSTAT )
      REAL               RACNCF( NAZIB2*2 )
C
      INTEGER            IBOX
      INTEGER            ISTAT
      INTEGER            IAZ
      REAL*4             RDAZIM
      REAL*4             AZIREF
      REAL*4             RSUM
      REAL*4             RCCVAL
      REAL*4             RCMAX
      REAL*4             RNORM
      REAL*4             RVAL
      INTEGER            JAZ
C
      IERR   = 0
C
C return if NAZIB2 is invalid
      IF ( NAZIB2.LT.4 ) THEN
        WRITE (6,*) 'Subroutine CCCOSC: NAZIB2 = ', NAZIB2
        IERR = 1
        RETURN
      ENDIF
C
C We loop around IAZ on the outside for the simple reason
C that we calculate RACNCF only once per azimuth if we do
C it this way around ...
C
      RDAZIM = 180.0/REAL( NAZIB2 )
      AZIREF = 0.0
      DO IAZ = 1, NAZIB2*2
C       .
C       . First calculate the ideal cosine relation for
C       . this reference azimuth. Do not need to error
C       . check as it only tests NAZIB2 which is tested here too.
C       .
        CALL ACNCFC( IERR, NAZIB2, AZIREF, RACNCF )
C       .
C       . RACNCF is already normalized after the call to ACNCFC
C       . We need to calculate the maximum value of RCCVLS
C       . and the norm.
C       .
        DO IBOX = 1, NBOX
          DO ISTAT = 1, NSTAT
            RSUM   = 0.0
            RNORM  = 0.0
            RCMAX  = 0.0
            DO JAZ = 1, NAZIB2*2
              RVAL  = RCCVLS(JAZ,IBOX,ISTAT)
              IF ( RVAL.GT.RCMAX ) RCMAX = RVAL
              RSUM  = RSUM  + RACNCF(JAZ)*RVAL
              RNORM = RNORM + RVAL*RVAL
            ENDDO
            RNORM   = 1.0/SQRT( RNORM )
            RCCVAL  = RSUM*RNORM
            RCCIDM( IAZ, IBOX, ISTAT ) = RCMAX * RCCVAL
          ENDDO
        ENDDO
C       .
        AZIREF = AZIREF + RDAZIM
      ENDDO
C
      RETURN
      END
C
C=====================================================================
C Azimuth Centered Normalized Cosine Function Calculate (ACNCFC)
C
C We receive a real*4 array of size NAZIB2*2 and fill it with the
C values of a cosine function centered at backazimuth aziref,
C specified in degrees.
C RACNCF( i ) is filled with cos( azii - aziref )
C where azii is given by (i-1)*360.0/REAL( NAZIB2*2 )
C 
      SUBROUTINE ACNCFC( IERR, NAZIB2, AZIREF, RACNCF )
      IMPLICIT NONE
C
      INTEGER IERR
      INTEGER NAZIB2
      REAL*4  AZIREF
      REAL*4  RACNCF( 2*NAZIB2 )
C
      REAL*8             DPI
      PARAMETER        ( DPI    = 3.14159265358979d0 )
      REAL*8             DEGRAD
      PARAMETER        ( DEGRAD = DPI/180.0d0 )
C
      INTEGER IAZ
      REAL*4  RDAZIM
      REAL*4  AZII
      REAL*4  AZIDEG
      REAL*4  RSUM
      REAL*4  RSCALE
      REAL*8  AZIRAD
      REAL*8  DVALUE
C
      IERR   = 0
C return if NAZIB2 is invalid
      IF ( NAZIB2.LT.4 ) THEN
        WRITE (6,*) 'Subroutine ACNCFC: NAZIB2 = ', NAZIB2
        IERR = 1
        RETURN
      ENDIF
C
      RDAZIM = 180.0/REAL( NAZIB2 )
      AZII   = 0.0
      DO IAZ = 1, NAZIB2*2
        AZIDEG = AZII - AZIREF
        AZIRAD = DBLE( AZIDEG )*DEGRAD
        DVALUE = DCOS( AZIRAD )
        RACNCF( IAZ ) = REAL( DVALUE )
        AZII   = AZII + RDAZIM
      ENDDO
C
C We now want to ensure that RACNCF has unit norm
C
      RSUM   = 0.0
      DO IAZ = 1, NAZIB2*2
        RSUM   = RSUM + RACNCF( IAZ )*RACNCF( IAZ )
      ENDDO
      RSCALE = 1.0/SQRT( RSUM )
      DO IAZ = 1, NAZIB2*2
        RACNCF( IAZ ) = RACNCF( IAZ )*RSCALE
      ENDDO
C
      RETURN
      END
C
C=====================================================================
C Cross-correlation terms calculate
C We loop around stations ISTAT to NSTAT
C and loop around IAZI from 1, NAZIB2 and calculate the radial
C component for each azimuth.
C Then we calculate the simularity between the vertical
C trace and the radial traces in windows of length NCCLEN,
C in NBOX boxes every NSKIP samples
ccc      REAL        RCCVLS( NAZIMX, NBOXMX, NSTATM )
ccc      REAL*4      RZCOMP( NPTS, NSTAT )
ccc      REAL*4      RNCOMP( NPTS, NSTAT )
ccc      REAL*4      RECOMP( NPTS, NSTAT )
ccc      REAL*4      RRCOMP( NPTS )
C
      SUBROUTINE CCTCAL( IERR, NSTAT, NAZIB2, NBOX, NPTS, ICFLAG,
     1                   NSKIP, NCCLEN, RCCVLS,
     2                   RZCOMP, RNCOMP, RECOMP, RRCOMP )
      IMPLICIT NONE
C
      INTEGER            IERR
      INTEGER            NSTAT
      INTEGER            NAZIB2
      INTEGER            NBOX
      INTEGER            NPTS
      INTEGER            ICFLAG
      INTEGER            NSKIP
      INTEGER            NCCLEN
      REAL               RCCVLS( NAZIB2*2, NBOX, NSTAT )
      REAL               RZCOMP( NPTS, NSTAT )
      REAL               RNCOMP( NPTS, NSTAT )
      REAL               RECOMP( NPTS, NSTAT )
      REAL               RRCOMP( NPTS )
C
      INTEGER            IPTST
      INTEGER            IAZI
      INTEGER            ISTAT
      REAL               RDELAZ
      REAL               AZIDEG
      REAL               RVAL
      INTEGER            IBOX
      REAL               RCC
      REAL               RCABSC
C
      IERR   = 0
      IF ( NAZIB2.LT.2 ) THEN
        IERR   = 1
        RETURN
      ENDIF
      RDELAZ = 180.0/REAL( NAZIB2 )
      DO ISTAT = 1, NSTAT
        AZIDEG = 0.0
        DO IAZI = 1, NAZIB2
C         .
C         . We have a new azimuth for station ISTAT.
C         . So we need to put the radial component into RRCOMP
C         .
          CALL NE2RAD( IERR, NPTS, NSTAT, ISTAT, AZIDEG,
     1                 RECOMP, RNCOMP, RRCOMP )
C         .
C         . Do not check for a false return code as this
C         . routine only fails for ISTAT out of range.
C         . that cannot happen here.
C         .
C         . Now we loop from IBOX = 1, NBOX
C         .
          IPTST  = 1
          DO IBOX = 1, NBOX
            CALL CCVCAL( IERR, NPTS, NSTAT, ISTAT, IPTST, NCCLEN,
     1                   RZCOMP, RRCOMP, RCC, RCABSC )
            IF ( IERR.NE.0 ) THEN
              WRITE (6,*) 'Subroutine CCTCAL'
              WRITE (6,*) 'CCVCAL returned IERR = ', IERR
              IERR   = 1
              RETURN
            ENDIF
            IF ( ICFLAG.EQ.1 ) THEN
              RVAL   = RCC
            ELSE
              RVAL   = RCABSC
            ENDIF
            RCCVLS( IAZI         , IBOX, ISTAT ) =  RVAL
            RCCVLS( IAZI + NAZIB2, IBOX, ISTAT ) = -RVAL
c          write (6,*) istat, iazi, azideg, ibox, rval
c          write (6,*) istat, iazi + NAZIB2, azideg+180.0, ibox, -rval
            IPTST  = IPTST + NSKIP
          ENDDO
C         .
          AZIDEG = AZIDEG + RDELAZ
        ENDDO
      ENDDO
C
      RETURN
      END
C
C=====================================================================
C REW = eastwest(i)
C RNS = northsouth(i)
C r(i) = -RNS*cos( AZI ) - REW*sin( AZI )
C t(i) =  RNS*sin( AZI ) - REW*cos( AZI )
C=====================================================================
C
      SUBROUTINE NE2RAD( IERR, NPTS, NSTAT, ISTAT, AZIDEG,
     1                   RECOMP, RNCOMP, RRCOMP )
      IMPLICIT NONE
C
      INTEGER            IERR
      INTEGER            NPTS
      INTEGER            NSTAT
      INTEGER            ISTAT
      REAL               AZIDEG
      REAL               RECOMP( NPTS, NSTAT )
      REAL               RNCOMP( NPTS, NSTAT )
      REAL               RRCOMP( NPTS )
C
      REAL*8             DPI
      PARAMETER        ( DPI    = 3.14159265358979d0 )
      REAL*8             DEGRAD
      PARAMETER        ( DEGRAD = DPI/180.0d0 )
      REAL*8             DAZRAD
      REAL*8             DCOSAZ
      REAL*8             DSINAZ
      REAL*4             RCOSAZ
      REAL*4             RSINAZ
      REAL*4             RNS
      REAL*4             REW
C
      INTEGER            IPT
C
      IERR   = 0
      IF ( ISTAT.LT.1 .OR. ISTAT.GT.NSTAT ) THEN
        WRITE (6,*) 'Subroutine NE2RAD'
        WRITE (6,*) 'ISTAT   = ', ISTAT
        WRITE (6,*) 'NSTAT   = ', NSTAT
        IERR = 1
        RETURN
      ENDIF
C
      DAZRAD = DBLE( AZIDEG )*DEGRAD
      DCOSAZ = DCOS( DAZRAD )
      DSINAZ = DSIN( DAZRAD )
      RCOSAZ = REAL( DCOSAZ )
      RSINAZ = REAL( DSINAZ )
C
      DO IPT = 1, NPTS
        RNS         = RNCOMP( IPT, ISTAT )
        REW         = RECOMP( IPT, ISTAT )
        RRCOMP(IPT) = -RNS*RCOSAZ - REW*RSINAZ 
      ENDDO
C
      RETURN
      END
C
C r(i) = -RNS*cos( AZI ) - REW*sin( AZI )
C
C CCVCAL just does the actual correlation calculation
C between the vertical component of station ISTAT and
C the present radial component
C and returns the fully normalized correlation coefficient and
C the value of C * abs( C )
C
C We calculate this value over NCCLEN samples starting at point IPTST
C
      SUBROUTINE CCVCAL( IERR, NPTS, NSTAT, ISTAT, IPTST, NCCLEN,
     1                   RZCOMP, RRCOMP, RCC, RCABSC )
      IMPLICIT NONE
C
      INTEGER            IERR
      INTEGER            NPTS
      INTEGER            NSTAT
      INTEGER            ISTAT
      INTEGER            IPTST
      INTEGER            NCCLEN
      REAL               RZCOMP( NPTS, NSTAT )
      REAL               RRCOMP( NPTS )
      REAL               RCC
      REAL               RCABSC
C
      INTEGER            ILAST
      INTEGER            IPT
      REAL               RLOW
      PARAMETER        ( RLOW = 0.00001 )
      REAL               RSCALE
      REAL               RNORMR
      REAL               RNORMZ
      REAL               RPROD
C
      IERR   = 0
C - just check that the final node is in range
      ILAST  = IPTST + NCCLEN - 1
      IF ( IPTST.LT.1 .OR. ILAST.GT.NPTS .OR. NCCLEN.LT.1 ) THEN
        WRITE (6,*) 'Subroutine CCVCAL'
        WRITE (6,*) 'NPTS    = ', NPTS
        WRITE (6,*) 'IPTST   = ', IPTST
        WRITE (6,*) 'ILAST   = ', ILAST
        WRITE (6,*) 'NCCLEN  = ', NCCLEN
        IERR = 1
        RETURN
      ENDIF
      IF ( ISTAT.LT.1 .OR. ISTAT.GT.NSTAT ) THEN
        WRITE (6,*) 'Subroutine CCVCAL'
        WRITE (6,*) 'ISTAT   = ', ISTAT
        WRITE (6,*) 'NSTAT   = ', NSTAT
        IERR = 1
        RETURN
      ENDIF
      RSCALE = 1.0/REAL( NCCLEN )
      RCC    = 0.0
      RCABSC = 0.0
      RNORMR = 0.0
      RNORMZ = 0.0
      RPROD  = 0.0
C ... calculate the norms
      DO IPT = IPTST, ILAST
        RNORMR = RNORMR + RRCOMP( IPT )*RRCOMP( IPT )*RSCALE
      ENDDO
c     print *, 'RNORMR = ', RNORMR
      IF ( RNORMR.LT.RLOW ) RETURN
      RNORMR = SQRT( RNORMR )
      DO IPT = IPTST, ILAST
        RNORMZ = RNORMZ + RZCOMP( IPT,ISTAT )*RZCOMP( IPT,ISTAT )*RSCALE
      ENDDO
c     print *, 'RNORMZ = ', RNORMZ
      IF ( RNORMZ.LT.RLOW ) RETURN
      RNORMZ = SQRT( RNORMZ )
C ... calculate the product
      DO IPT = IPTST, ILAST
        RPROD  = RPROD + RRCOMP( IPT )*RZCOMP( IPT,ISTAT )*RSCALE
      ENDDO
c     print *, 'RPROD  = ', RPROD 
      RCC    = RPROD/(RNORMR*RNORMZ)
      RCABSC = RCC*ABS( RCC )
      RETURN
      END
C
C ====================================================================
C ADD2ST reads in the one-dimensional arrays of data for Z, H1, and H2
C and enters them into the place for station ISTAT as Z, N, E
C Note that we fill the arrays using the "reduced dimensions"
C (NPTS,NSTAT) and so we must always reference them from subroutines
C that define them with the same dimensions.
C
      SUBROUTINE ADD2ST( IERR, NPTS, NSTAT, ISTAT, RAZIH1, RAZIH2,
     1                   RZVALS, R1VALS, R2VALS,
     2                   RZCOMP, RNCOMP, RECOMP )
      IMPLICIT NONE
C
      INTEGER     IERR
      INTEGER     NPTS
      INTEGER     NSTAT
      INTEGER     ISTAT
C
      REAL*4      RAZIH1
      REAL*4      RAZIH2
C
      REAL*4      RZVALS( NPTS )
      REAL*4      R1VALS( NPTS )
      REAL*4      R2VALS( NPTS )
      REAL*4      RZCOMP( NPTS, NSTAT )
      REAL*4      RNCOMP( NPTS, NSTAT )
      REAL*4      RECOMP( NPTS, NSTAT )
C
      INTEGER     IPTS
      REAL*8      DPI
      PARAMETER ( DPI    = 3.14159265358979d0 )
      REAL*8      DEGRAD
      PARAMETER ( DEGRAD = DPI/180.0d0 )
      REAL*8      DAZI1R
      REAL*8      DAZI2R
      REAL*8      DCOSA1
      REAL*8      DSINA1
      REAL*8      DCOSA2
      REAL*8      DSINA2
      REAL*4      RCOSA1
      REAL*4      RSINA1
      REAL*4      RCOSA2
      REAL*4      RSINA2
C
      IERR  = 0
      IF ( ISTAT.LT.1 .OR. ISTAT.GT.NSTAT ) THEN
        IERR   = 1
        RETURN
      ENDIF
C
      DAZI1R = DBLE( RAZIH1 )*DEGRAD
      DAZI2R = DBLE( RAZIH2 )*DEGRAD
C
      DCOSA1 = DCOS( DAZI1R )
      DSINA1 = DSIN( DAZI1R )
      DCOSA2 = DCOS( DAZI2R )
      DSINA2 = DSIN( DAZI2R )
C
      RCOSA1 = REAL( DCOSA1 )
      RSINA1 = REAL( DSINA1 )
      RCOSA2 = REAL( DCOSA2 )
      RSINA2 = REAL( DSINA2 )
C
C First do the Z-component
C
      DO IPTS = 1, NPTS
        RZCOMP( IPTS, ISTAT ) = RZVALS( IPTS )
      ENDDO
C
C Now add the contribution from H1 to HN
C
      DO IPTS = 1, NPTS
        RNCOMP( IPTS, ISTAT ) = R1VALS( IPTS )*RCOSA1
      ENDDO
C
C Now add the contribution from H2 to HN
C
      DO IPTS = 1, NPTS
        RNCOMP( IPTS, ISTAT ) = RNCOMP( IPTS, ISTAT ) +
     1                            R2VALS( IPTS )*RCOSA2
      ENDDO
C
C Now add the contribution from H1 to HE
C
      DO IPTS = 1, NPTS
        RECOMP( IPTS, ISTAT ) = R1VALS( IPTS )*RSINA1
      ENDDO
C
C Now add the contribution from H2 to HE
C
      DO IPTS = 1, NPTS
        RECOMP( IPTS, ISTAT ) = RECOMP( IPTS, ISTAT ) +
     1                            R2VALS( IPTS )*RSINA2
      ENDDO
C
      RETURN
      END
C

C*********************************************************************
C Subroutine Character String ARGument Map ***************************
C            -         -      ---      -   ***************************
C Steve Gibbons August 2001 (University of Leeds)                    C
C____________________________________________________________________C
C                                                                    C
C Takes in a character string, LINE, a number of arguments, NARGS,   C
C and a maximum line length. Assuming the arguments to be separated  C
C by spaces, CSARGM will fill the integer array IARGL with the       C
C character positions of the start and end of the argument.          C
C                                                                    C
C____________________________________________________________________C
C                                                                    C
C Input Variables :-                                                 C
C ===============                                                    C
C                                                                    C
C  Character                                                         C
C  ---------                                                         C
C                                                                    C
C     LINE      : String (*) containing input arguments.             C
C                                                                    C
C  Integer                                                           C
C  -------                                                           C
C                                                                    C
C     NARGS     : Number of arguments to be searched for.            C
C     NARGM     : Maximum no. of arguments (i.e. dimension for IARGL C
C     CSLEN     : Length of character string.                        C
C                                                                    C
C Output Variables :-                                                C
C ================                                                   C
C                                                                    C
C     IARGL     : Array dim ( NARGM, 2 ).                            C
C                 On exit, IARGL( I, 1 ) contains the character      C
C                 position where argument I begins: IARGL( I, 2 )    C
C                 contains the character position where arg I ends.  C
C                                                                    C
C     IERR      : =0 all NARGS are found and located.                C
C                 >0 CSLEN was exceeded with only IERR arguments     C
C                 being located.                                     C
C                 IERR = -1 means that no arguments were found.      C
C                 IERR = -2 means that the last argument was not     C
C                 terminated, indicating a possible string longer    C
C                 than was anticipated.                              C
C____________________________________________________________________C
C
C*********************************************************************
      SUBROUTINE CSARGM( LINE, NARGS, NARGM, CSLEN, IARGL, IERR )
      IMPLICIT NONE
C____________________________________________________________________C
C Variable declarations - Parameters ................................C
C
      CHARACTER *(*) LINE
      INTEGER        NARGS, NARGM, CSLEN, IARGL( NARGM, 2 ), IERR
C____________________________________________________________________C
C Variable declarations - Working variables .........................C
C
      INTEGER        I, IARGF
      LOGICAL        OWORDP, OWORDC
C_____________________________________________________________________
C                  **************************************************C
C START OF PROGRAM **************************************************C
C____________________________________________________________________C
C
      IF ( NARGS.LT.0 .OR. NARGS.GT.NARGM ) THEN
        PRINT *,' Subroutine CSARGM.'
        PRINT *,' NARGS = ', NARGS
        PRINT *,' NARGM = ', NARGM
        PRINT *,' Program aborted.'
        STOP
      ENDIF
C
      DO IARGF = 1, NARGM
        IARGL( IARGF, 1 ) = -1
        IARGL( IARGF, 2 ) = -1
      ENDDO
C
      IARGF  = 0
      OWORDP = .FALSE.
      OWORDC = .FALSE.
      DO I = 1, CSLEN
        IF ( LINE(I:I).EQ.' ' ) THEN
          OWORDC = .FALSE.
        ELSE
          OWORDC = .TRUE.
        ENDIF
        IF ( I.EQ.1 ) OWORDP = .FALSE.
C
C Treat the case of us starting a new word
C
        IF ( .NOT. OWORDP .AND. OWORDC ) THEN
          IARGF = IARGF + 1
          IARGL( IARGF, 1 ) = I
        ENDIF
C
C Treat the case of us ending a word
C
        IF ( OWORDP .AND. .NOT. OWORDC ) IARGL( IARGF, 2 ) = I - 1
C
        IF ( IARGF.EQ.NARGS .AND. IARGL( IARGF, 2 ).GT.0 ) THEN
          IERR = 0
          RETURN
        ENDIF
C
        OWORDP = OWORDC
C
      ENDDO
C
      IF ( IARGF.EQ.0 ) THEN
        IERR = -1
        RETURN
      ENDIF
C
      IF ( IARGF.LT.NARGS ) IERR = IARGF
      IF ( IARGF.EQ.NARGS .AND. IARGL( IARGF, 2 ).LT.0 ) THEN
        IERR = -2
        IARGL( IARGF, 2 ) = CSLEN
      ENDIF
C
      RETURN
      END
C*********************************************************************
C OPAZIF - Find the optimal backazimuth values,
C
      SUBROUTINE OPAZIF( IERR, NAZIB2, NBOX, NSTAT,
     1                   RVALUE, RVALUE_avg,
     2                   OPAZIM, OPAZIM_avg,
     3                   OPVALU, OPVALU_avg,
     4                   RWORK )
      IMPLICIT NONE
C
      INTEGER            IERR
      INTEGER            NAZIB2
      INTEGER            NBOX
      INTEGER            NSTAT
C
      REAL*4             RVALUE( NAZIB2*2, NBOX, NSTAT )
      REAL*4             RVALUE_avg( NAZIB2*2, NBOX )
      REAL*4             OPAZIM( NBOX, NSTAT )
      REAL*4             OPAZIM_avg( NBOX )
      REAL*4             OPVALU( NBOX, NSTAT )
      REAL*4             OPVALU_avg( NBOX )
      REAL*4             RWORK( NAZIB2*2 )
C
      INTEGER            IBOX
      INTEGER            ISTAT
C
      REAL*8             DXVALS( 5 )
      REAL*8             DYVALS( 5 )
      REAL*8             DAZOPT
      REAL*8             DVLOPT
C
      IERR   = 0
C
C First loop around stations
C
      DO ISTAT = 1, NSTAT
C       .
C       . Now loop around "boxes" from 1 to NBOX
C       .
        DO IBOX = 1, NBOX
          CALL V2WORK( IERR, NAZIB2, NBOX, NSTAT, ISTAT, IBOX,
     1                 RVALUE, RVALUE_avg, RWORK )
C         . RWORK now contains the values as a function of azimuth only
C         . Now fill DXVALS and DYVALS with double precision copies
C         . of azimuth and values centered on the optimal
          CALL RVDXDY( IERR, NAZIB2, RWORK, DXVALS, DYVALS )
C         .
          CALL DXDY2O( DXVALS, DYVALS, DAZOPT, DVLOPT )
          IF ( DAZOPT.LT.0.0d0   ) DAZOPT = DAZOPT + 360.0d0
          IF ( DAZOPT.GE.360.0d0 ) DAZOPT = DAZOPT - 360.0d0
C         .
          OPAZIM( IBOX, ISTAT ) = REAL( DAZOPT )
          OPVALU( IBOX, ISTAT ) = REAL( DVLOPT )
C         .
        ENDDO
C       .
      ENDDO
C
C If there is only one station then we trivially copy the
C values into the avg arrays
C
      IF ( NSTAT.EQ.1 ) THEN
        DO IBOX = 1, NBOX
          OPAZIM_avg( IBOX ) = OPAZIM( IBOX, NSTAT )
        ENDDO
        DO IBOX = 1, NBOX
          OPVALU_avg( IBOX ) = OPVALU( IBOX, NSTAT )
        ENDDO
        RETURN
      ENDIF
C
C OK - so there are more than one station and we have to
C repeat the procedure for the beam
C
      ISTAT = 0
      DO IBOX = 1, NBOX
        CALL V2WORK( IERR, NAZIB2, NBOX, NSTAT, ISTAT, IBOX,
     1               RVALUE, RVALUE_avg, RWORK )
C       . RWORK now contains the values as a function of azimuth only
C       . Now fill DXVALS and DYVALS with double precision copies
C       . of azimuth and values centered on the optimal
        CALL RVDXDY( IERR, NAZIB2, RWORK, DXVALS, DYVALS )
C       .
        CALL DXDY2O( DXVALS, DYVALS, DAZOPT, DVLOPT )
        IF ( DAZOPT.LT.0.0d0   ) DAZOPT = DAZOPT + 360.0d0
        IF ( DAZOPT.GE.360.0d0 ) DAZOPT = DAZOPT - 360.0d0
C       .
        OPAZIM_avg( IBOX ) = REAL( DAZOPT )
        OPVALU_avg( IBOX ) = REAL( DVLOPT )
C       .
      ENDDO
C
      RETURN
      END
C
C -------------------------------
C "trivial" subroutine which puts the right box for the right
C station into the work array. Specify ISTAT = 0 for the average.
C -------------------------------
      SUBROUTINE V2WORK( IERR, NAZIB2, NBOX, NSTAT, ISTAT, IBOX,
     1                   RVALUE, RVALUE_avg, RWORK )
      IMPLICIT NONE
C
      INTEGER            IERR
      INTEGER            NAZIB2
      INTEGER            NBOX
      INTEGER            NSTAT
      INTEGER            IBOX
      INTEGER            ISTAT
      REAL*4             RVALUE( NAZIB2*2, NBOX, NSTAT )
      REAL*4             RVALUE_avg( NAZIB2*2, NBOX )
      REAL*4             RWORK( NAZIB2*2 )
C
      INTEGER            NAZIM
      INTEGER            IAZI
C
      IERR   = 0
      IF ( IBOX.LT.1 .OR. IBOX.GT.NBOX ) THEN
        WRITE (6,*) 'Subroutine V2WORK:'
        WRITE (6,*) 'IBOX   = ', IBOX
        WRITE (6,*) 'NBOX   = ', NBOX
        IERR   = 1
        RETURN
      ENDIF
      IF ( ISTAT.LT.0 .OR. ISTAT.GT.NSTAT ) THEN
        WRITE (6,*) 'Subroutine V2WORK:'
        WRITE (6,*) 'ISTAT  = ', ISTAT
        WRITE (6,*) 'NSTAT  = ', NSTAT
        IERR   = 1
        RETURN
      ENDIF
C
      NAZIM  = 2*NAZIB2
      IF ( ISTAT.EQ.0 ) THEN
        DO IAZI = 1, NAZIM
          RWORK( IAZI ) = RVALUE_avg( IAZI, IBOX )
        ENDDO
      ELSE
        DO IAZI = 1, NAZIM
          RWORK( IAZI ) = RVALUE( IAZI, IBOX, ISTAT )
        ENDDO
      ENDIF
C
      RETURN
      END
C
C Subroutine RVDXDY( IERR, NAZIB2, RWORK, DXVALS, DYVALS )
C puts X (backazimuth) and Y (values)  values into double precision
C arrays DXVALS and DYVALS
C
      SUBROUTINE RVDXDY( IERR, NAZIB2, RWORK, DXVALS, DYVALS )
      IMPLICIT NONE
C
      INTEGER            IERR
      INTEGER            NAZIB2
      REAL*4             RWORK( 2*NAZIB2 )
      REAL*8             DXVALS( 5 )
      REAL*8             DYVALS( 5 )
C
      INTEGER            NAZIM
      INTEGER            IAZ
      INTEGER            IOPTAZ
      REAL*4             ROPTVL
      REAL*4             RDELAZ
      REAL*8             DDELAZ
C
      IERR   = 0
      IF ( NAZIB2.LT.4 ) THEN
        IERR = 1
        RETURN
      ENDIF
C
      NAZIM  = 2*NAZIB2
      RDELAZ = 360.0/REAL( NAZIM )
      DDELAZ = DBLE( RDELAZ )
C
C Find IOPTVL
C
      ROPTVL = RWORK( 1 )
      IOPTAZ = 1
      DO IAZ = 2, NAZIM
        IF ( RWORK( IAZ ).GT.ROPTVL ) THEN
          ROPTVL = RWORK( IAZ )
          IOPTAZ = IAZ
        ENDIF
      ENDDO
C
C We can now fill DXVALS
C
      DXVALS( 3 ) = DBLE( IOPTAZ-1 )*DDELAZ
      DXVALS( 2 ) = DXVALS( 3 )  - DDELAZ
      DXVALS( 1 ) = DXVALS( 2 )  - DDELAZ
      DXVALS( 4 ) = DXVALS( 3 )  + DDELAZ
      DXVALS( 5 ) = DXVALS( 4 )  + DDELAZ
C
      DYVALS( 3 ) = DBLE( ROPTVL )
C
C We now cover the special cases at the edges
C
      IF ( IOPTAZ.EQ.1 ) THEN
        DYVALS( 1 ) = DBLE( RWORK( NAZIM-1 ) )
        DYVALS( 2 ) = DBLE( RWORK( NAZIM   ) )
        DYVALS( 4 ) = DBLE( RWORK( 2       ) )
        DYVALS( 5 ) = DBLE( RWORK( 3       ) )
        RETURN
      ENDIF
      IF ( IOPTAZ.EQ.2 ) THEN
        DYVALS( 1 ) = DBLE( RWORK( NAZIM   ) )
        DYVALS( 2 ) = DBLE( RWORK( 1       ) )
        DYVALS( 4 ) = DBLE( RWORK( 3       ) )
        DYVALS( 5 ) = DBLE( RWORK( 4       ) )
        RETURN
      ENDIF
      IF ( IOPTAZ.EQ.NAZIM ) THEN   
        DYVALS( 1 ) = DBLE( RWORK( NAZIM-2 ) )
        DYVALS( 2 ) = DBLE( RWORK( NAZIM-1 ) )
        DYVALS( 4 ) = DBLE( RWORK( 1       ) )
        DYVALS( 5 ) = DBLE( RWORK( 2       ) )
        RETURN
      ENDIF
      IF ( IOPTAZ.EQ.(NAZIM-1) ) THEN
        DYVALS( 1 ) = DBLE( RWORK( NAZIM-3 ) )
        DYVALS( 2 ) = DBLE( RWORK( NAZIM-2 ) )
        DYVALS( 4 ) = DBLE( RWORK( NAZIM   ) )
        DYVALS( 5 ) = DBLE( RWORK( 1       ) )
        RETURN
      ENDIF
C     .
C     . OK - so the optimum value is in the middle
C     .
      DYVALS( 1 ) = DBLE( RWORK( IOPTAZ-2 ) )
      DYVALS( 2 ) = DBLE( RWORK( IOPTAZ-1 ) )
      DYVALS( 4 ) = DBLE( RWORK( IOPTAZ+1 ) )
      DYVALS( 5 ) = DBLE( RWORK( IOPTAZ+2 ) )
C
      RETURN
      END
C
C DXDY2O takes the REAL*8 arrays DXVALS, DYVALS
C and estimates the exact value DXOPT at which the maximum
C DYOPT is found ...
C It is assumed that DYVALS( 3 ) is the maximum of the 5 values
C and that the function in DYVALS varies smoothly such that the
C maximum value is where the first derivative becomes zero.
C It is assumed that the values in DXVALS increase strictly monotonically.
C
C DXOPT and DYOPT default to the values DXVALS(3) and DYVALS(3)
C
C If the procedure to refine fails then we just return these
C values rather than an error message.
C
      SUBROUTINE DXDY2O( DXVALS, DYVALS, DXOPT, DYOPT )
      IMPLICIT NONE
C
      REAL*8             DXVALS( 5 )
      REAL*8             DYVALS( 5 )
      REAL*8             DXOPT
      REAL*8             DYOPT
C
      INTEGER            NNDS
      INTEGER            NCFM
      PARAMETER        ( NCFM = 5, NNDS = 5 )
      REAL*8             COEFM( NCFM, NCFM )
      REAL*8             WORK( NCFM )
      INTEGER            IPCM( NCFM )
C
      INTEGER            J
      REAL*8             DX1
      REAL*8             DX2
      REAL*8             DYDX1
      REAL*8             DYDX2
      REAL*8             DLOW
      PARAMETER        ( DLOW = 1.0d-7 )
      REAL*8             DM
      REAL*8             DC
      REAL*8             DXPROV
C
      DXOPT   = DXVALS( 3 )
      DYOPT   = DYVALS( 3 )
C
C Set DX1 to be halfway between DXVALS( 2 ) and DXVALS( 3 )
C Set DX2 to be halfway between DXVALS( 3 ) and DXVALS( 4 )
C
      DX1     = 0.5d0*( DXVALS( 2 ) + DXVALS( 3 ) )
      DX2     = 0.5d0*( DXVALS( 3 ) + DXVALS( 4 ) )
      IF ( DABS( DX2-DX1 ).LT.DLOW ) RETURN
C
      CALL GFDCFD( DX1, DXVALS, NNDS, COEFM, NCFM, IPCM, WORK)
      DYDX1   = 0.0d0
      DO J = 1, NNDS
        DYDX1   = DYDX1 + COEFM( 2, J )*DYVALS( J )
      ENDDO
C
      CALL GFDCFD( DX2, DXVALS, NNDS, COEFM, NCFM, IPCM, WORK)
      DYDX2   = 0.0d0
      DO J = 1, NNDS
        DYDX2   = DYDX2 + COEFM( 2, J )*DYVALS( J )
      ENDDO
C
C We assume the first derivative to vary as a straight line
C around the points DX1 and DX2. Assume the form "y = mx + c"
C Our optimal x is where y = 0
C
      DM     = (DYDX1-DYDX2)/(DX1-DX2)
C     . If the line is flat we will never cross zero
      IF ( DABS( DM ).LT.DLOW ) RETURN
      DC     = DYDX1 - DM*DX1
C
      DXPROV = -DC/DM
C     .
C     . DXPROV now contains our guess at the azimuth for which the
C     . derivative is zero. If it is not between DXVALS(2) and DXVALS(4)
C     . then this contradicts our assumption about DXVALS(3) being the
C     . optimal with DYVALS well-behaved.
C     .
      IF ( DXPROV.LE.DXVALS( 2 ) ) RETURN
      IF ( DXPROV.GE.DXVALS( 4 ) ) RETURN
C     .
      DXOPT  = DXPROV
      CALL GFDCFD( DXOPT, DXVALS, NNDS, COEFM, NCFM, IPCM, WORK)
      DYOPT  = 0.0d0
      DO J = 1, NNDS
        DYOPT   = DYOPT + COEFM( 1, J )*DYVALS( J )
      ENDDO
C
      RETURN
      END
C
C*********************************************************************
C subroutine General Finite Difference Coefficient Find **************
C            -       -      -          -           -    **************
C Steve Gibbons Mon Sep 20 16:57:54 BST 1999                         C
C____________________________________________________________________C
C                                                                    C
C Given a value of X and the values of x_i at NNDS distinct points,  C
C (X need not necessarily be one of the x_i) then the array COEFM    C
C is returned with the finite difference coefficients such that      C
C the ND^{th} derivative of a function f, evaluated at x = X,        C
C is given by                                                        C
C                                                                    C
C f^{ ND }( X ) = \sum_{j = 1, NNDS} COEFM( ND + 1, j )*f( x_j )     C
C                                                                    C
C This is a general version of the routine FDCINV which is valid     C
C only for equally spaced grid nodes.                                C
C                                                                    C
C Coefficients for up to the (NNDS-1)^{th} derivative are given      C
C although care must be taken to ensure the highest derivatives      C
C are sufficiently accurate.                                         C
C                                                                    C
C____________________________________________________________________C
C                                                                    C
C Input variables :-                                                 C
C ===============                                                    C
C  Double Precision                                                  C
C  ----------------                                                  C
C     X         : Abscissa at which derivatives are to be            C
C                  evaluated.                                        C
C     XARR      : Array of dimension ( NNDS ).                       C
C                  XARR( i ) contains the value of x/r at the        C
C                   i^{th} grid node.                                C
C                                                                    C
C     COEFM     : Dimension ( NCFM, NCFM).                           C
C     WORK      : Workspace array for LAPACK inversion routine.      C
C                 Dimension ( NCFM )                                 C
C                                                                    C
C  Integer                                                           C
C  -------                                                           C
C                                                                    C
C     NNDS      : Number of grid nodes.                              C
C     NCFM      : Leading order of coefficient matrix.               C
C                                                                    C
C     IPCM      : Work array for LAPACK routines to perform          C
C                 pivotting in the matrix inversion.                 C
C                 Dimension ( NCFM )                                 C
C                                                                    C
C____________________________________________________________________C
C
C*********************************************************************
      SUBROUTINE GFDCFD ( X, XARR, NNDS, COEFM, NCFM, IPCM, WORK)
      IMPLICIT NONE
C____________________________________________________________________C
C Variable declarations - Parameters ................................C
      INTEGER NNDS, NCFM, IPCM( NCFM )
      DOUBLE PRECISION X, COEFM( NCFM, NCFM ), WORK( NCFM ),
     1                 XARR( NNDS )
C____________________________________________________________________C
C Variable declarations - Working variables .........................C
C
      INTEGER I, J, NDER, INODE, INFO, ICOL, IROW
      DOUBLE PRECISION DZERO, LOW, FAC
      PARAMETER ( DZERO = 0.0d0, LOW = 1.0d-8 )
C____________________________________________________________________C
C START OF PROGRAM **************************************************C
C____________________________________________________________________C
C
C Check bounds of integer parameters
C
      IF ( NNDS.GT.NCFM ) THEN
         PRINT *,' Subroutine GFDCFD: '
         PRINT *,' NNDS = ', NNDS,'. NCFM = ', NCFM
         PRINT *,' Program aborted.'
         STOP
      ENDIF
C
C Calculate the distances h_i = ( x_i - X )
C These h_i must be distinct and this will be
C checked for - otherwise matrix is singular.
C We can store the h_i in WORK as this will not
C be needed until the inversion, by which time
C it will not be needed by us.
C
      DO I = 1, NNDS
        WORK( I ) = XARR( I ) - X
      ENDDO
C
C Now check for the uniqueness of the points ...
C
      DO I = 1, NNDS - 1
        DO J = I + 1, NNDS
          IF ( ABS( WORK( I ) - WORK( J ) ).LT.LOW ) THEN
            PRINT *,' Subroutine GFDCFD.'
            PRINT *,' X values ',I,' and ',J,' are'
            PRINT *,' identical.'
            PRINT *,' Program aborted.'
            STOP
          ENDIF
        ENDDO
      ENDDO
C
C____________________________________________________________________C
C Parameters are ok so let's zero COEFM
C
      I = 0
      CALL MATOP( COEFM, DZERO, NCFM, NCFM, I )
C
C (nder+1) is the number of the matrix column being filled in.
C inode is the number of the matrix row being filled in.
C
      DO NDER = 0, NNDS - 1
        ICOL = NDER + 1
        DO INODE = 1, NNDS
          IROW = INODE
          IF ( NDER.EQ.0 ) THEN
            COEFM( IROW, ICOL )  = 1.0d0
          ELSE
            FAC = WORK( INODE )/DBLE( NDER )
            COEFM( IROW, ICOL ) = COEFM( IROW, ICOL-1 )*FAC
          ENDIF
        ENDDO
      ENDDO
C
C Ok - this matrix is now ready for inversion -
C For this we use the LAPACK routines DGETRF and DGETRI
C First perform LU decomposition
C
      CALL DGETRF( NNDS, NNDS, COEFM, NCFM, IPCM, INFO )
C
C     . Check that LU decomposition has gone without
C     . problem.
C     .
C
      IF ( INFO.NE.0 ) THEN
         PRINT *,' Subroutine GFDCFD.'
         PRINT *,' The LAPACK subroutine DGETRF has'
         PRINT *,' returned ',INFO,' as a value of '
         PRINT *,' INFO in LU decomposition of COEFM matrix.'
         PRINT *,' Program aborted.'
         STOP
      ENDIF
C     .
C     . Now compute the inverse with the LAPACK routine
C     . DGETRI.
C     .
      CALL DGETRI( NNDS, COEFM, NCFM, IPCM, WORK, NCFM, INFO )
C     .
C     . Check that inversion has gone without problem.
C     .
      IF ( INFO.NE.0 ) THEN
         PRINT *,' Subroutine GFDCFD.'
         PRINT *,' The LAPACK subroutine DGETRI has'
         PRINT *,' returned ',INFO,' as a value of '
         PRINT *,' INFO in inversion of COEFM matrix.'
         PRINT *,' Program aborted.'
         STOP
      ENDIF
C     .
      RETURN
      END
C*********************************************************************
C*********************************************************************
C subroutine MATrix OPeration ****************************************
C Steve Gibbons 23.4.97 Does operation on a two-dimensional array.   C
C                                                Can set equal to a  C
C                       constant; multiply by a constant or have a   C
C                       constant added to it.                        C
C____________________________________________________________________C
C Input variables :-                                                 C
C ===============                                                    C
C  Integer                                                           C
C  -------                                                           C
C     IOP	: Type of operation to be done.                      C
C                    WHOLE MATRIX OPERATIONS                         C
C                  IOP=0  -->  Each element of the matrix = CONST    C
C                  IOP=1  -->  Each el. is multiplied by CONST       C
C                  IOP=2  -->  Each el. is added to CONST            C
C     NDIM1     : First dimension of the matrix.	             C
C     NDIM2     : Second dimension of the matrix.	             C
C  Double Precision                                                  C
C  ----------------                                                  C
C     MAT	: Matrix with dimension ( NDIM1, NDIM2 )             C
C     CONST     : Double precision constant.                         C
C____________________________________________________________________C
C
C*********************************************************************
      SUBROUTINE MATOP ( MAT, CONST, NDIM1, NDIM2, IOP)
      IMPLICIT NONE
C____________________________________________________________________C
C Variable declarations - Parameters ................................C
      INTEGER NDIM1, NDIM2, IOP
      DOUBLE PRECISION MAT ( NDIM1, NDIM2 ), CONST
C____________________________________________________________________C
C Variable declarations - Working variables .........................C
      INTEGER I, J
C____________________________________________________________________C
C START OF PROGRAM **************************************************C
C____________________________________________________________________C
C
C First do case IOP=0 
      IF ( IOP.EQ.0 ) THEN
         DO J = 1, NDIM2
            DO I = 1, NDIM1
               MAT ( I, J) = CONST
            ENDDO
         ENDDO
         RETURN
      ENDIF
C Now do case IOP=1
      IF ( IOP.EQ.1 ) THEN
         DO J = 1, NDIM2
            DO I = 1, NDIM1
               MAT ( I, J) = MAT ( I, J)*CONST
            ENDDO
         ENDDO
         RETURN
      ENDIF
C Now do case IOP=2
      IF ( IOP.EQ.2 ) THEN
         DO J = 1, NDIM2
            DO I = 1, NDIM1
               MAT ( I, J) = MAT ( I, J) + CONST
            ENDDO
         ENDDO
         RETURN
      ENDIF
      PRINT *,' Subroutine MATOP. IOP must be 0,1 or 2.'
      STOP
      END
C*********************************************************************
