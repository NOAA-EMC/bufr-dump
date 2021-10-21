C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_DCODWINDSAT
C   PRGMMR: DONG             ORG: NP22        DATE: 2020-08-20
C
C ABSTRACT: REPROCESSES WINDSAT (SCATTEROMETER) DATA.  READS EACH
C   REPORT FROM INPUT BUFR DATA DUMP FILE (FROM EITHER THE NAVY OR
C   NESDIS) AND PERFORMS A NUMBER OF CHECKS, INCLUDING: REPORT DATE
C   CHECKED FOR REALISM, PROPER WIND SET SELECTED (FROM CHOICE OF 4),
C   REPORTS NOT EXPLICITLY OVER OCEAN (BASED ON SURFACE TYPE) SKIPPED,
C   REPORTS OVER LAND (BASED ON LAND-SEA MASK) SKIPPED, REPORTS WITH
C   MISSING SELECTED WIND VECTOR SKIPPED, REPORTS WITH MISSING MODEL
C   WIND DIRECTION AND SPEED SKIPPED, REPORTS WITH "BAD" OR "NO
C   RETRIEVAL" EDR QUALITY FLAG SKIPPED.  REPORTS PASSING CHECKS ARE
C   SUPEROBED ONTO A USER-SPECIFIED LAT/LON GRID ACCORDING TO SATELLITE
C   ID (OPTIONAL) AND THEN REPROCESSED INTO A BUFR FILE (WITH THE SAME
C   MESSAGE TYPE AS THE INPUT BUFR FILE) WHICH WILL LATER BE READ BY
C   THE PROGRAM PREPOBS_PREPDATA.  ALSO, A UNIQUE ID IS GENERATED HERE
C   FOR EACH REPORT AND THEN ENCODED INTO THE OUTPUT REPROCESSED BUFR
C   FILE.  THE REPROCESSED BUFR FILE CONTAINS ONLY THOSE DATA NEEDED
C   FOR PREPBUFR PROCESSING AND LATER ASSIMILATION INTO THE NCEP
C   ANALYSES.  A USER-SPECIFIED SWITCH ALLOWS REPORTS TO BE SELECTED BY
C   LOCATION (LAT/LON BOUNDARY).
C
C PROGRAM HISTORY LOG:
C
C 2007-05-25 D. KEYSER  - ORIGINAL AUTHOR (ADAPTED FROM
C            WAVE_DCODQUIKSCAT)
C 2011-08-04 D. KEYSER  - NOW STOPS WITH ABNORMAL R.C. 55 IF LAND-SEA
C            MASK FILE IS EMPTY OR INCOMPLETE (IN ADDITION TO READ
C            ERROR); IN RESPONSE TO THE LATEST VERSION OF BUFRLIB WHICH
C            CAN HANDLE EMBEDDED DICTIONARY MESSAGES:: INCREASES DEGREE
C            OF BUFRLIB PRINTOUT SUCH THAT CODE WILL PRINT A DIAGNOSTIC
C            IF ANY EMBEDDED DICTIONARY MESSAGES ARE FOUND WHEN READING
C            IN MESSAGES; FOR NON-SUPEROB PROCESSING ONLY: REPLACES
C            CALL TO OPENMG (WHICH FORCED THE SAME CENTER BUFR DUMP
C            DATE/HOUR TO BE WRITTEN INTO SEC. 1 OF ALL OUTPUT
C            MESSAGES) WITH CALL TO OPENMB (ALLOWING SEC.1 IN OUTPUT
C            MESSAGES TO HAVE SAME DATE/HOUR AS THAT FROM SEC. 1 OF THE
C            INPUT BUFR MESSAGE FROM WHICH EACH SUBSET IS BEING
C            PROCESSED), THIS FIXES A BUG WHICH HAD CAUSED A BUFRLIB
C            ABORT HERE WHEN THE INPUT BUFR FILE CONTAINS EMBEDDED BUFR
C            DICTIONARY MESSAGES, IT ALSO FORCES OUTPUT FILE TO NOW
C            COMPLY WITH NCEP BUFR STANDARD THAT ALL SUBSETS IN A BUFR
C            MESSAGE CONTAIN THE SAME YEAR, MONTH, DAY AND HOUR AS THAT
C            IN SEC. 1 OF THE BUFR MESSAGE; FOR SUPEROB PROCESSING
C            ONLY: MOVES CALL TO OPENMG FROM PRIOR TO READING OF INPUT
C            BUFR FILE TO AFTER READING OF INPUT BUFR FILE BUT PRIOR TO
C            WRITING OF OUTPUT BUFR FILE, THIS PREVENTS A BUFRLIB ABORT
C            WHEN THE INPUT BUFR FILE CONTAINS EMBEDDED BUFR DICTIONARY
C            MESSAGES (NOT SURE WHY!) (NOTE: WE USE OPENMG HERE RATHER
C            THAN OPENMB BECAUSE SUPEROBS ARE GENERATED AND ENCODED
C            INTO THE OUTPUT BUFR FILE ONLY AFTER THE ENTIRE INPUT BUFR
C            FILE IS READ - THE DATE IN SEC. 1 OF ALL BUFR MESSAGES IN
C            THE OUTPUT SUPEROB FILE IS SIMPLY THE DUMP CENTER DATE/
C            HOUR, ALTHOUGH THIS VIOLATES THE NCEP BUFR STANDARD THAT
C            ALL SUBSETS IN A BUFR MESSAGE CONTAIN THE SAME YEAR,
C            MONTH, DAY AND HOUR AS THAT IN SEC. 1 OF THE BUFR MESSAGE,
C            IT KEEPS THE OUTPUT FILE MORE COMPACT AND CAUSES NO HARM
C            SINCE ONLY PREPOBS_PREPDATA ENDS UP READING THIS FILE)
C 2012-11-20 J. WOOLLEN  INITIAL PORT TO WCOSS 
C 2020-08-20 J. DONG  - CHANGED MISSING VALUE TO 10E8 FROM 10E10.
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - INPUT DATA CARDS IN THE FORM OF A NAMELIST (SEE
C              - REMARKS)
C     UNIT 11  - NCEP BUFR DATA DUMP CONTAINING ORIGINAL FORM OF
C                WINDSAT DATA ("WNDSAT")
C     UNIT 19  - 0.5 x 0.5 DEG LAT/LON LAND/SEA MASK (INTEGER(4))
C     UNIT 20  - BUFR MNEMONIC TABLE (NEEDED TO PRODUCE NCEP BUFR FILE)
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 51  - TEXT LISTING OF ALL REPORTS ENCODED TO UNIT 52
C              - (GENERATED ONLY WHEN INPUT NAMELIST VARIABLE "IPRINT"
C              - IS 1)
C     UNIT 52  - NCEP BUFR DATA DUMP CONTAINING FINAL (REPROCESSED)
C                FORM OF WINDSAT DATA ("WDSATR") WITH UNIQUE REPORT IDS
C                (AND POSSIBLY SUPEROBED)
C
C   SUBPROGRAMS CALLED:
C     LIBRARY
C       W3LIB    - W3TAGB   W3TAGE   ERREXIT  W3FC05   W3FC06
C       BUFRLIB  - DATELEN  DUMPBF   OPENBF   OPENMG   MINIMG  CLOSMG
C                - READMG   IREADMG  IREADSB  UFBINT   WRITSB  UFBCNT
C                - CLOSBF   OPENMB
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =  55 - ERROR READING LAND/SEA MASK (OR FILE EMPTY OR
C                  INCOMPLETE)
C          =  61 - ERROR OBTAINING CENTER DATE FROM FIRST BUFR MESSAGE
C                  IN UNIT 11
C          =  62 - ERROR OBTAINING DUMP DATE FROM SECOND BUFR MESSAGE
C                  IN UNIT 11
C
C REMARKS: 
C
C     Contents of input BUFR file:
C
C     DATEIN_8 contains report year, month, day, hour, minute, second.
C      DATA DATSTR/'YEAR MNTH DAYS HOUR MINU SECO '/
C
C     WSTDAT_8 contains WindSat surface type, sea surface temperature
C              retrieval, rainfall rate, total water vapor retrieval,
C              total cloud liquid water retrieval, model wind direction
C              at 10 m, model wind speed at 10m, and WindSat EDR q.c.
C              flag-1.  Prior to 2006, the model here was the GFS.  In
C              2006 the model here will be the Navy NOGAPS.
C      DATA WSTSTR/'WSST SST1 REQV MRWVC MRLWC MWD10 MWS10 WSEQC1 '/
C
C     ERRDAT_8 contains the estimated retrieval error covariances in
C              sea surface temp, wind speed, total water vapor and
C              total cloud liquid water.
C      DATA ERRSTR/'SSTE SPDE VPRE CLDE '/
C
C     XLOCDT_8 contains the report latitude and longitude.
C      DATA LOCSTR /'CLAT CLON '/
C
C     WINDAT_8 contains the four sets of wind information,
C              corresponding to the four "views".  Each set comprises:
C              retrieved wind direction at 10 m, retrieved wind speed
C              at 10 m, chi-squared of the wind vector retrieval, and
C              estimated wind direction retrieval error covariance.
C      DATA SCWSTR/'WS10 WD10 CHSQ PHER '/
C
C     ISWVDT_8 contains two replications of selected wind vector index
C              (generated in different ways) which point to which one
C              of the four sets of wind information should be chosen.
C              The values can be either 0, 1, 2 or 3 corresponding to
C              wind set 1, 2, 3 or 4, respectively.  The value in the
C              first replication is equal to the array index of the
C              selected ambiguity as specified in the original WindSat
C              dataset from the Navy.  The value in the second
C              replication is equal to the array index of the selected
C              ambiguity value based on the smallest calculated wind
C              vector error (here vs. the GFS model prior to 2006 and
C              vs. the Navy NOGAPS model in 2006).  The model values
C              used to calculate the wind vector error are encoded here
C              as MWD10 and MWS10.
C      DATA ISWVST/'ISWV '/
C
C     SAIDDT_8 contains the satellite id.
C      DATA SAIDST/'SAID '/
C
C     Contents of output BUFR file:
C
C        ADATA_8(1)  -- REPORT YEAR ("YEAR")
C        ADATA_8(2)  -- REPORT MONTH ("MNTH")
C        ADATA_8(3)  -- REPORT DAY ("DAYS")
C        ADATA_8(4)  -- REPORT HOUR ("HOUR")
C        ADATA_8(5)  -- REPORT MINUTE ("MINU")
C        ADATA_8(6)  -- REPORT SECOND ("SECO")
C        ADATA_8(7)  -- LATITUDE ("CLAT")
C        ADATA_8(8)  -- LONGITUDE ("CLON")
C      * ADATA_8(9)  -- WINDSAT SURFACE TYPE ("WSST")
C      * ADATA_8(10) -- ESTIMATED ERROR COVARIANCE FOR SEA SURFACE
C                       TEMPERATURE RETRIEVAL ("SSTE")
C      * ADATA_8(11) -- ESTIMATED ERROR COVARIANCE FOR WIND SPEED
C                       RETRIEVAL ("SPDE")
C      * ADATA_8(12) -- ESTIMATED ERROR COVARIANCE FOR TOTAL WATER VAPOR
C                       RETRIEVAL ("VPRE")
C      * ADATA_8(13) -- ESTIMATED ERROR COVARIANCE FOR TOTAL CLOUD
C                       LIQUID RETRIEVAL ("CLDE")
C      * ADATA_8(14) -- RETRIEVED SEA SURFACE TEMPERATURE ("SST1")
C      * ADATA_8(15) -- RETRIEVED RAINFALL RATE ("REQV")
C      * ADATA_8(16) -- RETRIEVED TOTAL WATER VAPOR ("MRWVC")
C      * ADATA_8(17) -- RETRIEVED TOTAL CLOUD LIQUID WATER ("MRLWC")
C      * ADATA_8(18) -- MODEL WIND DIRECTION AT 10 M ("MWD10")
C                       (prior to 2006 this was the GFS, in 2006 this
C                        will be the Navy NOGAPS)
C      * ADATA_8(19) -- MODEL WIND SPEED AT 10 M ("MWS10")
C                       (prior to 2006 this was the GFS, in 2006 this
C                        will be the Navy NOGAPS)
C      * ADATA_8(20) -- WINDSAT EDR QC FLAG #1 ("WSEQC1")
C        ADATA_8(21) -- RETRIEVED WIND DIRECTION AT 10 METERS ("WD10")
C                       (based on second replication value of selected
C                        wind vector index)
C        ADATA_8(22) -- RETRIEVED WIND SPEED AT 10 METERS ("WS10")
C                       (based on second replication value of selected
C                        wind vector index)
C      * ADATA_8(23) -- CHI-SQUARED OF THE WIND VECTOR RETRIEVAL
C                       ("CHSQ") (based on second replication value of
C                                 selected wind vector index)
C      * ADATA_8(24) -- ESTIMATED ERROR COVARIANCE FOR WIND DIRECTION
C                       RETRIEVAL ("PHER")
C                       (based on second replication value of selected
C                        wind vector index)
C        ADATA_8(25) -- SATELLITE ID ("SAID")
C      $-ADATA_8(26) -- NUMBER OF OBSERVATIONS THAT WENT INTO MAKING A
C                       SUPEROB ("ACAV")
C      #-ADATA_8(27) -- REPORT ID ("RPID")
C 
C          ALL ABOVE ARE MEANS FOR SUPEROBS
C      * - NOT STORED FOR SUPEROBS
C      $ - STORED AS 1 FOR NON-SUPEROBS
C      # - REPORT ID MADE UP AS FOLLOWS:
C            Superobs:
C              Character 1: Set to "S" to identify superobs
C              Characters 2-7: Index which incrementally counts reports
C            Non-superobs:
C              Characters 1-7: Index which incrementally counts reports
C            Both:
C              Character 8: Indicator for satellite id {"W" - 283
C                           (CORIOLIS)}
C
C
C   VARIABLES READ IN NAMELIST "RDATA":
C
C    IPRINT - IF = 0 (DEFAULT) WILL NOT PRINT LISTING OF ALL
C             PROCESSED REPORTS TO UNIT 51; IF = 1 WILL PRINT
C             LISTING TO UNIT 51
CC
C    ISUPOB - SUPEROB SWITCH
C        ISUPOB = 0 -- SUPEROBS ARE NOT GENERATED; ALL REPROCESSED
C                      REPORTS PASSING CHECKS ARE PACKED INTO OUTPUT
C                      BUFR FILE                               (DEFAULT)
C        ISUPOB = 1 -- SUPEROBS ARE GENERATED BY TAKING LINEAR AVERAGE
C                      OF ALL VALID DATA/TIMES WITHIN DELATxDELON
C                      DEGREE LAT/LON BOXES (SEE BELOW FOR DELAT AND
C                      DELON); SUPEROB IS PLACED AT POINT WITHIN BOX
C                      REPRESENTED BY LINEAR AVERAGED LATITUDE AND
C                      LONGITUDE
C    DELAT   - LATITUDE  SPACING (DEGREES) OF SUPEROB GRID BOX
C        {NOTE: NORMALLY WHOLE DEGREES EXCEPT FOR CHOICE OF 0.5 DEGREES
C                                                          (DEFAULT=1.0)
C    DELON   - LONGITUDE SPACING (DEGREES) OF SUPEROB GRID BOX
C        {NOTE: NORMALLY WHOLE DEGREES EXCEPT FOR CHOICE OF 0.5 DEGREES
C                                                          (DEFAULT=1.0)
C    LIMCNT - LIMITING NUMBER OF INDIVIDUAL REPORTS FOR WHICH A
C             SUPEROB IS GENERATED {I.E., IF LESS THAN 'LIMCNT'
C             REPORTS ARE FOUND IN THE GRID (LAT/LON) BOX, THEN A
C             SUPEROB IS NOT GENERATED FOR THIS BOX}         (DEFAULT=1)
C
C    (NOTE: DELAT, DELON AND LIMCNT APPLY ONLY WHEN ISUPOB = 1)
CC
CC
C      THE FOLLOWING 4 SWITCHES INDICATE THE LATITUDE/LONGITUDE
C                    BOUNDARY FOR ACCEPTING DATA
C
C    LATS    - THIS IS THE SOUTHERN BDRY {LAT: DEG. N (+); DEG. S (-)}
C                                                          (DEFAULT=-90)
C    LATN    - THIS IS THE NORTHERN BDRY {LAT: DEG. N (+); DEG. S (-)}
C                                                          (DEFAULT=+90)
C    LONW    - THIS IS THE WESTERN  BDRY {LON: 0-360 DEG. W}
C                                                          (DEFAULT=360)
C    LONE    - THIS IS THE EASTERN  BDRY {LON: 0-360 DEG. W}
C                                                          (DEFAULT=  0)
C     (NOTE: THESE ARE ALWAYS INTEGERS; MUST BE WHOLE DEGREES)
CC
C
C
C          
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM-SP
C
C$$$

      PROGRAM BUFR_DCODWINDSAT

      LOGICAL ISKIP

      INTEGER(4) LSTHXH_4(720,361)

      INTEGER(8) IDATA20_8

      INTEGER  NOBS(283:283,720,360),ICDATE(5),IDDATE(5),IDATE(8),
     $ NDATE(8),ITOBS(283:283),ICNT(283:283),ILAND(2)

      REAL(8) DATEIN_8(6),WSTDAT_8(8),ERRDAT_8(4),XLOCDT_8(2),
     $ WINDAT_8(4,255),ISWVDT_8(255),ADATA_8(27),RPID_8,SAIDDT_8

      REAL XLOCDT(2),ADATA(27),BLAT(360),BLON(720),
     $ SUMTIM(283:283,720,360),SUMLAT(283:283,720,360),
     $ SUMLON(283:283,720,360),SUMUCO(283:283,720,360),
     $ SUMVCO(283:283,720,360)

      CHARACTER*1  SID8(283:283)
      CHARACTER*8  SUBSET,SUBOUT,SID
      CHARACTER*46 DATSTR,WSTSTR,ERRSTR,LOCSTR,SCWSTR,ISWVST,SAIDST
      CHARACTER*62 OUTST1,OUTST2,OUTST3

      DATA DATSTR/'YEAR MNTH DAYS HOUR MINU SECO '/
      DATA WSTSTR/'WSST SST1 REQV MRWVC MRLWC MWD10 MWS10 WSEQC1 '/
      DATA ERRSTR/'SSTE SPDE VPRE CLDE '/
      DATA LOCSTR /'CLAT CLON '/
      DATA SCWSTR/'WD10 WS10 CHSQ PHER '/
      DATA ISWVST/'ISWV '/
      DATA SAIDST/'SAID '/
      DATA OUTST1/
     $ 'YEAR MNTH DAYS HOUR MINU SECO CLAT CLON WSST SSTE SPDE VPRE '/
      DATA OUTST2/
     $ 'CLDE SST1 REQV MRWVC MRLWC MWD10 MWS10 WSEQC1 WD10 WS10 CHSQ '/
      DATA OUTST3/'PHER SAID ACAV RPID '/

      NAMELIST/RDATA/IPRINT,ISUPOB,DELAT,DELON,LIMCNT,LATS,LATN,LONW,
     $ LONE

      DATA LUNIN /11/,LUNOUT/52/,LUNTAB/20/
      DATA BMISS/10E8/

C  Char. 8 of report id for sat. id:
C                 283
      DATA  SID8/ 'W'/

      EQUIVALENCE  (SID,RPID_8)

      CALL W3TAGB('BUFR_DCODWINDSAT',2020,0233,0083,'NP22')

      PRINT *, ' '
      PRINT *, '=====> WELCOME TO PROGRAM BUFR_DCODWINDSAT - VERSION:',
     $ ' 08/20/2020'
      PRINT *, ' '

      IPRINT = 0
      ISUPOB = 0
      DELAT  = 1.0
      DELON  = 1.0
      LIMCNT = 1
      LATS   = -90
      LATN   =  90
      LONW   = 360
      LONE   =   0

      READ(5,RDATA,END=1905)
 1905 CONTINUE
      IF(LIMCNT.LE.0)  LIMCNT = 1

C  GENERATE DEFAULT PORTION OF OUTPUT REPORT ID
C  --------------------------------------------

      SID = '????????'

      IF(ISUPOB.EQ.1)  THEN

C*****************************************************************
C                      COME HERE IF SUPEROBING
C*****************************************************************

         LATSIZ = 180./DELAT + 0.5
         LONSIZ = 360./DELON + 0.5
         DGH = DELON/2.
         DGV = DELAT/2.

C  BLAT & BLON REPRESENT THE CENTER LAT/LON FOR THE SELECTED GRID
C   BOXES OVERWHICH SUPEROBING IS DONE:
C  --------------------------------------------------------------
C    RANGE:  -90 TO + 90 LAT; 0 TO 360 WEST LON

         LATBEG = 1
         K = 0
         DO LAT  = LATBEG,LATSIZ
            K = K + 1
            BLAT(K) = (DELAT * ((LAT - 1) - LATSIZ/2)) + DGV
         ENDDO
         DO LONG = 1,LONSIZ
            BLON(LONG) = (DELON * (LONG - 1)) + DGH
         ENDDO

C  CHARACTER 1 OF OUTPUT REPORT ID IS SET TO "S" TO IDENTIFY SUPEROBS
C  ------------------------------------------------------------------

         SID(1:1) = 'S'

C*****************************************************************

      END IF

C  INITALIZE ALL SUMS AS ZERO
C  --------------------------

      NOBS   = 0
      SUMTIM = 0
      SUMLAT = 0
      SUMLON = 0
      SUMUCO = 0
      SUMVCO = 0

      ICNT   = 0
      ICNTT  = 0
      ITOBS  = 0
      ITOBST = 0

      IBADD  = 0
      INOT4  = 0
      INOT2  = 0
      ISAID  = 0
      IBADW  = 0
      IGRD   = 0
      ILAND  = 0
      IMSG   = 0
      IMSDM  = 0
      IMSSM  = 0
      ILALO  = 0
      INORET = 0
      IBADQC = 0

C------------------------------------------------------------

C  GET LAND - SEA TABLE (.5 DEGREE X .5 DEGREE)
C  --------------------------------------------

      WRITE(6,6001)
 6001 FORMAT(//' READ IN .5 DEG x .5 DEG LAND/SEA MASK'/)

      DO I=1,720
         READ(19,ERR=9999,END=9999)  (LSTHXH_4(I,J),J=1,361)
ccc      WRITE(6,6111)  (LSTHXH_4(I,J),J=1,61)
 6111    FORMAT(61I1)
      ENDDO

C--------------------------------------------------------------

      WRITE(6,600)
  600 FORMAT(/' GET WINDSAT DATA FROM BUFR DATA DUMP'/)

      IRD = 0
      IGOOD=0

      CALL DATELEN(10)

C  GET THE CENTER & DUMP DATE FROM INPUT BUFR DATA DUMP FILE
C  ---------------------------------------------------------

      CALL DUMPBF(LUNIN,ICDATE,IDDATE)
      WRITE(6,*) ' '
      WRITE(6,*) 'From Original WindSat Bufr Data Dump File in Unit ',
     $ lunin,' (WNDSAT):'
      WRITE(6,*) '     - Center date (ICDATE) = ',ICDATE
      WRITE(6,*) '     - Dump date   (IDDATE) = ',IDDATE
      WRITE(6,*) 'Will transfer these to output reprocessed WindSat ',
     $ 'BUFR file in Unit ',lunout,' (WDSATR)'
      WRITE(6,*) ' '
      IF(ICDATE(1).LE.0)  THEN

C  IF CENTER DATE COULD NOT BE READ FROM FIRST DUMMY MESSAGE, STOP 61
C  ------------------------------------------------------------------

         WRITE(6,*) 'DUMPBF ERROR - CENTER DATE COULD NOT BE READ ',
     $    'FROM INPUT WINDSAT DATA DUMP FILE -- STOP 61'
         CALL W3TAGE('BUFR_DCODWINDSAT')
         CALL ERREXIT(61)
      END IF
      IF(IDDATE(1).LE.0)  THEN

C  IF DUMP DATE COULD NOT BE READ FROM SECOND DUMMY MESSAGE, STOP 62
C  -----------------------------------------------------------------

         WRITE(6,*) 'DUMPBF ERROR - DUMP DATE COULD NOT BE READ ',
     $    'FROM INPUT WINDSAT DATA DUMP FILE -- STOP 62'
         CALL W3TAGE('BUFR_DCODWINDSAT')
         CALL ERREXIT(62)
      END IF

      IDATBF =ICDATE(1)*1000000+ICDATE(2)*10000+ICDATE(3)*100+ICDATE(4)
      IDATDM =IDDATE(1)*1000000+IDDATE(2)*10000+IDDATE(3)*100+IDDATE(4)

C  OPEN THE INPUT BUFR DATA DUMP FILE
C  ----------------------------------

      CALL OPENBF(LUNIN,'IN',LUNIN)
         call openbf(0,'QUIET',1)
CVVVV

C  CALL READMG TO OBTAIN THE SUBSET (MESSAGE TYPE), THEN IMMEDIATELY
C   CLOSE INPUT BUFR FILE AND THEN RE-OPEN IT
C  -----------------------------------------------------------------

      CALL READMG(LUNIN,SUBSET,MDATE,IRET)
      SUBOUT = SUBSET
      CALL CLOSBF(LUNIN)
      CALL OPENBF(LUNIN,'IN',LUNIN)
CAAAA


C  OPEN OUTPUT BUFR FILE WHICH WILL CONTAIN REPROCESSED WINDSAT DATA
C  -----------------------------------------------------------------

      CALL OPENBF(LUNOUT,'OUT',LUNTAB)
      WRITE(6,101) LUNOUT,LUNTAB
  101 FORMAT(/8X,'===> BUFR DATA SET IN UNIT',I3,' SUCCESSFULLY OPENED',
     $ ' FOR OUTPUT; BUFR MNEMONIC TABLES A,B,D IN UNIT',I3/13X,'READ ',
     $ 'IN AND ENCODED INTO BEGINNING MESSAGES OF OUTPUT DATA SET'/)

      PRINT 106, ISUPOB,DELAT,DELON,LIMCNT,LATS,LATN,LONW,LONE
  106 FORMAT(/5X,'USER SPEC. SWITCHES: ISUPOB=',I2,'; DELAT=',F5.1,
     $ '; DELON=',F5.1,'; LIMCNT=',I3/5X,'LATITUDE BDRY:',I4,' TO',I4,
     $ ' (S-N)'/5X,'LONGITUDE BDRY:',I4,' TO',I4,' (W-E) DEG. W.'/)

C  TRANSFER CENTER & DUMP DATE FROM INPUT BUFR FILE TO OUTPUT BUFR FILE
C  --------------------------------------------------------------------

      CALL OPENMG(LUNOUT,SUBOUT,IDATBF)
      CALL MINIMG(LUNOUT,ICDATE(5))
      CALL OPENMG(LUNOUT,SUBOUT,IDATDM)
      CALL MINIMG(LUNOUT,IDDATE(5))
      CALL CLOSMG(LUNOUT)

      IDATE    = 0
      IDATE(1:3) = ICDATE(1:3)
      IDATE(5:6) = ICDATE(4:5)

      IPRT = 0

      IRECL = 0
      ISUBL = 0
      ISUBT = 0
      ISKIP = .FALSE.

      IF(IPRINT.EQ.1)  WRITE(51,8600) 
8600  FORMAT('------DATE------   CLAT   CLON WSST  SSTE  SPDE  VPRE  ',
     $ 'CLDE   SST1   REQV MRWVC  MRLWC MWD10  MWS10    WSEQC1  WD10 ',
     $ 'WS10 CHSQ  PHER SAID ACAV  RPID')

C  READ THROUGH THE MESSAGES/SUBSETS IN THE FILE
C  ---------------------------------------------

      LOOP1: DO WHILE(IREADMG(LUNIN,SUBSET,MDATE).EQ.0)
         IF(ISUPOB.EQ.0) CALL OPENMB(LUNOUT,SUBSET,MDATE)

         LOOP1n1: DO WHILE(IREADSB(LUNIN).EQ.0)

            IRD = IRD+1

C  READ THE INTERNAL DATE AND CHECK FOR REALISM
C  --------------------------------------------

            CALL UFBINT(LUNIN,DATEIN_8,6,1,IRET,DATSTR)
            IYR  = NINT(DATEIN_8(1))
            MON  = NINT(DATEIN_8(2))
            IDAY = NINT(DATEIN_8(3))
            IHR  = NINT(DATEIN_8(4))
            IMIN = NINT(DATEIN_8(5))
            ISEC = NINT(DATEIN_8(6))
            IF(IYR.LT.0 .OR.
     $         MON .LT.1 .OR. MON. GT.12 .OR.
     $         IDAY.LT.1 .OR. IDAY.GT.31 .OR.
     $         IHR .LT.0 .OR. IHR. GT.24 .OR.
     $         IMIN.LT.0 .OR. IMIN.GT.60 .OR.
     $         ISEC.LT.0 .OR. ISEC.GT.60) THEN
               PRINT '("BAD DATE:",I4.4,5I2.2," SUBSET:",A8)',
     $                  IYR,MON,IDAY,IHR,IMIN,ISEC,SUBSET
               IBADD = IBADD + 1
               CYCLE LOOP1n1
            END IF

C  DECODE INFORMATION OUT OF SUBSET
C  --------------------------------

            CALL UFBINT(LUNIN,WSTDAT_8, 8,  1,IRET,WSTSTR)
            CALL UFBINT(LUNIN,ERRDAT_8, 4,  1,IRET,ERRSTR)
            CALL UFBINT(LUNIN,XLOCDT_8, 2,  1,IRET,LOCSTR)
            XLOCDT = XLOCDT_8
            CALL UFBINT(LUNIN,WINDAT_8, 4,255,ILVW,SCWSTR)
            CALL UFBINT(LUNIN,ISWVDT_8, 1,255,ILVI,ISWVST)
            CALL UFBINT(LUNIN,SAIDDT_8, 1,  1,IRET,SAIDST)
            SAIDDT = SAIDDT_8

C  CHECK NUMBER OF REPLICATIONS FOR WIND SETS AND SELECTED WIND VECTOR
C   INDEX (SHOULD BE 4 AND 2, RESPECTIVELY)
C  -------------------------------------------------------------------

            IF(ILVW.NE.4)  THEN
               PRINT '("NUMBER OF REPLICATED WIND SETS (",I5,") IS NOT",
     $                 " 4")',ILVW
               INOT4 = INOT4 + 1
               CYCLE LOOP1n1
            END IF
            IF(ILVI.NE.2)  THEN
               PRINT '("NUMBER OF REPLICATED SELECTED WIND VECTOR ",
     $                 "INDECES (",I5,") IS NOT 2")',ILVI
               INOT2 = INOT2 + 1
               CYCLE LOOP1n1
            END IF

C  CHECK EDR QC FLAG #1 POSITIONS 0 AND 1 FOR "ON" BIT - IF "ON" REJECT
C   OBSERVATION
C  --------------------------------------------------------------------

            IBIT  = MOD(NINT(WSTDAT_8(8)),2)
            IF(IBIT.NE.0)  THEN
               INORET = INORET + 1
               CYCLE LOOP1n1
            END IF

            IBIT = MOD(NINT(WSTDAT_8(8))/2,2)
            IF(IBIT.NE.0)  THEN
               IBADQC = IBADQC + 1
               CYCLE LOOP1n1
            END IF

C  CHECK TO BE SURE OBSERVATION IS OVER WATER BASED ON SURFACE TYPE
C  ----------------------------------------------------------------

            IF(NINT(WSTDAT_8(1)).NE.5)  THEN
               ILAND(1) = ILAND(1) + 1
               CYCLE LOOP1n1
            END IF

C  CHECK FOR RECOGNIZABLE SAT. ID & TAG USING CHARACTER 8 OF REPORT ID
C  -------------------------------------------------------------------

            IF(NINT(SAIDDT).NE.283)  THEN
               PRINT '("SATELLITE ID (",I5,") IS NOT RECOGNIZED")',
     $                  NINT(SAIDDT)
               ISAID = ISAID + 1
               CYCLE LOOP1n1
            END IF

            SID(8:8) = SID8(NINT(SAIDDT))

C  CHECK REPORT FOR OVER LAND BASED ON LAND-SEA MASK
C  -------------------------------------------------

            YY = XLOCDT(1) + 91.5
            XX = XLOCDT(2) + 181.5
            IHX = 2.*XX - 1.0
            IHY = 2.*YY - 1.0
            SLA = XLOCDT(1) + 91.0
            SLO = XLOCDT(2) + 1.0
            IF(XLOCDT(2).LT.0.0) SLO = XLOCDT(2) + 361.0
            IF(SLA.LT.1.OR.SLA.GT.181.0.OR.SLO.LT.1.0.OR.SLO.GT.361.0)
     $       THEN
               IGRD = IGRD + 1
               CYCLE LOOP1n1
            END IF
            IF(LSTHXH_4(IHX,IHY).NE.0) THEN
               ILAND(2) = ILAND(2) + 1
               CYCLE LOOP1n1
            END IF

C  DETERMINE WHICH WIND SET SHOULD BE SELECTED (BASED ON THE VALUE IN
C   THE SECOND REPLICATION OF SELECTED WIND VECTOR INDEX) (AN INDEX OF
C   0, 1, 2 OR 3 CORRESPONDS TO WIND SET 1, 2, 3 OR 4, RESPECTIVELY)
C  -------------------------------------------------------------------

            ISWV = ISWVDT_8(2) + 1  ! must add 1
            IF(ISWV.EQ.0.OR.ISWV.GT.4) THEN
               IBADW = IBADW + 1
               CYCLE LOOP1n1
            END IF

C  CHECK THAT THE SELECTED WIND VECTOR SET IS VALID
C  ------------------------------------------------

            IF(MIN(WINDAT_8(1,ISWV),WINDAT_8(2,ISWV)).GE.999.0_8) THEN
               IMSG = IMSG + 1
               CYCLE LOOP1n1
            END IF

C  CHECK THAT MODEL WIND DIRECTION AND SPEED ARE VALID
C  ---------------------------------------------------

            IF(WSTDAT_8(6).LT.0..OR.WSTDAT_8(6).GT.360.)  THEN
               IMSDM = IMSDM + 1
               CYCLE LOOP1n1
            END IF
            IF(WSTDAT_8(7).LT.0..OR.WSTDAT_8(7).GT.50.) THEN
               IMSSM = IMSSM + 1
               CYCLE LOOP1n1
            END IF

C  STORE LATITUDE IN INLAT: INPUT: N(+), S(-);  OUTPUT: N(+), S(-) X 100
C  ---------------------------------------------------------------------

            INLAT = NINT(XLOCDT(1) * 100.)

C  STORE LON. IN IWLON: INPUT: 0-360 E; OUTPUT: 0-360 W X 100
C  -----------------------------------------------------------------

            IWLON = MOD((36000-NINT(XLOCDT(2)*100.)),36000)

C  CHECK THAT THIS REPORT IS WITHIN THE SPECIFIED LAT/LON BOUNDARY;
C   IF NOT, GO ON TO NEXT REPORT
C  -----------------------------------------------------------------

            IWLONT = IWLON
            LONWT  = LONW
            IF(LONW.LT.LONE)  THEN
               IF(IWLON.LT.LONE*100)  IWLONT = IWLON + 36000
               LONWT = LONW + 360
            END IF
            IF(IWLONT.EQ.0)  IWLONT = 360
            IF(INLAT.LT.LATS*100.OR.INLAT.GE.LATN*100.OR.IWLONT.GT.
     $       LONWT*100.OR.IWLONT.LE.LONE*100)  THEN
               ILALO = ILALO + 1
               CYCLE LOOP1n1
            END IF

            IGOOD = IGOOD + 1

            IF(ISUPOB.EQ.1) THEN

C*****************************************************************
C        REPORT PASSED ALL CHECKS - COME HERE TO SUPEROB
C*****************************************************************

C  STORE REPORT TIME IN "ITIME", CHECK FOR CROSS OVER TO NEXT DAY (IF
C   SO, ADD 24000 SO AVG. TIME MAKES SENSE; WILL CORRECT LATER IF ITIME
C   IS > 24000) (THIS MAKES THE ASSUMPTION THAT IS CAN ONLY OCCUR WHEN
C   THE CENTER DUMP HOUR IS 0, OKAY RIGHT NOW SINCE THIS DATA DUMPS
C   ONLY OCCUR AT 00, 06, 12 AND 18Z WITH +/- 3-HOUR TIME WINDOW)
C  ---------------------------------------------------------------------

               ITIME = NINT((1000. * (REAL(IHR) + REAL(IMIN)/60. +
     $          REAL(ISEC)/3600.)) + 0.00005)
               IF(ICDATE(4).EQ.0.AND.IHR.LT.12) ITIME=ITIME+24000

               ILM = 0
               JLM = 0

C  FIND LAT/LON BOX CONTAINING THIS REPORT (ILM, JLM)
C  --------------------------------------------------

               LATBEG = 1
               KNDX = 0
               LOOP1n2: DO II = LATBEG,LATSIZ
                  KNDX = KNDX + 1
                  IF(INLAT.GE.NINT((BLAT(KNDX)+DGV)*100.).OR.
     $             INLAT.LT.NINT((BLAT(KNDX)-DGV)*100.))  CYCLE LOOP1n2
                  ILM = KNDX
                  LOOP1n3: DO IJ = 1,LONSIZ
                     IF(IWLON.GE.NINT((BLON(IJ)+DGH)*100.).OR.IWLON.LT.
     $                NINT((BLON(IJ)-DGH)*100.))  CYCLE LOOP1n3
                     JLM = IJ
                     GO TO 5
                  ENDDO LOOP1n3
               ENDDO LOOP1n2

C  IF NO BOX FOUND, GO ON TO NEXT REPORT
C  -------------------------------------

               PRINT 105, XLOCDT(1),XLOCDT(2)
  105 FORMAT(5X,'* * * *   REPORT LAT/LON IS EITHER MISSING OR ',
     $ 'INVALID, LAT=',F6.2,', LON=',F7.2,' -- GO ON TO NEXT REPORT'/)
               CYCLE LOOP1n1

    5          CONTINUE

C  THE FOLLOWING SHOULD   N E V E R   HAPPEN!!
C  -------------------------------------------

               IF(ILM.EQ.0.OR.JLM.EQ.0)  THEN
                  PRINT *, '&&& EITHER ILM (',ILM,') OR JLM (',JLM,')',
     $             ' IS ZERO -- SHOULD NEVER HAPPEN !!!'
                  CYCLE LOOP1n1
               END IF

C  FIND CORRESPONDING U-COMP AND V-COMP OF WIND FROM DIRECTION & SPEED
C  -------------------------------------------------------------------

               WDIR = WINDAT_8(1,ISWV)
               WSPD = WINDAT_8(2,ISWV)
               CALL W3FC06(WDIR,WSPD,UCOMP,VCOMP)

C  SUM UP VALUES WITHIN THE GRID BOX ILM, JLM
C  ------------------------------------------

               NOBS(NINT(SAIDDT),JLM,ILM) = NOBS(NINT(SAIDDT),JLM,ILM)+1
               ITOBS(NINT(SAIDDT)) = ITOBS(NINT(SAIDDT)) + 1
               SUMTIM(NINT(SAIDDT),JLM,ILM) =
     $          SUMTIM(NINT(SAIDDT),JLM,ILM) + REAL(ITIME)
               SUMLAT(NINT(SAIDDT),JLM,ILM) =
     $          SUMLAT(NINT(SAIDDT),JLM,ILM) + XLOCDT(1)
               SUMLON(NINT(SAIDDT),JLM,ILM) =
     $          SUMLON(NINT(SAIDDT),JLM,ILM) + XLOCDT(2)
               SUMUCO(NINT(SAIDDT),JLM,ILM) =
     $          SUMUCO(NINT(SAIDDT),JLM,ILM) + UCOMP
               SUMVCO(NINT(SAIDDT),JLM,ILM) =
     $          SUMVCO(NINT(SAIDDT),JLM,ILM) + VCOMP

            ELSE 

C*****************************************************************
C      REPORT PASSED ALL CHECKS - COME HERE IF NOT SUPEROBING
C*****************************************************************

C  THIN REPORTS TO EVERY OTHER "GOOD" OBSERVATION
C  ----------------------------------------------

ccccc          IF(ISKIP)  THEN
ccccc             ISKIP = .FALSE.
ccccc             CYCLE LOOP1n1
ccccc          ELSE
ccccc             ISKIP = .TRUE.
ccccc          END IF

C  BUILD AN INTERMEDIATE ARRAY (ADATA_8) CONTAINING THE DATA
C  ---------------------------------------------------------

               ADATA_8(1:6)   = DATEIN_8(1:6)
               ADATA_8(7:8)   = XLOCDT_8(1:2)
               ADATA_8(9)     = WSTDAT_8(1)
               ADATA_8(10:13) = ERRDAT_8(1:4)
               ADATA_8(14:20) = WSTDAT_8(2:8)
               ADATA_8(21:24) = WINDAT_8(1:4,ISWV)
               ADATA_8(25)    = SAIDDT_8
               ADATA_8(26)    = 1_8

               ICNT(NINT(SAIDDT)) = MIN(9999999,ICNT(NINT(SAIDDT))+1)
               ICNTT = ICNTT + 1
               WRITE(SID(1:7),'(I7.7)')  ICNT(NINT(SAIDDT))
               ADATA_8(27)    = RPID_8

               IF(IPRINT.EQ.1)  WRITE(51,8601)  (NINT(ADATA_8(II)),
     $          II=1,6),(ADATA_8(II),II=7,8),NINT(ADATA_8(9)),
     $          (ADATA_8(II),II=10,19),NINT(ADATA_8(20)),
     $          NINT(ADATA_8(21)),ADATA_8(22),ADATA_8(23),ADATA_8(24),
     $          NINT(ADATA_8(25)),NINT(ADATA_8(26)),SID
C                     --DATE-------------------  CLAT CLON WSST   SSTE
 8601          FORMAT(I4,3I2.2,':',I2.2,':',I2.2,F7.2,F8.2, I3,1X,F6.2,
C               SPDE VRPE    CLDE SST1    REQV MRWVC MRLWC MWD10 MWS10
     $          F6.2,F6.2,1X,F6.3,F7.2,1X,F7.4,F6.2, F7.2, F7.2, F7.2,
C             WSEQC1 WD10 WS10 CHSQ    PHER   SAID  ACAV  RPID
     $          I11, I4,  F5.1,F6.2,1X,F5.1,1X,I3,1X,I3,1X,A8)
ccccc          print *, 'REQV = ',adata_8(15)
ccccc          print *, 'SSTE = ',adata_8(10)

C  ENCODE THE REPROCESSED WINDSAT DATA INTO A SUBSET (REPORT)
C  ----------------------------------------------------------

               CALL UFBINT(LUNOUT,ADATA_8(1), 12,1,IRET,OUTST1)
               CALL UFBINT(LUNOUT,ADATA_8(13),11,1,IRET,OUTST2)
               CALL UFBINT(LUNOUT,ADATA_8(24), 4,1,IRET,OUTST3)
     
C  WRITE THE ENCODED SUBSET (REPORT) INTO THE OUTPUT BUFR MESSAGE
C  --------------------------------------------------------------

               CALL WRITSB(LUNOUT)
               CALL UFBCNT(LUNOUT,IREC,ISUB)
               IF(IREC.GT.IRECL)  THEN
                  ISUBT = ISUBT + ISUBL
                  WRITE(6,1254) SUBSET,IREC-1,ISUBL,ISUBT
 1254 FORMAT(/' --- THIS RPT OPENS NEW BUFR MSG (SUBSET=',A,'): LAST ',
     $ 'MSG WAS NO.',I9,' (DATA) WITH',I5,' RPTS (TOTAL NO. RPTS ',
     $ 'WRITTEN=',I7,')'/)
               END IF
               ISUBL = ISUB
               IRECL = IREC

C*****************************************************************
            END IF

         ENDDO LOOP1n1

      ENDDO LOOP1

      IF(ISUPOB.EQ.1) THEN 

C***********************************************************************
C             IF SUPEROBING LOOP THRU THE SATELLITE ID'S
C***********************************************************************

         CALL OPENMG(LUNOUT,SUBOUT,IDATBF)

         LOOP2: DO K = 283,283

C  CHECK TO SEE IF ANY REPORTS ARE FROM THIS SATELLITE ID
C  ------------------------------------------------------

            IF(ITOBS(K).EQ.0)  CYCLE LOOP2

            ITOBS(K) = 0

C-----------------------------------------------------------------------
C    LOOP THRU BOXES: CONSTRUCT MEANS & WRITE SUPEROB TO ADATA ARRAY
C-----------------------------------------------------------------------

            LATBEG = 1
            KNDX = 0
            LOOP2n1: DO KI = LATBEG,LATSIZ
               KNDX = KNDX + 1
               LOOP2n2: DO KJ = 1,LONSIZ

C  IF LESS THAN 'LIMCNT' INDIVIDUAL REPORTS WENT INTO MAKING SUPEROB
C   IN THIS BOX, SKIP PROCESSING THE SUPEROB FOR THIS BOX
C  -----------------------------------------------------------------

                  IF(NOBS(K,KJ,KNDX).LT.LIMCNT)  CYCLE LOOP2n2
                  ITOBS(K) = ITOBS(K) + NOBS(K,KJ,KNDX)
                  ITOBST = ITOBST + NOBS(K,KJ,KNDX)
                  XMUL = REAL(NOBS(K,KJ,KNDX))

C  CALC. MEAN WIND SPEED, WIND DIRECTION, TIME, LATITUDE AND
C   LONGITUDE WITHIN EACH GRID BOX
C  ---------------------------------------------------------

                  AVGUCO = SUMUCO(K,KJ,KNDX)/XMUL
                  AVGVCO = SUMVCO(K,KJ,KNDX)/XMUL
                  AVGTIM = SUMTIM(K,KJ,KNDX)/XMUL
                  AVGLAT = SUMLAT(K,KJ,KNDX)/XMUL
                  AVGLON = SUMLON(K,KJ,KNDX)/XMUL

C  FIND CORRESPONDING SPEED & DIRECTION OF WINDS FROM U-COMP AND V-COMP
C  --------------------------------------------------------------------

                  CALL W3FC05(AVGUCO,AVGVCO,AVGWD,AVGWS)

C  STORE MEAN TIME IN "ITIME" (CHECK FOR MEAN TIME INTO THE NEXT DAY)
C  ------------------------------------------------------------------

                  ITIME = NINT(AVGTIM + 0.00005)
                  IF(ITIME.GE.24000)  ITIME = ITIME - 24000.

C  BUILD AN INTERMEDIATE ARRAY (ADATA) CONTAINING THE DATA
C  ---------------------------------------------------------

                  ADATA = BMISS

C  MODIFY (IF NEEDED) CENTER DATE YEAR, MONTH AND DAY TO PROPERLY
C   DEFINE THE YEAR, MONTH AND DAY OF THE SUPEROB
C  --------------------------------------------------------------

                  NDATE = IDATE
                  JTIME=NINT(1000. * (REAL(IDATE(5)) +
     $             REAL(IDATE(6))/60. + REAL(IDATE(7))/3600.))
                  IF(ABS(JTIME-ITIME).GT.6000)  THEN
                     DAYCHG = SIGN(1.0,REAL(JTIME-ITIME))
                     CALL W3MOVDAT((/DAYCHG,0.,0.,0.,0./),IDATE,NDATE)
                  END IF

                  ADATA(1:3) = NDATE(1:3)
                  ADATA(4) = INT(ITIME/1000.)
                  XMIN     = (ITIME - (NINT(ADATA(4)) * 1000)) * 0.06
                  ADATA(5) = INT(XMIN)
                  ADATA(6) = (NINT(XMIN*100.) - NINT(ADATA(5))*100)*0.6
                  ADATA(7) = AVGLAT
                  ADATA(8) = AVGLON
                  ADATA(21)= AVGWD
                  ADATA(22)= AVGWS
                  ADATA(25)= K
                  ADATA(26)= NOBS(K,KJ,KNDX)

                  ICNT(K) = MIN(999999,ICNT(K)+1)
                  ICNTT = ICNTT + 1
                  WRITE(SID(2:7),'(I6.6)')  ICNT(K)
                  ADATA_8(27) = RPID_8

                  ADATA_8(1:26) = ADATA(1:26)

                  IDATA20_8 = 999999999999_8

                  IF(IPRINT.EQ.1)  WRITE(51,8601)  (NINT(ADATA_8(II)),
     $             II=1,6),(ADATA_8(II),II=7,8),NINT(ADATA_8(9)),
     $             (ADATA_8(II),II=10,19),IDATA20_8,NINT(ADATA_8(21)),
     $             ADATA_8(22),ADATA_8(23),ADATA_8(24),
     $             NINT(ADATA_8(25)),NINT(ADATA_8(26)),SID

C  ENCODE THE REPROCESSED WINDSAT DATA INTO A SUBSET (REPORT)
C  ----------------------------------------------------------

                  CALL UFBINT(LUNOUT,ADATA_8(1), 12,1,IRET,OUTST1)
                  CALL UFBINT(LUNOUT,ADATA_8(13),11,1,IRET,OUTST2)
                  CALL UFBINT(LUNOUT,ADATA_8(24), 4,1,IRET,OUTST3)

C  WRITE THE ENCODED SUBSET (REPORT) INTO THE OUTPUT BUFR MESSAGE
C  --------------------------------------------------------------

                  CALL WRITSB(LUNOUT)
                  CALL UFBCNT(LUNOUT,IREC,ISUB)
                  IF(IREC.GT.IRECL)  THEN
                     ISUBT = ISUBT + ISUBL
                     WRITE(6,1254) SUBOUT,IREC-1,ISUBL,ISUBT
                  END IF
                  ISUBL = ISUB
                  IRECL = IREC

               ENDDO LOOP2n2
            ENDDO LOOP2n1
         ENDDO LOOP2 
C***********************************************************************

      END IF

C  CLOSE INPUT AND OUTPUT NCEP WINDSAT BUFR DATA SETS
C  --------------------------------------------------
                                                                        
      CALL CLOSBF(LUNIN)
      WRITE(6,102) LUNIN
  102 FORMAT(/5X,'===> BUFR DATA SET IN UNIT',I3,' SUCCESSFULLY ',
     $ 'CLOSED'/)

      CALL UFBCNT(LUNOUT,IREC,ISUB)
      ISUBT = ISUBT + ISUB
      WRITE(6,1253) IREC,ISUB,ISUBT
 1253 FORMAT(/' --- WROTE BUFR DATA MSG NO.',I10,' WITH',I5,' REPORTS ',
     $ '(TOTAL NO. REPORTS WRITTEN =',I7,')'/)
      CALL CLOSBF(LUNOUT)
      WRITE(6,102) LUNOUT

      WRITE(6,699) IRD,IBADD,INOT4,INOT2,ISAID,IBADW,IGRD,INORET,IBADQC,
     $ ILAND(1),ILAND(2),IMSG,IMSDM,IMSSM,ILALO,IGOOD
  699 FORMAT(//5X,'*** PROCESSING ENDED NORMALLY ***'//
     $ ' >>>  TOTAL NUMBER OF INPUT WINDSAT REPORTS READ ',42('.'),I7,
     $ '  <<<'//
     $ 12X,'- Number skipped due to invalid time stamp ',36('.'),I7//
     $ 12X,'- Number skipped due to # of replicated wind sets not',
     $ ' 4 ',23('.'),I7//
     $ 12X,'- Number skipped due to # of replicated selected wind ',
     $ 'vector indeces not 2 ',4('.'),I7//
     $ 12X,'- Number skipped due to unrecognized satellite id ',29('.'),
     $ I7//
     $ 12X,'- Number skipped due to invalid wind vector index ',29('.'),
     $ I7//
     $ 12X,'- Number skipped due to invalid lat/lon location ',30('.'),
     $ I7//
     $ 12X,'- Number skipped due to "no retrieval" EDR quality control',
     $ ' flag ',15('.'),I7//
     $ 12X,'- Number skipped due to bad EDR quality control flag ',
     $ 26('.'),I7//
     $ 12X,'- Number skipped due to not being over ocean based on ',
     $ 'surface type ',12('.'),I7//
     $ 12X,'- Number skipped due to being over land based on land-sea ',
     $ 'mask ',16('.'),I7//
     $ 12X,'- Number skipped due to missing wind ',42('.'),I7//
     $ 12X,'- Number skipped due to missing model wind direction ',
     $ 26('.'),I7//
     $ 12X,'- Number skipped due to missing model wind speed ',30('.'),
     $ I7//
     $ 12X,'- Number skipped due to being outside lat/lon domain ',
     $ 26('.'),I7//
     $ 12X,'- NUMBER OF INPUT WINDSAT REPORTS PASSING ALL CHECKS ',
     $ 26('.'),I7/)

      IF(ISUPOB.EQ.1)  THEN

C  IF SUPEROBING WAS DONE, PRODUCE FINAL COUNTS OF ORIGINAL REPORTS
C  ----------------------------------------------------------------

         PRINT 114, ITOBS,ITOBST
  114 FORMAT(/30X,'+++ NO. OF ORIGINAL REPORTS THAT WERE USED TO ',
     $ 'GENERATE SUPEROBS +++'/
     $ /45X,'FROM SATELLITE ID  283 ...... ',I10,
     $ /45X,'TOTAL FROM ALL SATELLITES ... ',I10/)
cdak     PRINT 115
  115    FORMAT(5X,'NOBS:')
cdak     PRINT 116, NOBS
  116    FORMAT(1X,30I4)
      END IF

      PRINT 110, ICNT,ICNTT
  110 FORMAT(/27X,'+++  NUMBER OF REPROCESSED WINDSAT REPORTS WRITTEN ',
     $ 'TO OUTPUT FILE +++'//
     $ 40X,'>>>  FROM SATELLITE ID  283 ...... ',I7,'  <<<'/
     $ ' >>>  TOTAL NUMBER OF REPROCESSED WINDSAT REPORTS WRITTEN ..',
     $ I7,'  <<<'//
     $ 44X,'*****  PROGRAM SUCCESSFULLY COMPLETED  *****'/)

      CALL W3TAGE('BUFR_DCODWINDSAT')

      STOP                                                              

C-----------------------------------------------------------------------

 9999 CONTINUE

C  IF ERROR READING LAND/SEA MASK (OR EMPTY OR INCOMPLETE), STOP 55
C  ----------------------------------------------------------------

      WRITE(6,*) 'ERROR READING 0.5 x 0.5 DEG LAND/SEA MASK (MAY BE ',
     $ 'EMPTY OR INCOMPLETE AS WELL)  -- STOP 55'
      CALL W3TAGE('BUFR_DCODWINDSAT')
      CALL ERREXIT(55)

      END 
