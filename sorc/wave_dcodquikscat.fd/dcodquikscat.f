C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: WAVE_DCODQUIKSCAT
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2017-01-12
C
C ABSTRACT: REPROCESSES QUIKSCAT, ASCAT AND OSCAT (SCATTEROMETER) DATA.
C   (NOTE: CURRENTLY, OSCAT DATA IS NOT REPROCESSED AND SO IS NOT READ
C   BY THIS CODE.  LOGIC IS IN PLACE TO DO SO IF IT EVER DOES NEED TO
C   BE REPROCESSED HERE.) READS EACH REPORT FROM INPUT BUFR DATA DUMP
C   FILE AND PERFORMS A NUMBER OF CHECKS, INCLUDING: REPORT DATE
C   CHECKED FOR REALISM, REPORTS OVER LAND SKIPPED, PROPER NUDGED WIND
C   VECTOR SELECTED (FROM CHOICE OF 4 FOR QUIKSCAT, 2 OR 4 FOR ASCAT,
C   AND ???? FOR OSCAT: NOTE: INCREASED FROM 2 TO 4 FOR ASCAT ON
C   9/14/2010), REPORTS WITH MISSING SELECTED NUDGED WIND VECTOR
C   SKIPPED, REPORTS WITH MISSING MODEL WIND DIRECTION AND SPEED ARE
C   SKIPPED, QUIKSCAT OR OSCAT (ONLY) REPORTS WITH PROBABILITY OF RAIN
C   GREATER THAN SPECIFIED LIMIT ARE SKIPPED (OPTIONAL), QUIKSCAT OR
C   OSCAT (ONLY) REPORTS AT THE EDGES OF THE ORBITAL SWATH (AS
C   SPECIFIED BY UPPER AND LOWER CELL NUMBER LIMITS) ARE SKIPPED
C   (OPTIONAL), ASCAT (ONLY) REPORTS WITH ONE OR MORE "CRITICAL" WIND
C   VECTOR CELL QUALITY FLAGS SET ARE SKIPPED (OPTIONAL). REPORTS
C   PASSING CHECKS ARE SUPEROBED ONTO A USER-SPECIFIED LAT/LON GRID
C   ACCORDING TO SATELLITE ID (OPTIONAL) AND THEN REPROCESSED INTO A
C   BUFR FILE WHICH WILL LATER BE READ BY EITHER THE PROGRAM
C   PREPOBS_PREPDATA (CURRENTLY QUIKSCAT AND ASCAT), OR DIRECTLY BY THE
C   ANALYSIS ITSELF (CURRENTLY OSCAT). ALSO, A UNIQUE ID IS GENERATED
C   HERE FOR EACH REPORT AND THEN ENCODED INTO THE OUTPUT REPROCESSED
C   BUFR FILE. THE REPROCESSED BUFR FILE CONTAINS ONLY THOSE DATA
C   NEEDED FOR EITHER PREPBUFR PROCESSING AND LATER ASSIMILATION INTO
C   THE NCEP ANALYSES OR FOR THE ANALYSIS ITSELF. A USER-SPECIFIED
C   SWITCH ALLOWS REPORTS TO BE SELECTED BY LOCATION (LAT/LON
C   BOUNDARY).
C
C PROGRAM HISTORY LOG:
C
C 2000-06-01 W. GEMMILL - ORIGINAL AUTHOR
C 2000-06-13 D. KEYSER  - ADDED ENCODING TO OUTPUT REPROCESSED
C            BUFR FILE; MODIFIED FOR OPERATIONAL IMPLEMENTATION
C 2000-12-05 D. KEYSER  - ADDED INPUT NAMELIST AND ENCODING OF
C            REPORT ID (RPID) INTO OUTPUT FILE
C 2001-04-05 D. KEYSER  - READS SATELITE ID (SAID) FROM INPUT
C            FILE AND ENCODES INTO OUTPUT FILE; ADDED NAMELIST SWITCH
C            "IPRINT" TO CONTROL WRITE OF REPORT LISTING TO UNIT 52
C 2001-08-20 D. KEYSER/T.-W. YU -- MODIFIED TO ADD THE OPTION FOR
C            SUPEROBING QUIKSCAT DATA INTO A USER-SPECIFIED LAT/LON
C            GRID BOX ACCORDING TO SATELLITE ID.  IN ADDITION, OPTIONS
C            ADDED TO ELIMINATE THE DATA WITH RAIN PROBABILITY GREATER
C            THAN SPECIFIED LIMIT AND TO ELIMINATE DATA AT EDGES OF
C            ORBITAL SWATHS (AS SPECIFIED BY UPPER AND LOWER CELL
C            NUMBER LIMITS) (BOTH PRIOR TO SUPEROBING)
C 2008-05-05 D. KEYSER - REMOVED VARIABLE "INDEX" FROM INPUT NAMELIST
C            AS IT IS NO LONGER SPECIFIED IN DRIVER SCRIPTS (NO LONGER
C            HAVE SPLIT DUMPS WHICH HAVE TO BE COMBINED) IMPORTANT
C            CHANGE RELATED TO THIS IS THAT STNID WILL NOW HAVE "S"
C            IN CHARACTER 1 FOR SUPEROBS INSTEAD OF IN CHARACTER 2; NOW
C            HANDLES ASCAT AS WELL AS QUIKSCAT DATA
C 2008-11-19 D. KEYSER - NOW STOPS WITH ABNORMAL R.C. 55 IF LAND-SEA
C            MASK FILE IS EMPTY OR INCOMPLETE (IN ADDITION TO READ
C            ERROR); FIXED BUG WHICH PREVENTED POSTING OF NUMBER OF
C            REPROCESSED REPORTS TO DUMP STATUS FILE FOR SUPEROBED
C            REPORTS
C 2010-10-27 D. KEYSER - ACCOUNTS FOR ASCAT NOW CONTAINING 4 (RATHER
C            THAN 2) SETS OF WIND INFORMATION AFTER 9/14/2010 (ASCAT
C            REPORTS WERE ALL BEING TOSSED BY THIS CODE SINCE THEN)
C 2011-08-04 D. KEYSER - IN RESPONSE TO THE LATEST VERSION OF BUFRLIB
C            WHICH CAN HANDLE EMBEDDED DICTIONARY MESSAGES:: INCREASES
C            DEGREE OF BUFRLIB PRINTOUT SUCH THAT CODE WILL PRINT A
C            DIAGNOSTIC IF ANY EMBEDDED DICTIONARY MESSAGES ARE FOUND
C            WHEN READING IN MESSAGES; FOR NON-SUPEROB PROCESSING ONLY:
C            REPLACES CALL TO OPENMG (WHICH FORCED THE SAME CENTER BUFR
C            DUMP DATE/HOUR TO BE WRITTEN INTO SEC. 1 OF ALL OUTPUT
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
C 2012-11-20 J. WOOLLEN - INITIAL PORT TO WCOSS
C 2014-01-28 D. KEYSER  -  NOW HANDLES OSCAT AS WELL AS QUIKSCAT AND
C            ASCAT DATA (ALTHOUGH CURRENTLY, OSCAT DATA IS NOT
C            REPROCESSED AND SO IS NOT READ BY THIS CODE); MODIFIED TO
C            ACCOUNT FOR NEW METOP-B SATELLITE (SAID=3) IN ASCAT
C            PROCESSING, ADDED UPDATED PRINT STATEMENTS SUMMARIZING
C            COUNTS BY SATELLITE FOR ASCAT, NO LONGER OUTPUTS SATELLITE
C            ID FOR ASCAT SUPEROBS (SINCE THEY COULD BE A MIXTURE OF
C            METOP-A AND -B); ADDED NEW NAMELIST SWITCH "ISATSK" WHICH
C            ALLOWS ALL REPORTS FROM A PARTICULAR BUFR SATELLITE ID TO
C            BE SKIPPED {APPLIES TO ASCAT WHICH CAN NOW BE FROM
C            SATELLITE ID 3 (METOP-B OR 4 (METOP-A}, DEFAULT IS 99999,
C            NO REPORTS SKIPPED; ONLY THE FIRST 100 REPORTS WITH AN
C            UNRECOGNIZED SATELLITE ID ARE NOW PRINTED (TO REDUCE SIZE
C            OF STDOUT IN SUCH CASES)
C 2017-01-12 D. Stokes  -  Minor bug fix to preserve valid obs located
C            just west of 180E.  Also, use 10E8_8 to represent missing
C            bufr values to reduce risk of integer overflows on WCOSS.
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - INPUT DATA CARDS IN THE FORM OF A NAMELIST (SEE
C              - REMARKS)
C     UNIT 11  - NCEP BUFR DATA DUMP CONTAINING ORIGINAL FORM OF
C                QUIKSCAT ("QKSCAT"), ASCAT ("ASCATT") OR OSCAT
C                ("OSCATT") DATA
C     UNIT 19  - 0.5 x 0.5 DEG LAT/LON LAND/SEA MASK (INTEGER(4))
C     UNIT 20  - BUFR MNEMONIC TABLE (NEEDED TO PRODUCE REPROCESSED
C                NCEP BUFR FILE)
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 51  - TEXT LISTING OF ALL REPORTS ENCODED TO UNIT 52
C              - (GENERATED ONLY WHEN INPUT NAMELIST VARIABLE "IPRINT"
C              - IS 1)
C     UNIT 52  - NCEP BUFR DATA DUMP CONTAINING FINAL (REPROCESSED)
C                FORM OF QUIKSCAT ("QKSWND"), ASCAT ("ASCATW") OR OSCAT
C                ("OSCATW") WIND DATA WITH UNIQUE REPORT IDS (AND
C                POSSIBLY SUPEROBED)
C
C   SUBPROGRAMS CALLED:
C     LIBRARY
C       W3NCO    - W3TAGB   W3TAGE   ERREXIT
C       W3EMC    - W3FC05   W3FC06
C       BUFRLIB  - DATELEN  DUMPBF   OPENBF   OPENMG   MINIMG   CLOSMG
C                - UFBREP   UFBINT   WRITSB   UFBCNT   CLOSBF   IREADMG
C                - UPFTBV   IBFMS    OPENMB   SETBMISS GETBMISS 
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =  55 - ERROR READING LAND/SEA MASK (OR FILE EMPTY OR
C                  INCOMPLETE)
C          =  60 - INPUT SWITCH "ITYPE" IS NOT RECOGNIZED {NEITHER "1"
C                  (QUIKSCAT), "2" (ASCAT) NOR "3" (OSCAT)}
C          =  61 - ERROR OBTAINING CENTER DATE FROM FIRST BUFR MESSAGE
C                  IN UNIT 11
C          =  62 - ERROR OBTAINING DUMP DATE FROM SECOND BUFR MESSAGE
C                  IN UNIT 11
C
C REMARKS: 
C
C     Contents of input BUFR file:
C
C     DATEIN_8 contains report year, month, day, hour, minute, second,
C      DATA DATSTR/'YEAR MNTH DAYS HOUR MINU SECO '/
C
C     QSCDAT_8 contains along-track row number, across-swath cell
C            number, model wind direction at 10 m, model wind speed
C            at 10m, and index of selected wind vector
C            (along-track row number only available for QUIKSCAT and
C             OSCAT)
C      DATA QSCSTR/'ATRN CTCN MWD10 MWS10 ISWV '/
C
C     XLOCDT_8 contains the report latitude and longitude.
C      DATA LOCSTR  /'CLAT CLON '/   - for QUIKSCAT and OSCAT
C      DATA LOCSTRH /'CLATH CLONH '/ - for ASCAT
C
C     QUIKSCAT:
C     WINDAT_8 contains 4 sets of wind information, corresponding to
C            the 4 "views."  Each set comprises: wind speed at 10 m,
C            and wind direction at 10 m (plus two missing "NUL" values
C            as "place holders")
C      DATA SCWSTRH /'WS10H WD10H NUL NUL '/
C
C     ASCAT:
C     WINDAT_8 contains 2 or 4 sets of wind information, corresponding
C            to the 2 or 4 "views." (Increased from 2 to 4 on 9/14/2010
C            at 1600 UTC.) Each set comprises: wind speed at 10 m, wind
C            direction at 10 m, backscatter distance and likelihood
C            computed for solution
C      DATA SCWSTR /'WS10 WD10 BSCD LKCS '/
C
C     OSCAT::
C     WINDAT_8 contains 4 sets of wind information, corresponding to
C            the 4 "views."  Each set comprises: wind speed at 10 m,
C            and wind direction at 10 m (plus two missing "NUL" values
C            as "place holders")
C      DATA SCWSTRo /'WS10 WD10 NUL NUL '/
C
C     QUIKSCAT and OSCAT:
C     RAINDT_8 contains the seawinds probability of rain
C      DATA RAINST/'SPRR '/
C
C     ASCAT:
C     QUALDT_8 contains the wind vector cell quality flag
C      DATA QUALST/'WVCQ '/
C
C     SAIDDT_8 contains the satellite id
C      DATA SAIDST/'SAID '/
C
C     Contents of output BUFR file:
C
C        ADATA(1)  -- REPORT YEAR ("YEAR")
C        ADATA(2)  -- REPORT MONTH ("MNTH")
C        ADATA(3)  -- REPORT DAY ("DAYS")
C        ADATA(4)  -- REPORT HOUR ("HOUR")
C        ADATA(5)  -- REPORT MINUTE ("MINU")
C        ADATA(6)  -- REPORT SECOND ("SECO")
C        ADATA(7)  -- LATITUDE ("CLAT")
C        ADATA(8)  -- LONGITUDE ("CLON")
C        ADATA(9)  -- WIND SPEED AT 10 METERS ("WS10")
C        ADATA(10) -- WIND DIRECTION AT 10 METERS ("WD10")
C      *-ADATA(11) -- ACROSS-SWATH CELL NUMBER (1-72) ("CTCN")
C      *-ADATA(12) -- QUIKSCAT and OSCAT: ALONG-TRACK ROW # ("ATRN")
C                  -- ASCAT:    BACKSCATTER DISTANCE ("BSCD")
C      *-ADATA(13) -- QUIKSCAT and OSCAT: SEAWINDS PROBABILITY OF RAIN
C                               (0.0 TO 1.0) ("SPRR")
C                  -- ASCAT:    WIND VECTOR CELL QUALITY FLAG ("WVCQ")
C      %-ADATA(14) -- SATELLITE ID (EITHER 281 FOR QUIKSCAT, 004 OR 003
C                      FOR ASCAT, OR 421 FOR OSCAT) ("SAID")
C      $-ADATA(15) -- NUMBER OF OBSERVATIONS THAT WENT INTO MAKING A
C                      SUPEROB ("ACAV")
C      #-ADATA(16) -- REPORT ID ("RPID")
C      *-ADATA(17) -- QUIKSCAT and OSCAT: NULL (MISSING) ("NUL")
C                  -- ASCAT:    LIKELIHOOD COMPUTED FOR SOLUTION
C                                ("LKCS")
C      
C          ALL ABOVE ARE MEANS FOR SUPEROBS
C      * - NOT STORED FOR SUPEROBS
C      % - NOT STORED FOR ASCAT SUPEROBS, STORED FOR QUIKSCAT AND OSCAT
C           SUPEROBS
C      $ - STORED AS 1 FOR NON-SUPEROBS
C      # - REPORT ID MADE UP AS FOLLOWS:
C            Superobs:
C              Character 1: Set to "S" to identify superobs
C              Characters 2-7: Index which incrementally counts reports
C            Non-superobs:
C              Characters 1-7: Index which incrementally counts reports
C            Character 8: Indicator for satellite id ("Q" - 281,
C                          "A" - 004 or 003, "O" - 421)
C
C
C   VARIABLES READ IN NAMELIST "RDATA":
C
C    ITYPE  - IF = 1 QUIKSCAT DATA (DEFAULT)
C             IF = 2 ASCAT DATA
C             IF = 3 OSCAT DATA
C    IPRINT - IF = 0 (DEFAULT) WILL NOT PRINT LISTING OF ALL
C             PROCESSED REPORTS TO UNIT 51; IF = 1 WILL PRINT
C             LISTING TO UNIT 51
CC
C    ISATSK - ALL REPORTS WITH THIS BUFR SATELLITE ID ARE SKIPPED
C             {CURRENTLY SHOULD APPLY ONLY TO ASCAT WHICH CAN COME FROM
C              EITHER METOP-2(A) (SATELLITE ID 4) OR METOP-1(B)
C              (SATELLITE ID 3)}, ONLY ONE VALUE POSSIBLE
C                                    (DEFAULT=99999, NO REPORTS SKIPPED)
C             
C    ISUPOB - SUPEROB SWITCH
C        ISUPOB = 0 -- SUPEROBS ARE NOT GENERATED; ALL REPROCESSED
C                      REPORTS PASSING CHECKS ARE PACKED INTO OUTPUT
C                      BUFR FILE
C        ISUPOB = 1 -- SUPEROBS ARE GENERATED (AFTER ANY QC) BY TAKING
C                      LINEAR AVERAGE OF ALL VALID DATA/TIMES WITHIN
C                      DELATxDELON DEGREE LAT/LON BOXES (SEE BELOW FOR
C                      DELAT AND DELON); SUPEROB IS PLACED AT POINT
C                      WITHIN BOX REPRESENTED BY LINEAR AVERAGED
C                      LATITUDE AND LONGITUDE                  (DEFAULT)
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
C    IQCPOR - PROBABILITY OF RAIN QUALITY CONTROL SWITCH (QUIKSCAT AND
C              OSCAT ONLY)
C        IQCPOR = 0 -- NO QUALITY CONTROL W.R.T. PROBABILITY OF RAIN
C                      IS PERFORMED (SHOULD ALWAYS BE SET TO THIS FOR
C                      ASCAT)
C        IQCPOR = 1 -- ALL REPORTS WITH PROBABILITY OF RAIN GREATER
C                      THAN LIMIT SPECIFIED BY PORLIM ARE SKIPPED
C                                                              (DEFAULT)
C    PORLIM - IF IQCPOR=1, ALL REPORTS WITH PROBABILITY OF RAIN
C             GREATER THAN THIS VALUE ARE SKIPPED          (DEFAULT=.10)
CC
C    IQCEDG - ORBITAL SWATH EDGE QUALITY CONTROL SWITCH BASED ON CELL
C             NUMBER (QUIKSCAT AND OSCAT ONLY)
C        IQCEDG = 0 -- NO QUALITY CONTROL WITH RESPECT TO CELL NUMBER
C                      IS PERFORMED (SHOULD ALWAYS BE SET TO THIS FOR
C                      ASCAT)
C        IQCEDG = 1 -- ALL REPORTS WITH CELL NUMBER LESS THAN OR EQUAL
C                      TO LIMIT SPECIFIED BY IEDLLM OR GREATER THAN OR
C                      EQUAL TO LIMIT SPECIFIED BY IEDULM ARE
C                      CONSIDERED TO BE ON THE EDGE OF THE ORBITAL
C                      SWATH AND ARE SKIPPED (DEFAULT)
C    IEDLLM - IF IQCEDG=1, ALL REPORTS WITH CELL NUMBER LESS THAN OR
C             EQUAL TO THIS ARE SKIPPED (RANGE 1-72, MUST BE LESS THAN
C             IEDULM) (QUIKSCAT AND OSCAT ONLY)              (DEFAULT=8)
C    IEDULM - IF IQCEDG=1, ALL REPORTS WITH CELL NUMBER GREATER THAN OR
C             EQUAL TO THIS ARE SKIPPED (RANGE 1-72, MUST BE GREATER
C             THAN IEDLLM) (QUIKSCAT AND OSCAT ONLY)        (DEFAULT=64)
CC
C    IQCWVC - WIND VECTOR CELL QUALITY FLAG SWITCH (ASCAT ONLY)
C        IQCWVC = 0 -- THE WIND VECTOR CELL QUALITY FLAGS ARE NOT
C                      CHECKED (SHOULD ALWAYS BE SET TO THIS FOR
C                      QUIKSCAT OR OSCAT)                      (DEFAULT)
C        IQCPOR = 1 -- "CRITICAL" WIND VECTOR CELL QUALITY FLAGS ARE
C                      CHECKED AND REPORTS WITH ANY ONE OF THESE FLAGS
C                      SET ARE SKIPPED
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
C   MACHINE : NCEP WCOSS (iDataPlex and Cray-XC40)
C
C$$$

      PROGRAM WAVE_DCODQUIKSCAT

      PARAMETER (MXIB=31)

      LOGICAL NO_LATLON_CHECK
      logical  six,seven,eight,nine,fourteen 
      CHARACTER*8  CTYPE(3)
      DATA CTYPE/'QUIKSCAT',' ASCAT  ',' OSCAT  '/
      CHARACTER*6  CTYPE1(3)
      DATA CTYPE1/'QKSCAT','ASCATT','OSCATT'/
      CHARACTER*6  CTYPE2(3)
      DATA CTYPE2/'QKSWND','ASCATW','OSCATW'/
      CHARACTER*6  CTYPE3(3)
      DATA CTYPE3/'Q','A','O'/
      CHARACTER*8  SUBSET,SUBOUT,SID
      CHARACTER*30 DATSTR
      DATA DATSTR/'YEAR MNTH DAYS HOUR MINU SECO '/
      CHARACTER*27 QSCSTR
      DATA QSCSTR/'ATRN CTCN MWD10 MWS10 ISWV '/
      CHARACTER*10 LOCSTR
      DATA LOCSTR /'CLAT CLON '/
      CHARACTER*12 LOCSTRH
      DATA LOCSTRH/'CLATH CLONH '/
      CHARACTER*20 SCWSTR
      DATA SCWSTR /'WS10 WD10 BSCD LKCS '/
      CHARACTER*20 SCWSTRH
      DATA SCWSTRH/'WS10H WD10H NUL NUL '/
      CHARACTER*20 SCWSTRo
      DATA SCWSTRo/'WS10 WD10 NUL NUL '/
      CHARACTER*5  RAINST
      DATA RAINST/'SPRR '/
      CHARACTER*5  QUALST
      DATA QUALST/'WVCQ '/
      CHARACTER*5  SAIDST
      DATA SAIDST/'SAID '/
      CHARACTER*40 OUTST1
      DATA OUTST1/'YEAR MNTH DAYS HOUR MINU SECO CLAT CLON '/
      CHARACTER*40 OUTST2Q
      DATA OUTST2Q/'WS10 WD10 CTCN ATRN SPRR SAID ACAV RPID '/
      CHARACTER*45 OUTST2A
      DATA OUTST2A/'WS10 WD10 CTCN BSCD WVCQ SAID ACAV RPID LKCS '/

      INTEGER(4) LSTHXH_4(720,361)

      REAL(8) DATEIN_8(6),XIOLDT_8(8),QSCDAT_8(5),XLOCDT_8(2),
     $ WINDAT_8(4,255),QSCDT1_8(4,4),QSCDT2_8(7,4),XNUMDT_8(4),
     $ RAINDT_8,BRTDAT_8(3,2),ANPODT_8(6),ADATA_8(17),WS_8(4),WD_8(4),
     $ RPID_8,SAIDDT_8,QUALDT_8

      REAL(8) GETBMISS

      REAL XLOCDT(2),ADATA(17)

      REAL BLAT(360),BLON(720),SUMTIM(720,360),
     $ SUMLAT(720,360),SUMLON(720,360),
     $ SUMUCO(720,360),SUMVCO(720,360)

      INTEGER  NOBS(720,360),ICDATE(5),IDDATE(5),IDATE(8),
     $ NDATE(8),IBIT(MXIB),icount(23),ISWV_COUNT(5),ISWV_COUNT_good(4),
     $ ICNTS(3:4),NOBSS(720,360,3:4)

      NAMELIST/RDATA/ITYPE,IPRINT,ISATSK,ISUPOB,DELAT,DELON,LIMCNT,
     $ IQCPOR,PORLIM,IQCEDG,IQCWVC,IEDLLM,IEDULM,LATS,LATN,LONW,LONE

      DATA LUNIN /11/,LUNOUT/52/,LUNTAB/20/,ifirst/0/

      EQUIVALENCE  (SID,RPID_8)

      CALL W3TAGB('WAVE_DCODQUIKSCAT',2017,0012,1200,'NP22')

      PRINT *, ' '
      PRINT *, '=====> WELCOME TO PROGRAM WAVE_DCODQUIKSCAT - ',
     $ 'VERSION: 01/12/2017'
      PRINT *, ' '

      CALL SETBMISS(10E8_8)
      BMISS=GETBMISS()
      print'(1X)'
      print'(" BUFRLIB value for missing is: ",G0)', bmiss
      print'(1X)'

      ITYPE  = 1
      IPRINT = 0
      ISATSK = 99999
      ISUPOB = 1
      DELAT  = 1.0
      DELON  = 1.0
      LIMCNT = 1
      IQCPOR = 1
      PORLIM = 0.10
      IQCEDG = 1
      IQCWVC = 0
      LATS   = -90
      LATN   =  90
      LONW   = 360
      LONE   =   0

      READ(5,RDATA,END=1905)
 1905 CONTINUE
      IF(LIMCNT.LE.0)  LIMCNT = 1

      NO_LATLON_CHECK = (LATS.EQ.-90.AND.LATN.EQ.90.AND.LONW.EQ.
     $                   360.AND.LONE.EQ.0)

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
      NOBSS  = 0
      SUMTIM = 0
      SUMLAT = 0
      SUMLON = 0
      SUMUCO = 0
      SUMVCO = 0

      ICNT   = 0
      ICNTT  = 0
      ICNTS  = 0
      ITOBS  = 0

      IBADD  = 0
      IWVEC  = 0
      ISAID  = 0
      ISAIDSK  = 0
      IGRD   = 0
      ILALO  = 0
      ILAND  = 0
      IBADW  = 0
      IMSG   = 0
      IMSDM  = 0
      IMSSM  = 0
      IRAIN  = 0
      IEDGE  = 0
      isix   = 0
      iseven = 0
      ieight = 0
      inine  = 0
      ifourteen = 0
      icount = 0
      ISWV_COUNT = 0
      ISWV_COUNT_good = 0

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

      IF(ITYPE.EQ.1.OR.ITYPE.EQ.2.OR.ITYPE.EQ.3) THEN
         WRITE(6,600) CTYPE(ITYPE)
  600    FORMAT(/' GET ',A,' DATA FROM BUFR DATA DUMP'/)
      ELSE
         WRITE(6,*) 'INPUT TYPE (ITYPE) IS NOT RECOGNIZED (=',ITYPE,')',
     $    ' -- STOP 60'
         CALL W3TAGE('WAVE_DCODQUIKSCAT')
         CALL ERREXIT(60)
      END IF

      IRD = 0
      IGOOD=0

      CALL DATELEN(10)

C  GET THE CENTER & DUMP DATE FROM INPUT BUFR DATA DUMP FILE
C  ---------------------------------------------------------

      CALL DUMPBF(LUNIN,ICDATE,IDDATE)
      WRITE(6,*) ' '
      WRITE(6,*) 'From Original ',CTYPE(ITYPE),' Bufr Data Dump File ',
     $ 'in Unit ',lunin,' (',CTYPE1(ITYPE),'):'
      WRITE(6,*) '     - Center date (ICDATE) = ',ICDATE
      WRITE(6,*) '     - Dump date   (IDDATE) = ',IDDATE
      WRITE(6,*) 'Will transfer these to output reprocessed ',
     $ CTYPE(ITYPE),' BUFR file in Unit ',lunout,' (',CTYPE2(ITYPE),')'
      WRITE(6,*) ' '
      IF(ICDATE(1).LE.0)  THEN

C  IF CENTER DATE COULD NOT BE READ FROM FIRST DUMMY MESSAGE, STOP 61
C  ------------------------------------------------------------------

         WRITE(6,*) 'DUMPBF ERROR - CENTER DATE COULD NOT BE READ ',
     $    'FROM INPUT ',CTYPE(ITYPE),' DATA DUMP FILE -- STOP 61'
         CALL W3TAGE('WAVE_DCODQUIKSCAT')
         CALL ERREXIT(61)
      END IF
      IF(IDDATE(1).LE.0)  THEN

C  IF DUMP DATE COULD NOT BE READ FROM SECOND DUMMY MESSAGE, STOP 62
C  -----------------------------------------------------------------

         WRITE(6,*) 'DUMPBF ERROR - DUMP DATE COULD NOT BE READ ',
     $    'FROM INPUT ',CTYPE(ITYPE),' DATA DUMP FILE -- STOP 62'
         CALL W3TAGE('WAVE_DCODQUIKSCAT')
         CALL ERREXIT(62)
      END IF

      IDATBF =ICDATE(1)*1000000+ICDATE(2)*10000+ICDATE(3)*100+ICDATE(4)
      IDATDM =IDDATE(1)*1000000+IDDATE(2)*10000+IDDATE(3)*100+IDDATE(4)

C  OPEN THE INPUT BUFR DATA DUMP FILE
C  ----------------------------------

      CALL OPENBF(LUNIN,'IN',LUNIN)
         call openbf(0,'QUIET',1)

C  SEE WHAT THE BUFR MESSAGE TYPE IS (SUBSET)
C  ------------------------------------------

      IF(IREADMG(LUNIN,SUBSET,MDATE).NE.0) THEN
        PRINT *, '#####WAVE_DCODQUIKSCAT - NO DATA IN INPUT FILE - STOP'
         CALL W3TAGE('WAVE_DCODQUIKSCAT')
         CALL ERREXIT(00)
      ENDIF
      SUBOUT = SUBSET
      CALL CLOSBF(LUNIN)
      CALL OPENBF(LUNIN,'IN',LUNIN)


C  OPEN OUTPUT BUFR FILE WHICH WILL CONTAIN REPROCESSED QUIKSCAT, ASCAT
C   OR OSCAT DATA
C  --------------------------------------------------------------------

      CALL OPENBF(LUNOUT,'OUT',LUNTAB)
      WRITE(6,101) LUNOUT,LUNTAB
  101 FORMAT(/8X,'===> BUFR DATA SET IN UNIT',I3,' SUCCESSFULLY OPENED',
     $ ' FOR OUTPUT; BUFR MNEMONIC TABLES A,B,D IN UNIT',I3/13X,'READ ',
     $ 'IN AND ENCODED INTO BEGINNING MESSAGES OF OUTPUT DATA SET'/)

      PRINT 106, ITYPE,IPRINT,ISATSK,ISUPOB,DELAT,DELON,LIMCNT,IQCPOR,
     $ PORLIM,IQCEDG,IQCWVC,IEDLLM,IEDULM,LATS,LATN,LONW,LONE,
     $ NO_LATLON_CHECK
  106 FORMAT(/5X,'USER SPEC. SWITCHES: ITYPE=',I2,'; IPRINT=',I2,
     $ '; ISATSK=',I3,'; ISUPOB=',I2,'; DELAT=',F5.1,'; DELON=',F5.1,
     $ '; LIMCNT=',I3/5X,'IQCPOR=',I2,'; PORLIM=',F6.3,'; IQCEDG=',I2,
     $ '; IQCWVC=',I2,'; IEDLLM=',I2,'; IEDULM=',I2,'; LATITUDE BDRY:',
     $ I4,' TO',I4,' (S-N)'/5X,'LONGITUDE BDRY:',I4,' TO',I4,' (W-E) ',
     $ 'DEG. W.'/5X,'NO_LATLON_CHECK:',L4/)

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

C  READ THROUGH THE MESSAGES/SUBSETS IN THE FILE
C  ---------------------------------------------

      PRINT '(/"BEGIN READING IN DATA FROM UNIT ",I2)', LUNIN

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

            RAINDT_8 = BMISS
            QUALDT_8 = BMISS
            CALL UFBINT(LUNIN,QSCDAT_8, 5,  1,IRET,QSCSTR)
            IF(ITYPE.EQ.1 .OR. ITYPE.EQ.3)  THEN
               CALL UFBINT(LUNIN,XLOCDT_8, 2,  1,IRET,LOCSTR)
               IF(ITYPE.EQ.1)  THEN
                  CALL UFBREP(LUNIN,WINDAT_8, 4,255,INSWV,SCWSTRH)
               ELSE
                  CALL UFBREP(LUNIN,WINDAT_8, 4,255,INSWV,SCWSTRo)
               END IF
               CALL UFBINT(LUNIN,RAINDT_8, 1,  1,IRET,RAINST)
               IF(INSWV.NE.4)  THEN
                  PRINT '("NUMBER OF INPUT WIND VECTORS (",I5,") IS ",
     $             "NOT THE EXPECTED VALUE OF 4 - SKIP REPORT")',INSWV
                  IWVEC = IWVEC + 1
                  CYCLE LOOP1n1
               END IF
            ELSE
               CALL UFBINT(LUNIN,XLOCDT_8, 2,  1,IRET,LOCSTRH)
               CALL UFBINT(LUNIN,WINDAT_8, 4,255,INSWV,SCWSTR)
               CALL UFBINT(LUNIN,QUALDT_8, 1,  1,IRET,QUALST)
               IF(INSWV.NE.2 .AND. INSWV.NE.4)  THEN
                  PRINT '("NUMBER OF INPUT WIND VECTORS (",I5,") IS ",
     $             "NOT THE EXPECTED VALUE OF 2 OR 4 - SKIP REPORT")',
     $             INSWV
                  IWVEC = IWVEC + 1
                  CYCLE LOOP1n1
               END IF
            END IF
            XLOCDT = XLOCDT_8
            CALL UFBINT(LUNIN,SAIDDT_8, 1,  1,IRET,SAIDST)
            SAIDDT = SAIDDT_8

C  CHECK FOR RECOGNIZABLE SAT. ID & TAG USING CHARACTER 8 OF REPORT ID
C  -------------------------------------------------------------------

            IF(ITYPE.EQ.1.AND.NINT(SAIDDT).NE.281)  THEN
               if(isaid.le.100) then
                  PRINT '("SATELLITE ID (",I5,") IS NOT RECOGNIZED FOR",
     $             " QUIKSCAT REPORTS - SKIP REPORT")',NINT(SAIDDT)
               else if(isaid.eq.101) then
                  print '("===> THERE ARE > 100 QUIKSCAT REPORTS WITH ",
     $             "AN UNRECOGNIZED SATELLITE ID - ONLY 1ST 100 ARE ",
     $             "PRINTED")'
               endif
               ISAID = ISAID + 1
               CYCLE LOOP1n1
            ELSE IF(ITYPE.EQ.2.AND.(NINT(SAIDDT).NE.004.AND.
     $                              NINT(SAIDDT).NE.003))  THEN
               if(isaid.le.100) then
                  PRINT '("SATELLITE ID (",I5,") IS NOT RECOGNIZED FOR",
     $             " ASCAT REPORTS - SKIP REPORT")',NINT(SAIDDT)
               else if(isaid.eq.101) then
                  print '("===> THERE ARE > 100 ASCAT REPORTS WITH ",
     $             "AN UNRECOGNIZED SATELLITE ID - ONLY 1ST 100 ARE ",
     $             "PRINTED")'
               endif
            ELSE IF(ITYPE.EQ.3.AND.NINT(SAIDDT).NE.421)  THEN
               if(isaid.le.100) then
                  PRINT '("SATELLITE ID (",I5,") IS NOT RECOGNIZED FOR",
     $             " OSCAT REPORTS - SKIP REPORT")',NINT(SAIDDT)
               else if(isaid.eq.101) then
                  print '("===> THERE ARE > 100 OSCAT REPORTS WITH ",
     $             "AN UNRECOGNIZED SATELLITE ID - ONLY 1ST 100 ARE ",
     $             "PRINTED")'
               endif
               ISAID = ISAID + 1
               CYCLE LOOP1n1
            ENDIF

C  CHECK TO SEE IF THIS REPORT SHOULD BE SKIPPED BASED ON ITS SAT. ID
C  ------------------------------------------------------------------

            IF(NINT(SAIDDT).EQ.ISATSK)  THEN
               ISAIDSK = ISAIDSK + 1
               CYCLE LOOP1n1
            ENDIF
       
            SID(8:8) = CTYPE3(ITYPE)

C  CHECK REPORT WITH INVALID LAT/LON LOCATION
C  ------------------------------------------

            YY = XLOCDT(1) + 91.5
            XX = XLOCDT(2) + 181.5
            IHX = 2.*XX - 1.0
            IF(IHX.EQ.721) IHX=1
            IHY = 2.*YY - 1.0
            SLA = XLOCDT(1) + 91.0
            SLO = XLOCDT(2) + 1.0
            IF(XLOCDT(2).LT.0.0) SLO = XLOCDT(2) + 361.0
            IF(SLA.LT.1.OR.SLA.GT.181.0.OR.SLO.LT.1.0.OR.SLO.GT.361.0)
     $       THEN
               IGRD = IGRD + 1
               CYCLE LOOP1n1
            ENDIF

C  STORE LATITUDE IN INLAT: INPUT: N(+), S(-);  OUTPUT: N(+), S(-) X 100
C  ---------------------------------------------------------------------

            INLAT = NINT(XLOCDT(1) * 100.)

C  STORE LON. IN IWLON: INPUT: 0-360 E; OUTPUT: 0-360 W X 100
C  -----------------------------------------------------------------

            IWLON = MOD((36000-NINT(XLOCDT(2)*100.)),36000)

            IF(.NOT. NO_LATLON_CHECK)  THEN

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
               IF(INLAT.LT.LATS*100.OR.INLAT.GE.LATN*100.OR.IWLONT
     $          .GT.LONWT*100.OR.IWLONT.LE.LONE*100)  THEN
                  ILALO = ILALO + 1
                  CYCLE LOOP1n1
               END IF
            END IF

C  CHECK REPORT FOR OVER LAND
C  --------------------------

            IF(LSTHXH_4(IHX,IHY).NE.0) THEN
               ILAND = ILAND + 1
               CYCLE LOOP1n1
            ENDIF

C  GET "NUDGED" WIND VECTOR SELECTION
C  ----------------------------------

            ISWV = QSCDAT_8(5)
            IF(IBFMS(QSCDAT_8(5)).NE.0)  ISWV = 5
            IF(ISWV.LE.0.OR.ISWV.GT.INSWV) THEN
               IF(ISWV.EQ.5)  ISWV_COUNT(ISWV) = ISWV_COUNT(ISWV) + 1
               IBADW = IBADW + 1
               CYCLE LOOP1n1
            ENDIF
            ISWV_COUNT(ISWV) = ISWV_COUNT(ISWV) + 1

C  CHECK THAT SELECTED NUDGED WIND VECTOR IS VALID
C  -----------------------------------------------

            WS_8 = BMISS
            WD_8 = BMISS
            DO J=1,INSWV
               WS_8(J) = WINDAT_8(1,J)
               WD_8(J) = WINDAT_8(2,J)
            ENDDO
            IF(MIN(WS_8(ISWV),WD_8(ISWV)).GE.999.0_8) THEN
               IMSG = IMSG + 1
               CYCLE LOOP1n1
            END IF

C  CHECK THAT MODEL WIND DIRECTION AND SPEED ARE VALID
C  ---------------------------------------------------

            DM    = QSCDAT_8(3)
            SM    = QSCDAT_8(4)

            IF(DM.LT.0.0.OR.DM.GT.360)  THEN
               IMSDM = IMSDM + 1
               CYCLE LOOP1n1
            END IF
            IF(SM.LT.0.0.OR.SM.GT.50.0) THEN
               IMSSM = IMSSM + 1
               CYCLE LOOP1n1
            END IF

            six      = .false.
            seven    = .false.
            eight    = .false.
            nine     = .false.
            fourteen = .false.

            IF(ITYPE.EQ.1 .OR. ITYPE.EQ.3)  THEN

               IF(IQCPOR.EQ.1)  THEN

C  CHECK THAT THE PROBABILITY OF RAIN IS LESS THAN OR EQUAL TO PORLIM
C   (MISSING PROBABILITY OF RAIN IS TREATED AS POB 100%)
C   (QUIKSCAT AND OSCAT ONLY)
C  ------------------------------------------------------------------

                  IF(RAINDT_8.GT.PORLIM)  THEN
                     IRAIN = IRAIN + 1
                     CYCLE LOOP1n1
                  END IF
               END IF

               IF(IQCEDG.EQ.1)  THEN

C  CHECK THAT THE REPORT IS NOT ON THE EDGE OF THE ORBITAL SWATH
C   AS DEFINED BY ITS CELL NUMBER AND THE SPECIFIED LIMITS
C   (QUIKSCAT AND OSCAT ONLY)
C  -------------------------------------------------------------

                  IF(NINT(QSCDAT_8(2)).LE.IEDLLM.OR.NINT(QSCDAT_8(2))
     $             .GE.IEDULM) THEN
                     IEDGE = IEDGE + 1
                     CYCLE LOOP1n1
                  END IF
               END IF

            ELSE

               IF(IBFMS(QUALDT_8).EQ.0)  THEN
                  CALL UPFTBV(LUNIN,'WVCQ',QUALDT_8,MXIB,IBIT,NIB)

C  The Wind Vector Cell Quality Flag is currently not being encoded
C   properly in BUFR - must subtract 1 from each bit flag
C  -----------------------------------------------------------------

                  if(nib.gt.0)  then
                    do ii=1,nib
                       if(ibit(ii).gt.1) ibit(ii) = ibit(ii) - 1
                       if(ibit(ii).le.23) icount(ibit(ii)) =
     $                  icount(ibit(ii)) + 1
                       if(ibit(ii).eq. 6)  six      = .true.
                       if(ibit(ii).eq. 7)  seven    = .true.
                       if(ibit(ii).eq. 8)  eight    = .true.
                       if(ibit(ii).eq. 9)  nine     = .true.
                       if(ibit(ii).eq.14)  fourteen = .true.
                    enddo
                  end if
                  IF(IQCWVC.EQ.1)  THEN

C  CHECK THAT NO "CRITICAL" WIND VECTOR CELL QUALITY FLAGS ARE SET
C   (ASCAT ONLY)
C  ---------------------------------------------------------------

                     if(six) then
                        isix = isix + 1
                        cycle LOOP1n1
                     else  if(seven) then
                        iseven = iseven + 1
                        cycle LOOP1n1
                     else  if(eight) then
                        ieight = ieight + 1
                        cycle LOOP1n1
                     else  if(nine) then
                        inine = inine + 1
                        cycle LOOP1n1
                     else  if(fourteen) then
                        ifourteen = ifourteen + 1
                        cycle LOOP1n1
                     end if
                  END IF
               END IF

            END IF

            IGOOD = IGOOD + 1
            ISWV_COUNT_good(ISWV) = ISWV_COUNT_good(ISWV) + 1

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

C  FIND CORRESPONDING U-COMP AND V-COMP OF WIND FROM SPEED & DIRECTION
C  -------------------------------------------------------------------

               WDIR = WD_8(ISWV)
               WSPD = WS_8(ISWV)
               CALL W3FC06(WDIR,WSPD,UCOMP,VCOMP)

C  SUM UP VALUES WITHIN THE GRID BOX ILM, JLM
C  ------------------------------------------

               NOBS(JLM,ILM) = NOBS(JLM,ILM)+1
               IF(ITYPE.EQ.2)  NOBSS(JLM,ILM,NINT(SAIDDT)) =
     $                         NOBSS(JLM,ILM,NINT(SAIDDT)) + 1

               ITOBS =
     $          ITOBS + 1  ! this line may not be needed, DAK
               SUMTIM(JLM,ILM) =
     $          SUMTIM(JLM,ILM) + REAL(ITIME)
               SUMLAT(JLM,ILM) =
     $          SUMLAT(JLM,ILM) + XLOCDT(1)
               SUMLON(JLM,ILM) =
     $          SUMLON(JLM,ILM) + XLOCDT(2)
               SUMUCO(JLM,ILM) =
     $          SUMUCO(JLM,ILM) + UCOMP
               SUMVCO(JLM,ILM) =
     $          SUMVCO(JLM,ILM) + VCOMP

            ELSE 

C*****************************************************************
C      REPORT PASSED ALL CHECKS - COME HERE IF NOT SUPEROBING
C*****************************************************************

C  BUILD AN INTERMEDIATE ARRAY (ADATA_8) CONTAINING THE DATA
C  ---------------------------------------------------------

               ADATA_8       = BMISS
               ADATA_8(1:6)  = DATEIN_8(1:6)
               ADATA_8(7:8)  = XLOCDT_8(1:2)
               ADATA_8(9)    = WS_8(ISWV)
               ADATA_8(10)   = WD_8(ISWV)
               ADATA_8(11)   = QSCDAT_8(2)
               IF(ITYPE.EQ.1 .OR. ITYPE.EQ.3)  THEN
                  ADATA_8(12)   = QSCDAT_8(1)
                  ADATA_8(13)   = RAINDT_8
               ELSE
                  ADATA_8(12)   = WINDAT_8(3,ISWV)
                  ADATA_8(13)   = QUALDT_8
                  ADATA_8(17)   = WINDAT_8(4,ISWV)
               END IF
               ADATA_8(14)   = SAIDDT_8
               ADATA_8(15)   = 1_8
               ICNT = MIN(9999999,ICNT+1)
               ICNTT = ICNTT + 1
               IF(ITYPE.EQ.2) ICNTS(NINT(SAIDDT))=ICNTS(NINT(SAIDDT))+ 1
               WRITE(SID(1:7),'(I7.7)')  ICNT
               ADATA_8(16)   = RPID_8

               IF(IPRINT.EQ.1)  THEN
                  IF(ITYPE.EQ.1 .OR. ITYPE.EQ.3)  THEN
                     if(ifirst.eq.0) then
                        WRITE(51,9601)
 9601 FORMAT(' YEAR MM DD HH mm ss   CLAT    CLON     WS10   WD10   ',
     $ 'CTCN  ATRN     SPRR  SAID ACAV  RPID'/' ---- -- -- -- -- --  ',
     $ '------ -------   -----  ------  ----  ----    -----  ---- ----',
     $ '  --------')
                        ifirst = 1
                     end if
                     WRITE(51,8601)  (NINT(ADATA_8(II)),II=1,6),
     $                (ADATA_8(II),II=7,10),NINT(ADATA_8(11)),
     $                NINT(ADATA_8(12)),ADATA_8(13),NINT(ADATA_8(14)),
     $                NINT(ADATA_8(15)),SID
 8601 FORMAT(1X,I4,5I3.2,4F8.2,2(1X,I5),2X,F7.3,3X,I3,3X,I2,2X,A8)
                  ELSE
                     if(ifirst.eq.0) then
                        WRITE(51,9602)
 9602 FORMAT(' YEAR MM DD HH mm ss   CLAT    CLON    WS10   WD10     ',
     $ 'CTCN    BSCD      WVCQ                   SAID ACAV  LKCS    ',
     $ 'RPID'/' ---- -- -- -- -- --   ----- -------   -----  ------   ',
     $ '----    ----      ----                   ---- ----  ------  ',
     $ '--------')
                        ifirst = 1
                     end if
                     if(nib.gt.0) then
                        WRITE(51,8602)  (NINT(ADATA_8(II)),II=1,6),
     $                   (ADATA_8(II),II=7,10),NINT(ADATA_8(11)),
     $                   ADATA_8(12),NINT(ADATA_8(13)),
     $                   (IBIT(II),II=1,NIB),NINT(ADATA_8(14)),
     $                   NINT(ADATA_8(15)),ADATA_8(17),SID
 8602 FORMAT(1X,I4,5I3.2,4F8.2,2X,I5,2X,F6.1,1X,I9,1X,'(',<NIB>I3,')',
     $ T98,I3,2X,I2,2X,F7.3,2X,A8)
                     else
                        WRITE(51,8603)  (NINT(ADATA_8(II)),II=1,6),
     $                   (ADATA_8(II),II=7,10),NINT(ADATA_8(11)),
     $                   ADATA_8(12),NINT(ADATA_8(13)),
     $                   NINT(ADATA_8(14)),NINT(ADATA_8(15)),
     $                   ADATA_8(17),SID
 8603 FORMAT(1X,I4,5I3.2,4F8.2,2X,I5,2X,F6.1,1X,I9,T98,I3,2X,I2,2X,F7.3,
     $ 2X,A8)
                     end if
                  END IF
               END IF

C  ENCODE THE REPROCESSED QUIKSCAT, ASCAT OR OSCAT WIND DATA INTO A
C   SUBSET (REPORT)
C  ----------------------------------------------------------------

               CALL UFBINT(LUNOUT,ADATA_8,8,1,IRET,OUTST1)
               IF(ITYPE.EQ.1 .OR. ITYPE.EQ.3)  THEN
                  CALL UFBINT(LUNOUT,ADATA_8(9),8,1,IRET,OUTST2Q)
               ELSE
                  CALL UFBINT(LUNOUT,ADATA_8(9),9,1,IRET,OUTST2A)
               END IF

C  WRITE THE ENCODED SUBSET (REPORT) INTO THE OUTPUT BUFR MESSAGE
C  --------------------------------------------------------------

               CALL WRITSB(LUNOUT)
               CALL UFBCNT(LUNOUT,IREC,ISUB)
               IF(IREC.GT.IRECL)  THEN
                  ISUBT = ISUBT + ISUBL
                  WRITE(6,1254) IREC-1,ISUBL,ISUBT
 1254 FORMAT(/' --- THIS REPORT OPENS NEW BUFR MSG: LAST MSG WAS NO.',
     $ I10,' (DATA) WITH',I5,' RPTS (TOTAL NO. RPTS WRITTEN=',I7,')'/)
               END IF
               ISUBL = ISUB
               IRECL = IREC

C*****************************************************************
            END IF

         ENDDO LOOP1n1

      ENDDO LOOP1

      PRINT '(/"DONE READING IN DATA FROM UNIT ",I2)', LUNIN

      IF(ISUPOB.EQ.1) THEN 

C***********************************************************************
C                IF SUPEROBING COME HERE
C***********************************************************************

         CALL OPENMG(LUNOUT,SUBOUT,IDATBF)

         ITOBS = 0

C-----------------------------------------------------------------------
C    LOOP THRU BOXES: CONSTRUCT MEANS & WRITE SUPEROB TO ADATA ARRAY
C-----------------------------------------------------------------------

         LATBEG = 1
         KNDX = 0
         LOOP2: DO KI = LATBEG,LATSIZ
            KNDX = KNDX + 1
            LOOP2n1: DO KJ = 1,LONSIZ

C  IF LESS THAN 'LIMCNT' INDIVIDUAL REPORTS WENT INTO MAKING SUPEROB
C   IN THIS BOX, SKIP PROCESSING THE SUPEROB FOR THIS BOX
C  -----------------------------------------------------------------

               IF(NOBS(KJ,KNDX).LT.LIMCNT)  CYCLE LOOP2n1
               ITOBS = ITOBS + NOBS(KJ,KNDX)
               IF(ITYPE.EQ.2)  THEN
                  DO ISAT = 3,4
                     ICNTS(ISAT) = ICNTS(ISAT) + NOBSS(KJ,KNDX,ISAT)
                  ENDDO
               END IF
               XMUL = REAL(NOBS(KJ,KNDX))

C  CALC. MEAN WIND SPEED, WIND DIRECTION, TIME, LATITUDE AND
C   LONGITUDE WITHIN EACH GRID BOX
C  ---------------------------------------------------------

               AVGUCO = SUMUCO(KJ,KNDX)/XMUL
               AVGVCO = SUMVCO(KJ,KNDX)/XMUL
               AVGTIM = SUMTIM(KJ,KNDX)/XMUL
               AVGLAT = SUMLAT(KJ,KNDX)/XMUL
               AVGLON = SUMLON(KJ,KNDX)/XMUL

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
     $          REAL(IDATE(6))/60. + REAL(IDATE(7))/3600.))
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
               ADATA(9) = AVGWS
               ADATA(10)= AVGWD
               IF(ITYPE.NE.2)  ADATA(14)= SAIDDT_8
               ADATA(15)= NOBS(KJ,KNDX)
               ICNT = MIN(999999,ICNT+1)
               ICNTT = ICNTT + 1
               WRITE(SID(2:7),'(I6.6)')  ICNT
               ADATA_8(16) = RPID_8
               ADATA_8(1:15) = ADATA(1:15)
               ADATA_8(17) = ADATA(17)

               IF(IPRINT.EQ.1)  THEN
                  IF(ITYPE.EQ.1 .OR. ITYPE.EQ.3)  THEN
                     if(ifirst.eq.0) then
                        WRITE(51,9601)
                        ifirst = 1
                     end if
                     WRITE(51,8601)  (NINT(ADATA_8(II)),II=1,6),
     $                (ADATA_8(II),II=7,10),NINT(ADATA_8(11)),
     $                NINT(ADATA_8(12)),ADATA_8(13),NINT(ADATA_8(14)),
     $                NINT(ADATA_8(15)),SID
                  ELSE
                     if(ifirst.eq.0) then
                        WRITE(51,9602)
                        ifirst = 1
                     end if
                     WRITE(51,8603)  (NINT(ADATA_8(II)),II=1,6),
     $                (ADATA_8(II),II=7,10),NINT(ADATA_8(11)),
     $                ADATA_8(12),NINT(ADATA_8(13)),NINT(ADATA_8(14)),
     $                NINT(ADATA_8(15)),ADATA_8(17),SID
                  END IF
               END IF

C  ENCODE THE REPROCESSED QUIKSCAT, ASCAT OR OSCAT WIND DATA INTO A
C   SUBSET (REPORT)
C  ----------------------------------------------------------------

               CALL UFBINT(LUNOUT,ADATA_8,8,1,IRET,OUTST1)
               IF(ITYPE.EQ.1 .OR. ITYPE.EQ.3)  THEN
                  CALL UFBINT(LUNOUT,ADATA_8(9),8,1,IRET,OUTST2Q)
               ELSE
                  CALL UFBINT(LUNOUT,ADATA_8(9),9,1,IRET,OUTST2A)
               END IF

C  WRITE THE ENCODED SUBSET (REPORT) INTO THE OUTPUT BUFR MESSAGE
C  --------------------------------------------------------------

               CALL WRITSB(LUNOUT)
               CALL UFBCNT(LUNOUT,IREC,ISUB)
               IF(IREC.GT.IRECL)  THEN
                  ISUBT = ISUBT + ISUBL
                  WRITE(6,1254) IREC-1,ISUBL,ISUBT
               END IF
               ISUBL = ISUB
               IRECL = IREC

            ENDDO LOOP2n1
         ENDDO LOOP2
C***********************************************************************

      END IF

C  CLOSE INPUT AND OUTPUT NCEP OUIKSCAT, ASCAT OR OSCAT BUFR DATA SETS
C  -------------------------------------------------------------------
                                                                        
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

      WRITE(6,699) CTYPE(ITYPE),IRD,IBADD,IWVEC,ISAID,ISATSK,ISAIDSK,
     $ IGRD,ILALO,ILAND,IBADW,IMSG,IMSDM,IMSSM,NINT(PORLIM*100.),IRAIN,
     $ IEDGE,isix,iseven,ieight,inine,ifourteen,CTYPE(ITYPE),IGOOD
  699 FORMAT(//5X,'*** PROCESSING ENDED NORMALLY ***'//
     $ ' >>>  TOTAL NUMBER OF INPUT ',A,' REPORTS READ ',26('.'),I7,
     $ '  <<<'//
     $ 12X,'- Number skipped due to invalid time stamp ',21('.'),I7//
     $ 12X,'- Number skipped due to unexpected # input wind vectors',
     $ 9('.'),I7//
     $ 12X,'- Number skipped due to unrecognized satellite id ',14('.'),
     $ I7//
     $ 12X,'- Number skipped due to having satellite id ',I3,1X,16('.'),
     $ I7//
     $ 12X,'- Number skipped due to invalid lat/lon location ',15('.'),
     $ I7//
     $ 12X,'- Number skipped due to being outside lat/lon domain ',
     $ 11('.'),I7//
     $ 12X,'- Number skipped due to being over land ',24('.'),I7//
     $ 12X,'- Number skipped due to invalid wind vector index ',14('.'),
     $ I7//
     $ 12X,'- Number skipped due to missing wind ',27('.'),I7//
     $ 12X,'- Number skipped due to missing model wind direction ',
     $ 11('.'),I7//
     $ 12X,'- Number skipped due to missing model wind speed ',15('.'),
     $ I7//
     $ 12X,'- Number skipped due to prob. of rain >',I3,'% ',20('.'),
     $ I7//
     $ 12X,'- Number skipped due to being on edge of orbital swath ',
     $ 9('.'),I7//
     $ 12X,'- Number skipped due to WVCQ flag 6 (KNMI q.c. fails) ',
     $ 10('.'),I7//
     $ 12X,'- Number skipped due to WVCQ flag 7 (variational q.c. ',
     $ 'fails) ',3('.'),I7//
     $ 12X,'- Number skipped due to WVCQ flag 8 (over land) ',16('.'),
     $ I7//
     $ 12X,'- Number skipped due to WVCQ flag 9 (over ice) ',17('.'),
     $ I7//
     $ 12X,'- Number skipped due to WVCQ flag 14 (rain) ',20('.'),
     $ I7//
     $ 12X,'- NUMBER OF INPUT ',A,' REPORTS PASSING ALL CHECKS ',
     $ 10('.'),I7/)

      print *
      do ii=1,23
         if(icount(ii).gt.0)  print *, 'WVCQ: # rpts passing all ',
     $    'prior checks with bit ',ii,' on = ',icount(ii)
      enddo

      print *
      do ii=1,4
         print *, '# input oceanic rpts with ISWV = ',ii,' is ',
     $    ISWV_COUNT(ii)
      enddo
      print *, '# input oceanic rpts w/ missing ISWV is ',ISWV_COUNT(5)
      print *
      do ii=1,4
         print *, '# good  oceanic rpts with ISWV = ',ii,' is ',
     $    ISWV_COUNT_good(ii)
      enddo
      print *

      IF(ISUPOB.EQ.1)  THEN

C  IF SUPEROBING WAS DONE, PRODUCE FINAL COUNTS OF ORIGINAL REPORTS
C  ----------------------------------------------------------------

         PRINT 114
  114 FORMAT(/30X,'+++ NO. OF ORIGINAL REPORTS THAT WERE USED TO ',
     $ 'GENERATE SUPEROBS +++'/)
         IF(ITYPE.NE.2) THEN
            PRINT 214, NINT(SAIDDT),ITOBS
  214 FORMAT(45X,'FROM SATELLITE ID',I4,' ...... ',I10)
         ELSE
            DO ISAT = 3,4
               IF(ICNTS(ISAT).GT.0) PRINT 214, ISAT,ICNTS(ISAT)
            ENDDO
            PRINT 215, ICNTS(3)+ICNTS(4)
  215 FORMAT(/30X,'TOTAL ..................... ',I10)
         END IF
         PRINT *
cdak     PRINT 115
  115    FORMAT(5X,'NOBS:')
cdak     PRINT 116, NOBS
  116    FORMAT(1X,30I4)
         PRINT 110, CTYPE(ITYPE),ICNTT,
     $    (1.-FLOAT(ICNTT)/FLOAT(ITOBS))*100.
  110 FORMAT(/27X,'+++++++++++++++++++++++++++++++++++++++++++++++++',
     $ '++++++++++++++++++++++++++'//
     $ ' >>>  TOTAL NUMBER OF REPROCESSED ',A,' WIND REPORTS ',
     $ 'WRITTEN ..',I7,'  <<<'//
     $ 'percentage compression is ',F6.2,'%'//
     $ 44X,'*****  PROGRAM SUCCESSFULLY COMPLETED  *****'/)

      ELSE

         PRINT 111, CTYPE(ITYPE)
  111 FORMAT(/27X,'+++  NUMBER OF REPROCESSED ',A,' WIND REPORTS ',
     $ 'WRITTEN TO OUTPUT FILE +++'/)
         IF(ITYPE.NE.2) THEN
            PRINT 211, NINT(SAIDDT),ICNTT
  211 FORMAT(40X,'>>>  FROM SATELLITE ID',I4,' ...... ',I7,'  <<<')
         ELSE
            DO ISAT = 3,4
               IF(ICNTS(ISAT).GT.0) PRINT 211, ISAT,ICNTS(ISAT)
            ENDDO
         END IF
         PRINT 212, CTYPE(ITYPE),ICNTT
  212 FORMAT(/' >>>  TOTAL NUMBER OF REPROCESSED ',A,' WIND REPORTS ',
     $ 'WRITTEN ..',I7,'  <<<'//
     $ 44X,'*****  PROGRAM SUCCESSFULLY COMPLETED  *****'/)

      END IF

      CALL W3TAGE('WAVE_DCODQUIKSCAT')

      STOP                                                              

C-----------------------------------------------------------------------

 9999 CONTINUE

C  IF ERROR READING LAND/SEA MASK (OR EMPTY OR INCOMPLETE), STOP 55
C  ----------------------------------------------------------------

      WRITE(6,*) 'ERROR READING 0.5 x 0.5 DEG LAND/SEA MASK (MAY BE ',
     $ 'EMPTY OR INCOMPLETE AS WELL)  -- STOP 55'
      CALL W3TAGE('WAVE_DCODQUIKSCAT')
      CALL ERREXIT(55)

      END 
