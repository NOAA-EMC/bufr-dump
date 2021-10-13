C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_GEOFIL
C   PRGMMR: DONG             ORG: NP22        DATE: 2020-08-20
C
C ABSTRACT: GEOGRAPHICALLY FILTERS BUFR DATABASE DUMP FILES BY EITHER
C   A LAT/LON BOX FILTER, A CENTER POINT LAT/LON AND RADIUS FILTER OR A
C   0.5 DEGREE GLOBAL LAT/LON GRID POINT MASK FILE.  WILL WORK WITH
C   COMPRESSED AS WELL AS UNCOMPRESSED BUFR MESSAGES.  THE FILE PATH/
C   NAMES OF THE INPUT AND OUTPUT FILES AND THE GEOGRAPHICAL FILTERING
C   PARAMETERS ARE READ FROM STANDARD INPUT (UNIT 5) AT THE START OF
C   THIS PROGRAM.  ALL OTHER FILE CONNECTIONS ARE MADE THROUGH THE
C   FORTRAN OPEN STATEMENT.
C
C PROGRAM HISTORY LOG:
C 1999-06-04  L. SAGER    MODIFIED DUPMAR & EDTBUF TO CREATE THIS NEW
C     CODE
C 2000-06-07  D. KEYSER   INCREASED THE SIZE OF PARAMETER MXTB FROM
C     100K to 800K FOR DUMPS WITH MANY MORE REPORTS (E.G., QUIKSCAT
C     DATA)
C 2000-12-05  D. KEYSER   INCREASED LIMIT FOR I/O FILENAME LENGTH FROM
C     80 CHARACTERS TO 500 CHARACTERS
C 2001-08-21  D. KEYSER   INCREASED MXTB FROM 800000 TO 2400000 TO
C     HANDLE LARGE VOLUME GOES SFOV CLOUD-TOP PROCESSING
C 2002-03-05  D. KEYSER   IMPROVED DOCUMENTATION; IMPROVED STANDARD
C     OUTPUT PRINT; ADDED CALL TO COMPRESS_CHECK TO INDICATE IF INPUT/
C     OUTPUT FILES ARE COMPRESSED OR UNCOMPRESSED, IF COMPRESSED
C     PROGRAM EXITS NORMALLY BUT CANNOT FILTER; CORRECTED LOGIC FOR
C     LAT/LON CIRCLE FILTER CASE (STILL NOT TESTED/READY)
C 2003-12-11  D. KEYSER   CHECKS IF FIRST SUBSET HAS MISSING LOW-
C     ACCURACY LAT (CLAT) - IF SO ASSUMES ALL REPORTS ENCODE HI-
C     ACCURACY LAT/LON (CLATH/ CLONH) (NEEDED BECAUSE MESONETS ARE
C     CHANGING TO HI-ACCURACY LAT/LON IN 2004 AND OTHER TYPES MAY
C     FOLLOW)
C 2004-02-02  D. KEYSER   REPLACED CALL TO IN-LINE SUBROUTINE
C     COMPRESS_CHECK WITH CALL TO NEW BUFRLIB ROUTINE MESGBC; CORRECTED
C     SUBROUTINE ARGUMENT MISALIGNMENT IN SUBROUTINE CHDIST (THIS DID
C     NOT APPEAR TO CAUSE PROBLEMS IN OUTPUT BUT COULD CAUSE MEMORY
C     CLOBBERING DOWN THE LINE SOMEWHERE)
C 2005-10-19  D. KEYSER   INTRODUCED ALLOCATABLE ARRAYS TO AVOID ARRAY
C     OVERFLOW PROBLEMS, DETERMINES SIZE OF ARRAYS BY CALLING UFBTAB
C     WITH NEGATIVE UNIT NUMBER TO SIMPLY COUNT SUBSETS; CAN NOW
C     HANDLE DATA TYPES STORED IN COMPRESSED BUFR MESSAGES {I.E,
C     AIRNOW - NOTE THAT SATELLITE TYPES IN COMPRESSED BUFR MESSAGES
C     (I.E., AIRS, AMSR-E, ATOVS 1B, AVHRR) ARE NOW GEOGRAPHICALLY
C     FILTERED DIRECTLY IN BUFR_DUPSAT RATHER THAN HERE}, DONE THROUGH
C     TEMPORARY IN-LINE VERSIONS OF UFBTAB AND COPYSB WHICH CAN NOW
C     HANDLE COMPRESSED MESSAGES (THESE WILL BE IMPLEMENTED IN NEXT
C     VERSION OF BUFRLIB AFTERWHICH THEY SHOULD BE REMOVED HERE)
C 2006-03-02  D. KEYSER   CORRECTED BUG WHICH OPENED THE INPUT BUFR
C     FILE WITH THE PROPER INPUT FILENAME BUT WITH THE FILENAME HAVING
C     THE LENGTH (I.E, NUMBER OF CHARACTERS) OF THE OUTPUT FILENAME
C     (DID NOT CAUSE A PROBLEM IN PRODUCTION BECAUSE BOTH FILENAMES ARE
C     THE SAME LENGTH); REMOVES IN-LINE VERSIONS OF UFBTAB AND COPYSB
C     (UPDATED TO HANDLE COMPRESSED BUFR MESSAGES IN 1/31/2006 VERSION
C     OF BUFRLIB)
C 2007-03-23  D. KEYSER   CORRECTED METHOD OF CALLING UFBTAB WITH
C     NEGATIVE UNIT NUMBER TO COUNT SUBSETS FOR USE WITH EXISTING
C     ALLOCATABLE ARRAYS, PREVIOUS LOGIC COULD HAVE READ TO ARRAY
C     OVERFLOW PROBLEMS - STREAMLINED THIS LOGIC AS WELL
C 2007-06-13  D. KEYSER   ADDED NEW OPTION TO FILTER VIA THE USE OF A
C     0.5 DEGREE GLOBAL LAT/LON GRID POINT MASK FILE (SEE REMARKS FOR
C     MORE INFORMATION)
C 2012-11-20  J. WOOLLEN  INITIAL PORT TO WCOSS -- NO LOGIC CHANGES
C 2013-01-13  J. WHITING  FINAL PORT TO WCOSS -- UPDATED DOC BLOCKS;
C     REPLACED TESTS VS BMISS W/ IBFMS FUNCTION; REPLACED EXPLICIT
C     ASSIGNMENT OF BMISS W/ GETBMISS() FUNCTION.
C 2014-11-07  D. KEYSER   DECLARE FUNCTION GETBMISS AND ITS RETURN
C     VALUE BMISS AS REAL*8 TO GET A RELIABLE VALUE FOR BMISS IN PRINT
C     STATEMENTS
C 2020-08-20  J. DONG  --  ADDED SETBMISS CALL TO SET BMISS TO 10E8
C     TO AVOID INTEGER OVERFLOW
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT - FIRST RECORD CONTAINS INPUT FILE
C                NAME, SECOND RECORD CONTAINS OUTPUT FILE NAME,
C                THIRD RECORD CONTAINS GEOGRAPHICAL FILTERING
C                PARAMETERS (SEE REMARKS)
C     UNIT 20  - UNFILTERED BUFR DUMP FILE
C
C   OUTPUT FILES:
C     UNIT 20  - GEOGRAPHICALLY FILTERED BUFR DUMP FILE
C     UNIT 50  - WORKSPACE (SCRATCH) FILE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE     - CHDIST  GEOCHK
C     LIBRARY:
C       W3NCO    - W3TAGB  W3TAGE ERREXIT
C       BUFRLIB  - DATELEN OPENBF COPYMG UFBTAB OPENMB COPYSB COPYBF
C                  CLOSMG  CLOSBF MESGBC IBFMS  GETBMISS
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C            > 0 - ABNORMAL RUN
C
C REMARKS: CONTENTS OF THIRD RECORD IN STANDARD INPUT UNIT 05
C           (GEOGRAPHICAL FILTERING PARAMETERS):
C      If "F<file name>":
C          the presence of the character "F" is position 1 indicates a
C          lat/lon grid point mask file identified by the full path
C          name in characters 2-end -
C           the characteristics of the mask are:
C             full global lat/lon
C             grid spacing is 0.5 deg
C             mask is integer*4 with dimension mask(720,361)
C                 mask(1,1) is at 0.0 E lon, 90.0 S lat
C                 mask(720,1) is at 359.5 E lon, -90.0 S lat
C                 mask(1,361) is at 0.0 E lon, 90.0 N lat
C                 mask(720,361) is at 359.5 E lon, 90.0 N lat
C             mask(i,j) = 0 --> grid point is outside domain
C             mask(i,j) = 1 --> grid point is inside domain
C             for each report's lat/lon location in the grid, if at
C              least one of the four surrounding grid points is inside
C              the domain, then the report is considered to be inside
C              the domain and is kept; otherwise the report is
C              considered to be outside the domain and is skipped
C             (e.g.,  "F/nwprod/fix/nam_expdomain_halfdeg_imask.gbl"
C                      filters all reports outside of the 0.5 deg lat/
C                      lon mask specified in the file
C                      /nwprod/fix/nam_expdomain_halfdeg_imask.gbl)
C
C      If "yyyxxxdddddC":
C          the absence of a character "F" in position 1 and the
C          presence the character "C" in postion 12 indicates a lat/lon
C          circle is used for filtering -
C           here:
C             yyy=latitude  in center of circle in deg (N+,S-)
C             xxx=longitude in center of circle in deg (0.-360. West)
C             ddddd=radius for circle filter in km
C             (e.g.,  "045269 6500C"  filters out all reports outside of
C                      of a circle of radius 6500 km centered at 45
C                      degrees North lat and 260 degrees West lon)
C
C      If "sssnnneeewww":
C          the absence of a character "F" is postion 1 and the absence
C          of a character "C" in position 12 indicates a lat/lon
C          boundary is used for filtering - here:
C             sss=southern latitude  limit in deg (N+,S-)
C             nnn=northern latitude  limit in deg (N+,S-)
C             eee=eastern  longitude limit in deg (0.-360. West)
C             www=western  longitude limit in deg (0.-360. West)
C             (e.g.,  " 10 50 70120" filters out all reports outside of
C                      a rectangle bounded by 10-50 degrees North lat
C                      and 70-120 degrees West lon)
C
C      If "0           "  or empty:
C          no geopgraphical filtering is done
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  WCOSS
C
C$$$
      PROGRAM BUFR_GEOFIL
 
      PARAMETER (MXTS=6)
      PARAMETER (NX=720,NY=361) ! Dimension of mask if using mask

      REAL(8),ALLOCATABLE :: TAB_8(:,:)
      INTEGER,ALLOCATABLE :: JGEO(:)

      CHARACTER*80  TSTR,TSTRH
      CHARACTER*500 FILI,FILO,CARD
      CHARACTER*8   SUBSET
      DIMENSION     NGEO(0:4)
      REAL(8)       UFBTAB_8,BMISS,GETBMISS
      INTEGER(4)    MASK_4(NX,NY)
 
      DATA TSTR  /'CLAT  CLON  DAYS HOUR MINU SID '/
      DATA TSTRH /'CLATH CLONH DAYS HOUR MINU SID '/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_GEOFIL',2020,0233,0062,'NP22')

      print *
      print * ,'---> Welcome to BUFR_GEOFIL - Version 08-20-2020'
      print *

      CALL DATELEN(10)

ccccc CALL OPENBF(0,'QUIET',2) ! Uncomment for extra print from bufrlib

C  ASSIGN DEFAULT VALUE FOR 'MISSING' TO LOCAL BMISS VARIABLE
C  ----------------------------------------------------------

      CALL SETBMISS(10E8_8)
      BMISS = GETBMISS()     ! assign default value for "missing"
      print *
      print *, 'BUFRLIB value for missing is: ',bmiss
      print *
 
C  SET THE COUNTERS TO INITIAL VALUES
C  ----------------------------------

      ISUB  =  0
      IBOX  = -1
      LUBFI = 20
      LUBFC = 30
      LUBFJ = 50
 
      ISWT  = 0

      DEL  = .5! if filtering using mask, mask grid spacing is 0.5 deg
      ICHK = 2 ! if filtering using mask, if one of four grid points in
               !  mask surrounding report is in domain, report is
               !  considered to be in domain

C  READ I/O FILENAMES AND THE LAT/LONG DATA FILTERING LIMITS          
C  ---------------------------------------------------------
 
      READ(5,'(Q,A)',END=900,ERR=900) NBYTES_FILI,FILI(1:NBYTES_FILI)
      READ(5,'(Q,A)',END=900,ERR=900) NBYTES_FILO,FILO(1:NBYTES_FILO)
      READ(5,'(Q,A)',END=900,ERR=900) NBYTES_CARD,CARD(1:NBYTES_CARD)

cppppp
ccc   print *, 'file fili is ',nbytes_fili,' bytes long'
ccc   print *, 'file filo is ',nbytes_filo,' bytes long'
ccc   print *, 'file card is ',nbytes_card,' bytes long'
cppppp

C  DECIDE ON FILTERING METHOD--LAT/LON BOX, MASK, CIRCLE, OR NONE
C  --------------------------------------------------------------

      IF(CARD(1:1).EQ.'F')  THEN
         OPEN(LUBFC,FILE=CARD(2:NBYTES_CARD),FORM='UNFORMATTED')
         READ(LUBFC,ERR=99,END=99) MASK_4
         IBOX = 2
         GO TO 98
   99    CONTINUE
         print *
         print *, '--> BUFR_GEOFIL: ERROR READING MASK FILE'
         print *
         IBOX = -1
      ELSE IF(CARD(12:12).EQ.'C')  THEN
         READ(CARD,'(2F3.0,F5.0)') RLATC,RLONC,RRADC
         IBOX = 0
         IF(RRADC.EQ.0.) IBOX = -1
      ELSE
         READ(CARD,'(4F3.0)') RLATS,RLATN,RLONE,RLONW
         IBOX = 1
         IF(RLATS.EQ.0. .AND. RLATN.EQ.0.) IBOX = -1
      ENDIF

   98 CONTINUE

      PRINT 200, FILI(1:NBYTES_FILI),FILO(1:NBYTES_FILO),
     . FILI(1:NBYTES_FILI),CARD(1:NBYTES_CARD)
  200 FORMAT('UNFILTERED INPUT FILE IS                '/5X,A/
     .       'WORKSPACE (SCRATCH) FILE IS             '/5X,A/
     .       'GEOGRAPHICALLY FILTERED OUTPUT FILE IS  '/5X,A/
     .       'GEOGRAPHICAL FILTERING PARAMETER FILE IS'/5X,A/)
      IF(IBOX.EQ.0)  THEN
         PRINT 250, RLATC,RLONC,RRADC
  250    FORMAT('GEOGRAPHICAL FILTERING IS PERFORMED HERE USING ',
     .              'LAT/LON CIRCLE:'/
     .          '.. CENTRAL LATITUDE  (DEG. N+, S-) ..... ',F6.1/
     .          '.. CENTRAL LONGITUDE (0-360 W) ......... ',F6.1/
     .          '.. CIRCLE RADIUS (KM) .................. ',F6.1/
     .          '.. >>>>> N O T I C E :  THIS IS NOT YET AVAILABLE',
     .              ' - GEOGRAPHICAL FILTERING NOT PERFORMED HERE'/)
         IBOX = -1 ! Not ready
         CALL W3TAGE('BUFR_GEOFIL')
         CALL ERREXIT(04)
      ELSE  IF(IBOX.EQ.1)  THEN
         PRINT 251, RLATN,RLATS,RLONE,RLONW
  251    FORMAT('GEOGRAPHICAL FILTERING IS PERFORMED HERE USING ',
     .              'LAT/LON BOX WITH THE FOLLOWING BOUNDARY:'/
     .          '.. NORTHERN LATITUDE (DEG. N+, S-) ...... ',F6.1/
     .          '.. SOUTHERN LATITUDE (DEG. N+, S-) ...... ',F6.1/
     .          '.. EASTERN LONGITUDE (0-360 W) .......... ',F6.1/
     .          '.. WESTERN LONGITUDE (0-360 W) .......... ',F6.1/)
         IF(RLONE.GT.RLONW) ISWT=1
      ELSE  IF(IBOX.EQ.2)  THEN
         print'("GEOGRAPHICAL FILTERING IS PERFORMED USING 0.5 DEGREE'//
     .    ' GLOBAL LAT/LON GRID POINT MASK FILE"/A/)',
     .     CARD(2:NBYTES_CARD)
      ELSE
         PRINT 252
  252    FORMAT('GEOGRAPHICAL FILTERING NOT PERFORMED HERE'/)
         CALL W3TAGE('BUFR_GEOFIL')
         CALL ERREXIT(04)
      ENDIF
 
      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')

      CALL MESGBC(LUBFI,MSGT,ICOMP)
      IF(ICOMP.EQ.1) THEN
         PRINT'(/"INPUT BUFR FILE MESSAGES   C O M P R E S S E D"/'//
     .    '"FIRST MESSAGE TYPE FOUND IS",I5/)', MSGT
      ELSE  IF(ICOMP.EQ.0) THEN
         PRINT'(/"INPUT BUFR FILE MESSAGES   '//
     .    'U N C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND IS",I5/)',
     .    MSGT
      ELSE IF(ICOMP.EQ.-1)  THEN
         PRINT'(//"ERROR READING INPUT BUFR FILE - MESSAGE '//
     .    'COMPRESSION UNKNOWN"/)'
      ELSE
         IF(ICOMP.EQ.-3)  THEN
            PRINT'(/"INPUT BUFR FILE DOES NOT EXIST"/)'
         ELSE  IF(ICOMP.EQ.-2)  THEN
            PRINT'(/"INPUT BUFR FILE HAS NO DATA MESSAGES"/"FIRST '//
     .       'MESSAGE TYPE FOUND IS",I5/)', MSGT
         ENDIF
         PRINT'("GEOGRAPHICAL FILTERING WILL NOT BE DONE HERE"/)'
         CALL W3TAGE('BUFR_GEOFIL')
         CALL ERREXIT(04)
      ENDIF

      CALL CLOSBF(LUBFI)

C  COUNT THE NUMBER OF SUBSETS IN THE FILE TO ALLOCATE SPACE
C  ---------------------------------------------------------

      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      CALL OPENBF(0,'QUIET',1) ! will generate diagnostic print if an
                               ! embedded BUFR table is read
      CALL UFBTAB(-LUBFI,UFBTAB_8,1,1,MXTB,' ')
      CALL OPENBF(0,'QUIET',0) ! return to default wrt degree of print

      ALLOCATE(TAB_8(MXTS,MXTB) ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(JGEO(MXTB)       ,STAT=I);IF(I.NE.0) GOTO 901

      TAB_8 = BMISS
      JGEO  = 2

C  MAKE A TABLE OUT OF THE LATS, LONS, AND TIME COORDINATES
C  --------------------------------------------------------

      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      CALL UFBTAB(LUBFI,TAB_8,MXTS,MXTB,NTAB,TSTR)
      IF(IBFMS(TAB_8(1,1)).EQ.1) THEN                     ! data missing
         OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
         CALL UFBTAB(LUBFI,TAB_8,MXTS,MXTB,NTAB,TSTRH)
      ENDIF

C  GO THROUGH THE REPORTS IN ORDER, MARKING REPORTS WITHIN AREA       
C  ------------------------------------------------------------        
 
      DO K=1,NTAB  
cppppp
ccc      print *,' tabs ',tab_8(1,k),tab_8(2,k),tab_8(3,k),
ccc  .              tab_8(4,k),tab_8(5,k)
cppppp

         ALAT = TAB_8(1,K)

C  CONVERT THE LONGITUDE TO WEST LONGITUDE
C  --------------------------------------

         IF(TAB_8(2,K).LT.0.) THEN
            ALONW = -TAB_8(2,K)
         ELSE
            ALONW = 360. - TAB_8(2,K)
         END IF
 
C  CONVERT THE LONGITUDE TO EAST LONGITUDE
C  ---------------------------------------

         ALONE = 360. - ALONW

         IF(IBOX.EQ.1) THEN

C  FILTER REPORTS USING THE LATITUDE/LONGITUDE BOX
C  -----------------------------------------------

            IF(ALAT.LE.RLATN .AND. ALAT.GE.RLATS) THEN
               IF(ISWT .EQ. 0) THEN
                  IF(ALONW.LE.RLONW .AND. ALONW.GE.RLONE) JGEO(K) = 0
               ELSE
cppppp
ccc   print *,'lon test ',alonw,' rlonw rlone ',rlonw,rlone
cppppp
                  IF(ALONW.LE.RLONW .OR. ALONW.GE.RLONE) JGEO(K) = 0
               END IF
            END IF

         ELSE  IF(IBOX.EQ.0) THEN

C  FILTER REPORTS USING THE LAT/LON CIRCLE (NOT YET READY)
C  -------------------------------------------------------
 
            CALL CHDIST(RLONC,RLATC,ALONW,ALAT,RDIST)
            IF(RDIST.LE.RRADC) JGEO(K) = 0

         ELSE  IF(IBOX.EQ.2) THEN

C  FILTER REPORTS USING 0.5 DEGREE GLOBAL LAT/LON GRID POINT MASK
C  --------------------------------------------------------------
 
            CALL GEOCHK(ALAT,ALONE,ICHK,MASK_4,NX,NY,DEL,IGEO)
            IF(IGEO.EQ.1) JGEO(K) = 0
         ENDIF
      ENDDO
 
C  WRITE THE GEOGRAPHICALLY FILTERED FILE TO SCRATCH FILE
C  ------------------------------------------------------
 
      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      OPEN(LUBFJ,FILE=FILO(1:NBYTES_FILO),FORM='UNFORMATTED')
      CALL OPENBF(LUBFI,'IN ',LUBFI)
      CALL OPENBF(LUBFJ,'OUT',LUBFI)

      DO WHILE(IREADMG(LUBFI,SUBSET,IDATE).EQ.0)
         NSUBS = NMSUB(LUBFI)
cppppp
ccc      print *,' read in the BUFR data date ',idate    
ccc      print *,' number of nsubs is ',nsubs
cppppp
         IF(NSUBS.GT.0) THEN
            CALL OPENMB(LUBFJ,SUBSET,IDATE)
            DO N=ISUB+1,ISUB+NSUBS
               IF(JGEO(N) .EQ. 0) THEN
                  CALL COPYSB(LUBFI,LUBFJ,IRET)
                  NGEO(1) = NGEO(1) + 1
               ELSE
                  CALL COPYSB(LUBFI,00000,IRET)
                  NGEO(2) = NGEO(2) + 1
               END IF
            ENDDO
         ELSE
            CALL CLOSMG(LUBFJ)
            CALL COPYMG(LUBFI,LUBFJ)
         ENDIF
         ISUB = ISUB+NSUBS
      ENDDO

      NGEO(3) = NGEO(1) + NGEO(2)
      CALL CLOSBF(LUBFI)
      CALL CLOSBF(LUBFJ)
 
C  COPY THE GEOGRAPHICALLY FILTERED REPORTS TO OUTPUT FILE
C  -------------------------------------------------------
 
      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      OPEN(LUBFJ,FILE=FILO(1:NBYTES_FILO),FORM='UNFORMATTED')
      CALL COPYBF(LUBFJ,LUBFI)
      CLOSE(LUBFI)
      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      CALL MESGBC(LUBFI,MSGT,ICOMP)
      IF(ICOMP.EQ.1) THEN
         PRINT'(/"OUTPUT BUFR FILE MESSAGES   C O M P R E S S E D"/'//
     .    '"FIRST MESSAGE TYPE FOUND IS",I5/)', MSGT
      ELSE  IF(ICOMP.EQ.0) THEN
         PRINT'(/"OUTPUT BUFR FILE MESSAGES   '//
     .    'U N C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND IS",I5/)',
     .    MSGT
      ELSE IF(ICOMP.EQ.-1)  THEN
         PRINT'(//"ERROR READING OUTPUT BUFR FILE - MESSAGE '//
     .    'COMPRESSION UNKNOWN"/)'
      ELSE  IF(ICOMP.EQ.-3)  THEN
         PRINT'(/"OUTPUT BUFR FILE DOES NOT EXIST"/)'
      ELSE  IF(ICOMP.EQ.-2)  THEN
         PRINT'(/"OUTPUT BUFR FILE HAS NO DATA MESSAGES"/"FIRST '//
     .    'MESSAGE TYPE FOUND IS",I5/)', MSGT
      ENDIF
      CLOSE(LUBFI)
      CLOSE(LUBFC)
 
C  GENERATE REPORT
C  ---------------
 
      PRINT 300, NGEO(3),NGEO(1),NGEO(2)
  300 FORMAT(/'BUFR_GEOFIL READ IN A TOTAL OF',I8,' REPORTS'//
     .        'NUMBER OF PASSED REPORTS WRITTEN OUT ...........',I7/
     .        'NUMBER OF REPORTS SKIPPED DUE TO:'/
     .        '   GEOGRAPHIC FILTERING ........................',I7/)

C  END OF PROGRAM
C  --------------

      CALL W3TAGE('BUFR_GEOFIL')
      STOP

C  ERROR EXITS
C  -----------

  900 CONTINUE

      PRINT *, '#####BUFR_GEOFIL - EOF/ERR READING STDIN'
      CALL W3TAGE('BUFR_GEOFIL')
      CALL ERREXIT(99)

901   CONTINUE

      PRINT *, '#####BUFR_GEOFIL - UNABLE TO ALLOCATE ARRAYS'
      CALL W3TAGE('BUFR_GEOFIL')
      CALL ERREXIT(99)

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: CHDIST
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2004-02-02
C
C ABSTRACT:  COMPUTES CHORD LENGTH DISTANCE FROM ONE LAT/LON POINT
C   TO ANOTHER LAT/LON POINT USING THE FORMULA:
C     S**2/2 = 1 - COS(Y1-Y2) + COS(Y1)*COS(Y2)*(1-COS(X1-X2)).
C
C PROGRAM HISTORY LOG:
C 1990-11-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 2004-02-02  D. KEYSER  -- CORRECTED SUBROUTINE ARGUMENT MISALIGNMENT
C    (THIS DID NOT APPEAR TO CAUSE PROBLEMS IN OUTPUT BUT COULD CAUSE
C    MEMORY CLOBBERING DOWN THE LINE SOMEWHERE)
C
C USAGE:  CALL CHDIST(X1,Y1,X2,Y2,DIST)
C   INPUT ARGUMENTS:
C     X1         - LONGITUDE (0.-360. W) OF POINT 1
C     Y1         - LATITUDE  (N+,S-)     OF POINT 1
C     X2         - LONGITUDE (0.-360. W) OF POINT 2
C     Y2         - LATITUDE  (N+,S-)     OF POINT 2
C
C   OUTPUT ARGUMENTS:
C     DIST       - CHORD LENGTH DISTANCE BETWEEN POINTS (KM)
C
C REMARKS: NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  WCOSS
C
C$$$
      SUBROUTINE CHDIST(X1,Y1,X2,Y2,DIST)

      DATA PI180/.0174532 /,RADE/6371./

      SAVE PI180,RADE

C  COMPUTE THE DISTANCE
C  --------------------

      COSY1 = COS(Y1*PI180)
      COSY2 = COS(Y2*PI180)
      COSDX = COS((X1-X2)*PI180)
      COSDY = COS((Y1-Y2)*PI180)
      S = 1.0-COSDY+COSY1*COSY2*(1.0-COSDX)
      S = SQRT(2.*S)
      IF(S.LE..002) S = 0.
      DIST = S*RADE

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    GEOCHK
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2007-06-13
C      
C ABSTRACT: CHECKS THE VALUES OF THE FOUR MASK LAT/LON GRID POINTS
C   SURROUNDING A REPORT (BASED ON ITS LAT/LON).  BASED ON THE TYPE OF
C   CHECK WILL FLAG THE REPORT WITH "IGEO=1" IF ANY ONE (ICHK=2) OR IF
C   ALL FOUR (ICHK=1) SURROUNDING GRID POINTS HAVE A VALUE OF 1. MASK
C   IS ASSUMED TO HAVE VALUES OF 0 (OUTSIDE DOMAIN) OR 1 (INSIDE
C   DOMAIN).  THE MASK IS INTEGER*4.
C
C PROGRAM HISTORY LOG:
C 2007-06-13  D. A. KEYSER (W/NMC22)
C
C USAGE:    CALL GEOCHK(ALAT,ALONE,ICHK,MASK_4,NX,NY,DEL,IGEO)
C   INPUT ARGUMENT LIST:
C     ALAT     - LATITUDE OF DATA REPORT  (DEG. N+,S-)
C     ALONE    - LONGITUDE OF DATA REPORT (DEG. 0.-360. E)
C     ICHK     - TYPE OF CHECK:
C                   = 1 - ALL SURROUNDING GRID POINTS MUST BE 1
C                   = 2 - AT LEAST ONE SURROUNDING GRID POINT MUST BE 1
C                IF SATISFIED, OUTPUT ARGUMENT "IGEO" SET TO 1;
C                OTHERWISE "IGEO" SET TO 0
C     MASK_4   - INTEGER*4 NX x NY GRID ("DEL" DEGREE MASK)
C     NX       - NUMBER OF X-DIRECTION POINTS (COLUMNS, CORRESPONDING
C                TO LONGITUDE) IN "MASK_4"
C     NY       - NUMBER OF Y-DIRECTION POINTS (ROWS, CORRESPONDING TO
C                LATITUDE) IN "MASK_4"
C     DEL      - GRID SPACING OF MASK (IN DEGREES) (SEE REMARKS)
C
C   OUTPUT ARGUMENT LIST:
C     IGEO     - RETURN FLAG FOR CHECK (=0 "ICHK" NOT SATISFIED; =1
C                "ICHK" IS SATISFIED)
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       W3NCO    - W3TAGB  W3TAGE ERREXIT
C
C REMARKS: THE GRID SPACING (IN DEGREES) FOR THE MASK (DEL) CAN BE
C   EITHER 0.25, 0.5, 1.0 OR 2.0.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  WCOSS
C
C$$$
      SUBROUTINE GEOCHK(ALAT,ALONE,ICHK,MASK_4,NX,NY,DEL,IGEO)

      INTEGER(4) MASK_4(NX,NY)

      DATA IFIRST/0/

      IF(IFIRST.EQ.0) THEN
         IF(DEL.NE..25.AND.DEL.NE..5.AND.DEL.NE.1..AND.DEL.NE.2.) THEN
            PRINT'("#####BUFR_GEOFIL - LATITUDE SPACING IN MASK (="'//
     $       'G0,") NOT VALID - MUST BE EITHER 0.25, 0.5, 1.0 OR 2.0")',
     $       DEL
            CALL W3TAGE('BUFR_GEOFIL')
            CALL ERREXIT(99)
         ENDIF
         IFIRST = 1
      ENDIF

      IGEO = 0

cppppp
ccc   print *, '-- new report into geochk: ALAT,ALONE: ',ALAT,ALONE
cppppp
      XX = ALONE +  DEL
      YY = ALAT  +  90 + DEL

      IHX_L = (1/DEL)*XX
      IHX_H = IHX_L + 1
      IF(IHX_H.EQ.NX+1) IHX_H = 1
      IF(IHX_L.LT.1 .OR. IHX_L.GT.NX) THEN
         PRINT *, '#####BUFR_GEOFIL - LONGITUDE INDEX IN MASK (=',IHX_L,
     $    ') OUT OF RANGE'
         PRINT *, '     LAT = ',ALAT,', LON(E) = ',ALONE
cccccccc CALL W3TAGE('BUFR_GEOFIL')
cccccccc CALL ERREXIT(99)
         RETURN
      ENDIF

      IHY_L = (1/DEL)*YY
      IHY_H = IHY_L + 1
      IF(IHY_L.EQ.NY) IHY_H = NY
      IF(IHY_L.LT.1 .OR. IHY_L.GT.NY) THEN
         PRINT *, '#####BUFR_GEOFIL - LATITUDE INDEX IN MASK (=',IHY_L,
     $    ') OUT OF RANGE'
         PRINT *, '     LAT = ',ALAT,', LON(E) = ',ALONE
cccccccc CALL W3TAGE('BUFR_GEOFIL')
cccccccc CALL ERREXIT(99)
         RETURN
      ENDIF

cppppp
ccc   print *, '-- IHX_L,IHX_H,IHY_L,IHY_H: ',IHX_L,IHX_H,IHY_L,IHY_H
cppppp

C-----------------------------------------------------------------------
C      CHECK FOUR GRID POINTS SURROUNDING REPORT -- TWO METHODS
C-----------------------------------------------------------------------

      IF(ICHK.EQ.1)  THEN

C  CHECK 1-> ALL SURROUNDING PTS IN MASK MUST BE 1 (MOST RESTRICTIVE)
C  ------------------------------------------------------------------

         IF(MASK_4(IHX_L,IHY_L).EQ.1.AND.MASK_4(IHX_H,IHY_L).EQ.1.AND.
     $    MASK_4(IHX_L,IHY_H).EQ.1.AND.MASK_4(IHX_H,IHY_H).EQ.1) IGEO=1
      ELSE

C  CHECK 2-> ONLY 1 SURROUNDING PT IN MASK MUST BE 1 (LEAST RESTRICTIVE)
C  --------------------------------------------------------------------

cppppp
ccc   print *, '-- MASK_4(IHX_L,IHY_L),MASK_4(IHX_H,IHY_L),
ccc  $ MASK_4(IHX_L,IHY_H),MASK_4(IHX_H,IHY_H) : ', 
ccc  $ MASK_4(IHX_L,IHY_L),MASK_4(IHX_H,IHY_L),MASK_4(IHX_L,IHY_H),
ccc  $ MASK_4(IHX_H,IHY_H)
cppppp
         IF(MASK_4(IHX_L,IHY_L).EQ.1.OR.MASK_4(IHX_H,IHY_L).EQ.1.OR.
     $    MASK_4(IHX_L,IHY_H).EQ.1.OR.MASK_4(IHX_H,IHY_H).EQ.1)  IGEO=1
cppppp
ccc   print *, '-- coming out of geochk, IGEO = ',IGEO
cppppp

      END IF

      RETURN
      END
