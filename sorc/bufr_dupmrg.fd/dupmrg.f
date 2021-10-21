C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_DUPMRG
C   PRGMMR: DONG             ORG: NP22        DATE: 2020-08-20
C
C ABSTRACT: PROCESSES PROFILE DATABASE REPORT PARTS WITH CORRECTION
C   CHOOSING, DUPLICATE CHECKING, REPORT PART MERGING AND TRIMMING TO
C   EXACT TIME WINDOW (DAY DOWN TO MINUTE).  THIS CURRENTLY INCLUDES
C   ALL RADIOSONDE, DROPWINSONDE, PIBAL AND WIND PROFILERS FROM "PILOT"
C   REPORTS.  THE ALGORITHM SORTS THE REPORT PARTS IN ASCENDING ORDER
C   OF LAT, LON, OBS TIME (DAY DOWN TO MINUTE)), BUFR TABLE (USUALLY
C   ONLY ONE, THE SAME FOR ALL REPORTS, RARELY MORE THAN ONE IF
C   EMBEDDED BUFR TABLES ARE PRESENT IN DATABASE FILE BEING READ),
C   CORRECTION INDICATOR AND RECEIPT TIME (YEAR DOWN TO MINUTE).  FOR
C   DROPS, A CHECK IS ALSO MADE TO ACCOUNT FOR POSSIBLE UP-STREAM LAT/
C   LON CORRECTIONS (ADJUSTMENTS) TO THE SAME REPORT ID SO THAT THEY
C   CAN STILL BE DUP-CHECKED USING LAT/LON COORDINATES LIKE ALL OTHER
C   TYPES HERE.  THE MERGE IS COMPLETED BY COMBINING THE PARTS OF EACH
C   REPORT IN DESCENDING ORDER, ACCEPTING FOR EACH PART (TTAA, TTBB,
C   ...) THE BULLETIN LAST RECEIVED OR CORRECTED.  IN THE DUPLICATE
C   CHECKING, REPORTS ARE CHECKED FOR LAT, LON AND OBS TIME (DAY DOWN
C   TO MINUTE) ALL INSIDE THE TOLERANCE LIMITS.  THE FILE PATH/NAMES OF
C   THE INPUT AND OUTPUT FILES, (OPTIONALLY) THE TIME WINDOW TO TRIM TO
C   AND (OPTIONALLY) DEFAULT OVERRIDE DUP-CHECKING TOLERANCE LIMITS ARE
C   READ FROM STANDARD INPUT AT THE START OF THIS PROGRAM.  IF THE
C   TIME WINDOW RECORD IS MISSING, THEN NO TIME WINDOWING IS
C   PERFORMED.  ALL FILE CONNECTIONS (EXCEPT STANDARD INPUT WHICH IS
C   PRE-CONNECTED) ARE MADE THROUGH THE FORTRAN OPEN STATEMENT.
C
C PROGRAM HISTORY LOG:
C 1996-09-06  J. WOOLLEN  ORIGINAL VERSION FOR IMPLEMENTATION
C 1996-11-27  J. WOOLLEN  ADDED NEW OUTPUT STACKS FOR BID AND UARID
C 1996-11-28  J. WOOLLEN  FIXED PROBLEM WITH LAST REPORT NOT BEING
C     MERGED
C 1996-12-04  J. WOOLLEN  FIXED PROBLEM WITH KEEPING DUMP TIME MESSAGES
C 1996-12-12  J. WOOLLEN  DON'T COPY DUMP DATE MESSAGES IF THEY DON'T
C     EXIST
C 1997-06-17  J. WOOLLEN  CHANGE CRITERIA FOR MERGING DROPSONDE PARTS
C 1997-07-15  J. WOOLLEN  REMOVE INCORRECT OFFSET FROM ORDERS ARGUMENT
C     NTAB
C 1997-11-05  B. FACEY    REVERSE SORT PRECEDENCE FOR LENGTH OF
C     BULLETIN
C 1998-07-21  J. WOOLLEN  PROTECT MERGE LOOP FROM OUT OF BOUNDS
C     REFERENCE
C 1999-01-30  B. FACEY    PROTECT FROM OUT OF BOUNDS REFERENCE WHEN
C     ONLY BUFR TABLES AND NO DATA ARE PASSED TO PROGRAM
C 1999-06-03  D. KEYSER   MODIFIED TO PORT TO IBM SP AND RUN IN 4 OR
C     8 BYTE STORAGE
C 2000-12-05  D. KEYSER   INCREASED LIMIT FOR I/O FILENAME LENGTH
C     FROM 80 CHARACTERS TO 500 CHARACTERS
C 2002-03-05  D. KEYSER   IMPROVED DOCUMENTATION; IMPROVED STANDARD
C     OUTPUT PRINT; ADDED CALL TO COMPRESS_CHECK TO INDICATE IF INPUT/
C     OUTPUT FILES ARE COMPRESSED OR UNCOMPRESSED; ACCOUNTS FOR CHANGES
C     IN INPUT UPPER-AIR PROFILE BUFR DUMP FILES AFTER 3/2002: MNEMONIC
C     "HBLCS" REPLACES "HOCB" IN REPLICATED "UACLD" SEQUENCE; MNEMONIC
C     "BORG" REPLACES "ICLI" IN REPLICATED "BID" SEQUENCE (WILL STILL
C     WORK PROPERLY FOR INPUT UPPER-AIR PROFILE BUFR DUMP FILES PRIOR
C     TO 3/2002)
C 2002-06-05  J. WOOLLEN   FOR DROPS, CHECKS FOR POSSIBLE UP-STREAM
C     LAT/LON CORRECTIONS (ADJUSTMENTS) TO THE SAME REPORT ID SO THAT
C     THEY CAN STILL BE DUP-CHECKED USING LAT/LON COORDINATES LIKE ALL
C     OTHER TYPES  
C 2004-02-02  D. KEYSER   REPLACED CALL TO IN-LINE SUBROUTINE
C     COMPRESS_CHECK WITH CALL TO NEW BUFRLIB ROUTINE MESGBC; REFINED
C     DROPS LAT/LON CORRECTION (SEE 2002-06-05) SUCH THAT ADJUSTMENTS
C     WILL ONLY BE MADE IF CORRECTION INDICATOR ("CORN") IS BOTH .GT.
C     0 AND NON-MISSING (PRIOR TO THIS CHECK WAS ONLY FOR NON-MISSING
C     "CORN" WHICH WAS TOSSING UNIQUE TAIWANESE DROPS BECAUSE THEY ALL
C     HAD THE SAME ID "DRP99A", AND THEY HAD CORN=0); BETTER DEFINED
C     WHAT ERROR IS IN BAD RETURN FROM BUFRLIB ROUTINE RDMEMM
C 2006-03-02  D. KEYSER   NO LONGER HARDWIRED TO COPY "DUMMY" MESSAGES
C     CONTAINING DUMP CENTER TIME AND PROCESSING TIME, RESP. FROM FIRST
C     TWO MESSAGES OF INPUT FILE (AFTER TABLE MSGS) INTO FIRST TWO
C     MESSAGES OF OUTPUT FILE (AFTER TABLE MSGS) BECAUSE INPUT FILE
C     MOST LIKELY WILL NOT CONTAIN THEM AS GENERATION OF DUMMY MESSAGES
C     HAS MOVED FROM PGM BUFR_DUMPMD, WHICH RUNS BEFORE THIS PGM, TO
C     PGM BUFR_COMBFR WHICH RUNS AFTER THIS PGM - EXCEPTION IS WHEN
C     DUMPJB SCRIPT VARIABLE "FORM" IS SET TO 'copy', IN WHICH CASE
C     BUFR_DUMPMD GENERATES DUMMY MESSAGES BECAUSE BUFR_COMBFR DOES NOT
C     RUN -  THIS CODE WILL ONLY COPY THE DUMMY MESSAGES IF DUMPJB
C     SCRIPT VARIABLE "DUMMY_MSGS" (READ IN VIA "GETENV") IS "YES",
C     MEANING THAT THE INPUT FILE DOES CONTAIN DUMMY MESSAGES; IF TIME
C     WINDOW RECORD (LINE 3) IS EMPTY IN STANDARD INPUT, TIME WINDOW
C     TRIMMING LOOP BYPASSED, BASED ON TEST FOR DEFAULT VALUE OF LATEST
C     REQUESTED DATE WHICH IS NOW CORRECTED TO BE 99999999.00_8 (WAS
C     99999999.00 BUT IN 4-BYTE REAL MACHINES THIS COULD BE ROUNDED UP
C     TO 100000000.00 MEANING LASTEST DDHH.hh IN TIME TESTS, BEFORE
C     TIME WINDOW TRIMMING LOOP WAS BYPASSED, WOULD BE 0000 AND ALL
C     REPORTS WOULD BE TRIMMED RATHER THAN RETAINED)
C 2007-03-23  D. KEYSER   INTRODUCED ALLOCATABLE ARRAYS TO AVOID ARRAY
C     OVERFLOW PROBLEMS, DETERMINES SIZE OF ARRAYS BY CALLING UFBTAB
C     WITH NEGATIVE UNIT NUMBER TO SIMPLY COUNT SUBSETS
C 2010-12-10  D. KEYSER   MODIFIED TO NOT ATTEMPT TO MERGE REPORT
C     "PARTS" WHICH ARE GENERATED FROM DIFFERENT (EMBEDDED) BUFR TABLES
C      IN THE DATABASE FILES AS THIS CAN CAUSE A SEG-FAULT (BASED ON
C      NEWEST VERSION OF BUFRLIB WHICH, FOR THE FIRST TIME, ALLOWS FOR,
C      THE USE OF EMBEDDED BUFR TABLES) - FOR A PARTICULAR LOCATION/
C      OBS-TIME, IF DUPLICATE REPORT PARTS ARE ENCOUNTERED, THE PART
C      GENERATED FROM THE LATEST (FURTHEST DOWN IN TANK) EMBEDDED BUFR
C      TABLE IS RETAINED, ALL OTHERS ARE TOSSED, THEN IN THE MERGING
C      PROCESS, ONLY THOSE PARTS GENERATED FROM THE SAME EMBEDDED BUFR
C      TABLE ARE MERGED - THIS COULD RESULT IN DUPLICATE LOCATION/OBS-
C      TIME REPORTS IN THE OUTPUT DUMP IF UNIQUE PARTS WERE GENERATED
C      FROM DIFFERENT EMBEDDED BUFR TABLES (ALL OF THIS CAN ONLY HAPPEN
C      ON THOSE RARE DAYS WHEN AN EMBEDDED BUFR TABLE IS FOUND IN THE
C      TANK, ON ALL OTHER DAYS THIS CODE WORKS EXACTLY AS BEFORE)
C 2013-01-14  J.WHITING  PORT TO WCOSS -- ADAPTED IBM/AIX GETENV 
C      SUBPROGRAM CALL TO INTEL/LINUX SYNTAX; UPDATED DOC BLOCKS;
C      REPLACED TESTS VS BMISS W/ IBFMS FUNCTION; REPLACED EXPLICIT 
C      ASSIGNMENT OF BMISS W/ GETBMISS() FUNCTION. READY FOR WCOSS 
C      IMPLEMENTATION; 
C 2014-11-07  D. KEYSER   DECLARE FUNCTION GETBMISS AND ITS RETURN
C     VALUE BMISS AS REAL*8 TO GET A RELIABLE VALUE FOR BMISS IN PRINT
C     STATEMENTS
C 2020-08-20  J. DONG  --  ADDED SETBMISS CALL TO SET BMISS TO 10E8
C     TO AVOID INTEGER OVERFLOW
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT - FIRST RECORD CONTAINS INPUT FILE
C                NAME, SECOND RECORD CONTAINS OUTPUT FILE NAME,
C                OPTIONAL THIRD RECORD CONTAINS TIME-WINDOWING
C                SPECIFICATIONS (THE YYYYMMDDHH<.HH> DATE OF THE
C                EARLIEST TIME TO DUMP AND THE YYYYMMDDHH<.HH> DATE OF
C                THE LATEST TIME TO DUMP), OPTIONAL FOURTH RECORD
C                CONTAINS DUP-CHECKING TOLERANCE LIMITS (IF FOURTH
C                RECORD IS MISSING, DEFAULT DUP-CHECKING TOLERANCE
C                LIMITS ARE USED, IF THIRD RECORD IS ALSO MISSING, NO
C                TIME WINDOWING IS PERFORMED)
C     UNIT 20  - UNCHECKED, UNCORRECTED, UNWINDOWED AND UNMERGED BUFR
C              - DUMP FILE
C
C   OUTPUT FILES:
C     UNIT 50  - DUPLICATE CHECKED, CORRECTED, TIME WINDOWED AND MERGED
C              - BUFR DUMP FILE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE     - UARINI  UARDUP UARMRG UARWRT
C     SYSTEM     - GETENV  SYSTEM
C     LIBRARY:
C       W3NCO    - W3TAGB  W3TAGE  ERREXIT
C       W3EMC    - ORDERS
C       BUFRLIB  - DATELEN OPENBF COPYMG UFBINT UFBTAM OPENMB WRITSB
C                  UFBMMS  UFBMEM UFBOVR CLOSBF RDMEMM STATUS INVMRG
C                  MRGINV  NEMTAB MESGBC UFBTAB IBFMS  GETBMISS
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C            > 0 - ABNORMAL RUN
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  WCOSS
C
C$$$
      PROGRAM BUFR_DUPMRG
 
      PARAMETER (MXTS=6)

      REAL(8),ALLOCATABLE :: TAB_8(:,:)
      REAL(8),ALLOCATABLE :: RAB_8(:,:)
      REAL(8),ALLOCATABLE :: CAB_8(:,:)
      REAL(8),ALLOCATABLE :: DAB_8(:)
      REAL(8),ALLOCATABLE :: REC_8(:,:)
      INTEGER,ALLOCATABLE :: IWORK(:)
      INTEGER,ALLOCATABLE :: IORD(:)
      INTEGER,ALLOCATABLE :: MDUP(:)
 
      COMMON /UNITS/ LUBFI,LUBFJ
      COMMON /VERSION_FLAGS/ IBORG,IHBLCS
      COMMON /BUFRLIB_MISSING/BMISS
 
      CHARACTER*500 FILI,FILO
      CHARACTER*80  TSTR,RSTR
      CHARACTER*8   SUBSET,CRPID,CRPID_THIS
      CHARACTER*8   CRPID_IREC,CRPID_JREC
      CHARACTER*3   DUMMY_MSGS
      CHARACTER*1   CDUMMY
 
      DIMENSION    NDUP(0:3),NREP(8)
 
      LOGICAL      WINDOW,SKIP_BTBL_DISAGR
 
      REAL(8)      ADATE,BDATE,CDATE,DDATE,RDATE,UFBTAB_8,RPID_8
      REAL(8)      RPID_IREC_8,RPID_JREC_8
      REAL(8)      BMISS,GETBMISS
 
      EQUIVALENCE  (RPID_8,CRPID)
      EQUIVALENCE  (RPID_IREC_8,CRPID_IREC),(RPID_JREC_8,CRPID_JREC)
 
      DATA TSTR  /'CLAT CLON DAYS HOUR MINU {UARLV}'/
      DATA RSTR  /'RCYR RCMO RCDY RCHR RCMI RCTS   '/
 
      DATA ADATE /00000000.00_8/
      DATA BDATE /99999999.00_8/
      DATA DEXY  /0.0/
      DATA DDAY  /0.0/
      DATA DOUR  /0.0/
      DATA DMIN  /0.0/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_DUPMRG',2020,0233,0062,'NP22')
 
      print *
      print * ,'---> Welcome to BUFR_DUPMRG - Version 08-20-2020'
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
 
      NREP  = 0
      NDUP  = 0
      ITAB  = 0
      NSKIP = 0
      LUBFI = 20
      LUBFJ = 50
 
C  If SKIP_BTBL_DISAGR=T, skip unique parts generated from a different
C   BUFR table than earlier unique parts (for same location/obs-time)
C  If SKIP_BTBL_DISAGR=F, create a new report from unique parts
C   generated from a different BUFR table than earlier unique parts
C   (for same location/obs-time)
C  CURRENTLY THIS IS SUPPOSED TO BE SET TO FALSE
C  --------------------------------------------------------------------

      SKIP_BTBL_DISAGR = .FALSE.

C  READ I/O FILENAMES AND ANY OVERRIDE VALUES FOR THINNING PARAMETERS
C  ------------------------------------------------------------------
C     DEFAULT PARAMETERS:
C     ADATE = 00000000.00  LOWER LIMIT FOR DATE/TIME
C     BDATE = 99999999.00  UPPER LIMIT FOR DATE/TIME
C     DEXY  = 0.0  TOLERANCE FOR LAT,LON CHECKS
C     DDAY  = 0.0  TOLERANCE FOR DAY CHECK
C     DOUR  = 0.0  TOLERANCE FOR HOUR CHECK
C     DMIN  = 0.0  TOLERANCE FOR MINUTE CHECK
C  ------------------------------------------------------------------
 
      READ(5,'(Q,A)',END=900,ERR=900) NBYTES_FILI,FILI(1:NBYTES_FILI)
      READ(5,'(Q,A)',END=900,ERR=900) NBYTES_FILO,FILO(1:NBYTES_FILO)
 
cpppp
ccc   print *, 'file fili is ',nbytes_fili,' bytes long'
ccc   print *, 'file filo is ',nbytes_filo,' bytes long'
cpppp
 
      READ(5,*,END=1) ADATE,BDATE
      READ(5,*,END=1) DEXY,DDAY,DOUR,DMIN
 
    1 CONTINUE
 
      IF(BDATE.NE.99999999.00_8) THEN
         PRINT 200, ADATE,BDATE
      ELSE
         PRINT 201
      ENDIF
  200 FORMAT(/'REQUESTED EARLIEST DATE IS ....... ',F15.2/
     .        'REQUESTED LATEST   DATE IS ....... ',F15.2)
  201 FORMAT(/'@@@@ AS REQUESTED, NO TIME WINDOW TRIMMING IS PERFORMED'/
     .        '@@@@ ALL NON-DUPLICATES ARE RETAINED REGARDLESS OF TIME')
      PRINT 202, FILI(1:NBYTES_FILI),FILO(1:NBYTES_FILO),DEXY,DDAY,DOUR,
     . DMIN
  202 FORMAT(/'UNCHECKED AND UNMERGED INPUT FILE IS        '/5X,A/
     .        'DUPLICATE CHECKED AND MERGED OUTPUT FILE IS '/5X,A//
     .        'BUFR_DUPMRG PARAMETERS:'/
     .        3X,'TOLERANCE FOR LAT/LON CHECKS (IN DEGREES) .. ',F7.3/
     .        3X,'TOLERANCE FOR YEAR CHECK (** NOT CHECKED **) '/
     .        3X,'TOLERANCE FOR MONTH CHECK (** NOT CHECKED **) '/
     .        3X,'TOLERANCE FOR DAY CHECK (IN DAYS) .......... ',F7.3/
     .        3X,'TOLERANCE FOR HOUR CHECK (IN HOURS) ........ ',F7.3/
     .        3X,'TOLERANCE FOR MINUTE CHECK (IN MINUTES) .... ',F7.3/
     .        3X,'TOLERANCE FOR SECOND CHECK (** NOT CHECKED **) ')
 
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
      ELSE  IF(ICOMP.EQ.-3)  THEN
         PRINT'(/"INPUT BUFR FILE DOES NOT EXIST"/)'
      ELSE  IF(ICOMP.EQ.-2)  THEN
         PRINT'(/"INPUT BUFR FILE HAS NO DATA MESSAGES"/"FIRST '//
     .    'MESSAGE TYPE FOUND IS",I5/)', MSGT
      ENDIF
 
      CALL CLOSBF(LUBFI)

C  COUNT THE NUMBER OF SUBSETS IN THE FILE TO ALLOCATE SPACE
C  ---------------------------------------------------------

      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      CALL OPENBF(0,'QUIET',1) ! will generate diagnostic print if an
                               ! embedded BUFR table is read
      CALL UFBTAB(-LUBFI,UFBTAB_8,1,1,MXTB,' ')
      CALL OPENBF(0,'QUIET',0) ! return to default wrt degree of print

      MXTB = MXTB + 1  ! need one more than total number of subsets

      ALLOCATE(TAB_8(MXTS,MXTB),STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(RAB_8(MXTS,MXTB),STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(CAB_8(MXTS,MXTB),STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(DAB_8(MXTB),STAT=I);     IF(I.NE.0) GOTO 901
      ALLOCATE(REC_8(   3,MXTB),STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(IWORK(MXTB)     ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(IORD(MXTB)      ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(MDUP(MXTB)      ,STAT=I);IF(I.NE.0) GOTO 901

      TAB_8 = BMISS
      REC_8 = BMISS
      IORD  = 0
      MDUP  = 99
 
C  STORE THE INPUT BUFR FILE INTO MEMORY
C  -------------------------------------
 
      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      CALL UFBMEM(LUBFI,0,IMSG,MUNIT)
      IF(MUNIT.EQ.0) THEN
         PRINT *, '###BUFR_DUPMRG - NO DATA IN INPUT FILE - STOP'
         CALL W3TAGE('BUFR_DUPMRG')
         CALL ERREXIT(00)
      ENDIF
 
      OPEN(LUBFJ,FILE=FILO(1:NBYTES_FILO),FORM='UNFORMATTED')
      CALL OPENBF(LUBFJ,'OUT',LUBFI)
 
C  Check to see if the new (post 3/2002) version of the mnemonic
C   table is being used here
C  -------------------------------------------------------------
 
      CALL STATUS(LUBFJ,LUN,IDUMMY1,IDUMMY2)
      CALL NEMTAB(LUN,'BORG',IDUMMY1,CDUMMY,IRET)  ! "BORG" is only
      IBORG = IRET                                 !  in new table
      CALL NEMTAB(LUN,'HBLCS',IDUMMY1,CDUMMY,IRET) ! "HBLCS" is only
      IHBLCS = IRET                                !  IN NEW TABLE
 
      CALL GETENV('DUMMY_MSGS',DUMMY_MSGS)

      IF(DUMMY_MSGS.EQ.'YES') THEN

C  IF INPUT FILE CONTAINS DUMMY MESSAGES CONTAINING THE CENTER TIME AND
C    DUMP TIME (MESSAGES 1 AND 2 AFTER THE TABLE MESSAGES), COPY THEM
C    TO OUTPUT FILE
C  --------------------------------------------------------------------
 
         DO I=1,2
            CALL RDMEMM(I,SUBSET,IDATE,IRET)
            IF(IRET.NE.0) GO TO 902
            IF(NMSUB(LUBFI).EQ.0) CALL COPYMG(LUBFI,LUBFJ)
         ENDDO
      ELSE

C  OTHERWISE, READ FIRST MESSAGE IN MEMORY TO GET MESSAGE TYPE IN
C   "SUBSET" (FOR LATER USE)
C  --------------------------------------------------------------

         CALL RDMEMM(1,SUBSET,IDATE,IRET)
      ENDIF
 
C  PARSE OUT THE REPORT REGISTRATIONS INTO TABLES FOR LOCATING PARTS
C  -----------------------------------------------------------------
 
      CALL UFBTAM(RAB_8,MXTS,MXTB-1,NTAB,RSTR)
      CALL UFBTAM(TAB_8,MXTS,MXTB-1,NTAB,TSTR)
      CALL UFBTAM(CAB_8,MXTS,MXTB-1,NTAB,'CORN')
      CALL UFBTAM(DAB_8,   1,MXTB-1,NTAB,'RPID')
      CALL UFBTAM(REC_8,   3,MXTB-1,NTAB,'IREC ISUB ITBL')
 
C  SET MISSING MINUTES TO ZERO; COUNT THE NUMBER OF BUFR TABLES
C  ------------------------------------------------------------
 
      NUM_TABLES = 0
      DO I=1,NTAB
cppppp
ccc      print *, 'input subset # ',i,'; from msg ',rec_8(1,i),
ccc  $    '; dictionary = ',rec_8(3,i)
cppppp
         IF(IBFMS(TAB_8(5,I)).EQ.1) TAB_8(5,I) = 0          ! data missing
         NUM_TABLES = MAX(NUM_TABLES,INT(REC_8(3,I)))
      ENDDO

      print'(/"The number of embedded BUFR tables (including at the '//
     $ 'top of the file) read in is ",I0/)', NUM_TABLES

C  RESOLVE LAT/LON CORRECTIONS POSSIBLE IN DROPSONDE DATA
C  ------------------------------------------------------
 
      IF(SUBSET.EQ.'NC002004') THEN
         DO I=NTAB,1,-1
         IF(IBFMS(CAB_8(1,I)).EQ.0 .AND. CAB_8(1,I).GT.0.) THEN ! CAB_8(1,I) not missing
            RPID_8 = DAB_8(I)
            CRPID_THIS = CRPID
            RLAT = TAB_8(1,I)
            RLON = TAB_8(2,I)
            DO N=1,NTAB
               RPID_8 = DAB_8(N)   
               IF(CRPID_THIS.EQ.CRPID) THEN
                  TAB_8(1,N) = RLAT
                  TAB_8(2,N) = RLON
               ENDIF
            ENDDO
         ENDIF
         ENDDO
      ENDIF
 
C  GET A SORTED INDEX OF THE REPORTS KEYED IN THIS ORDER: LAT, LON, OBS
C   TIME, BUFR TABLE, CORRECTION INDICATOR, # OBS LEVELS, RECEIPT TIME
C  --------------------------------------------------------------------
 
      CALL ORDERS( 2,IWORK,RAB_8(5,1),IORD,NTAB,MXTS,8,2) ! rcpt minute
      CALL ORDERS(12,IWORK,RAB_8(4,1),IORD,NTAB,MXTS,8,2) ! rcpt hour
      CALL ORDERS(12,IWORK,RAB_8(3,1),IORD,NTAB,MXTS,8,2) ! rcpt day
      CALL ORDERS(12,IWORK,RAB_8(2,1),IORD,NTAB,MXTS,8,2) ! rcpt month
      CALL ORDERS(12,IWORK,RAB_8(1,1),IORD,NTAB,MXTS,8,2) ! rcpt year
      CALL ORDERS(12,IWORK,TAB_8(6,1),IORD,NTAB,MXTS,8,2) ! # obs lvls
      CALL ORDERS(12,IWORK,CAB_8(1,1),IORD,NTAB,MXTS,8,2) ! correction
      CALL ORDERS(12,IWORK,REC_8(3,1),IORD,NTAB,   3,8,2) ! BUFR table
      CALL ORDERS(12,IWORK,TAB_8(5,1),IORD,NTAB,MXTS,8,2) ! obs minute
      CALL ORDERS(12,IWORK,TAB_8(4,1),IORD,NTAB,MXTS,8,2) ! obs hour
      CALL ORDERS(12,IWORK,TAB_8(3,1),IORD,NTAB,MXTS,8,2) ! obs day
      CALL ORDERS(12,IWORK,TAB_8(2,1),IORD,NTAB,MXTS,8,2) ! longitude
      CALL ORDERS(12,IWORK,TAB_8(1,1),IORD,NTAB,MXTS,8,2) ! latitude
 
C  CALL THE MERGER FOR EACH REPORT GROUP IN THE TIME WINDOW
C  --------------------------------------------------------
 
   10 CONTINUE
 
      IF(ITAB+1.GT.NTAB) GO TO 20
      NPAR = 0
      IPAR = ITAB+1
      IPTR = IORD(IPAR)
 
C  PREPARE TO TRIM EXCESS DATA FROM THE EXACT TIME WINDOW (IF REQUESTED)
C  ---------------------------------------------------------------------

      IF(BDATE.NE.99999999.00_8) THEN
         CDATE = MOD(ADATE,10000._8)
         DDATE = MOD(BDATE,10000._8)
         RDATE = TAB_8(3,IPTR)*1E2 + TAB_8(4,IPTR) + TAB_8(5,IPTR)/60.
         IF(CDATE.LE.DDATE) WINDOW=RDATE.GE.CDATE .AND. RDATE.LE.DDATE
         IF(CDATE.GT.DDATE) WINDOW=
     .                          .NOT.(RDATE.LT.CDATE.AND.RDATE.GT.DDATE)
      ELSE
         WINDOW=.TRUE.
      ENDIF
 
      DO ITAB=ITAB+1,NTAB
         IREC = IORD(ITAB)
         JREC = IORD(MIN(ITAB+1,NTAB))
cpppp
ccc      print *, 'New input report number (itab) ',itab,' found in ',
ccc  $    'record (irec) ',irec
ccc      print *, 'Next input report number (itab+1) ',itab+1,' found ',
ccc  $   'in record (jrec) ',jrec
cpppp
         RPID_IREC_8 = DAB_8(IREC)
         RPID_JREC_8 = DAB_8(JREC)
cpppp
ccc      print 1789, irec,crpid_irec,(tab_8(ii,irec),ii=1,2),
ccc  $    nint(rec_8(3,irec)),(nint(tab_8(ii,irec)),ii=3,5),
ccc  $    nint(cab_8(1,irec)),nint(tab_8(6,irec)),
ccc  $    (nint(rab_8(ii,irec)),ii=1,6)
c1789    format('IREC: ',I5,'; STNID: ',A8,';{ LAT: ',F6.2,'; LON: ',
ccc  $    F7.2,'; BTBL: ',I3,'; RPRT DDHHMM: ',3I2.2,'; CORN: ',I3,
ccc  $    '; UARLV: ',I3,'; RCPT YYYYMMDDHHMM: ',I4,4I2.2,' }; RCTS: ',
ccc  $    I2)
cpppp	
         IF(
     .    NINT(ABS(TAB_8(1,IREC)-TAB_8(1,JREC))*100.).LE.NINT(DEXY*100.)
     .    .AND.
     .    NINT(ABS(TAB_8(2,IREC)-TAB_8(2,JREC))*100.).LE.NINT(DEXY*100.)
     .    .AND.
     .    NINT(ABS(TAB_8(3,IREC)-TAB_8(3,JREC))*100.).LE.NINT(DDAY*100.)
     .    .AND.
     .    NINT(ABS(TAB_8(4,IREC)-TAB_8(4,JREC))*100.).LE.NINT(DOUR*100.)
     .    .AND.
     .    NINT(ABS(TAB_8(5,IREC)-TAB_8(5,JREC))*100.).LE.NINT(DMIN*100.)
     .    .AND. ITAB.LT.NTAB) THEN
            NPAR = NPAR+1   ! This report is a duplicate of the next one
                            !  wrt location and time (the reports may
                            !  be from the same or different "parts")
cpppp
ccc         print *, 'This input report has same time/loc as input ',
ccc  .               'report number ',itab+1,', NPAR = ',npar
ccc         print *, 'BUFR tables:: IREC: ',nint(rec_8(3,IREC)),' and ',
ccc  .      'JREC: ',nint(rec_8(3,JREC))
cpppp
         ELSE IF(WINDOW) THEN
            CALL UARINI
cpppp
ccc    print *, 'Ready to loop backwards through ',npar+1, ' dups'
ccc    print *, '# REMOVE DUPL. PARTS #########################'
cpppp

C  Loop backwards through all the location/obs-time duplicates in this
C   unique "report" solely for the purpose of removing duplicate parts
C  -------------------------------------------------------------------

            DO N=IPAR+NPAR,IPAR,-1
               IREP = IORD(N)
               RCTS = RAB_8(6,IREP)
               CORN = CAB_8(1,IREP)

C  The following identifies the location of this rpt in the input file
C  -------------------------------------------------------------------

               MREC = REC_8(1,IREP)  !   MREC gives the message number
               MSUB = REC_8(2,IREP)  !   MSUB gives the subset number
cpppp
ccc            print *, 'For part number ',N,
ccc  .          ', IREP,RCTS,CORN,MREC,MSUB: ',IREP,nint(RCTS),
ccc  .          nint(CORN),MREC,MSUB
cpppp
               CALL UARDUP(RCTS,CORN,MREC,MSUB,IDUP)
cpppp
ccc            print *,'For part number ',N,', IDUP comes back as ',IDUP
cpppp
               NDUP(IDUP) = NDUP(IDUP) + 1
               MDUP(N) = IDUP
            ENDDO
            NCOUNT = 0
            NTABLES = 0
   30       CONTINUE
            NTABLES = NTABLES + 1
            IF(NTABLES.GT.5)  THEN
               PRINT'("The number of different embedded BUFR tables '//
     $          'generating parts within the same ""report"" exceeds '//
     $          'the limit of 5 - no more separate, new reports can '//
     $          'be generated")'
               GO TO 10
            ENDIF
            CALL UARINI
cpppp
ccc    print *, 'Ready to AGAIN loop backwards through ',npar+1, ' dups'
ccc    print *, '# MERGE TOGETHER UNIQUE PARTS ################'
cpppp

C  Again loop backwards through all the location/obs-time duplicates in
C   this unique "report", skipping duplicate parts identified in
C   previous loop, and merge together unique parts into a single new
C   report
C  --------------------------------------------------------------------

            ITBL_LAST = -99
            DO N=IPAR+NPAR,IPAR,-1  
               NCOUNT = NCOUNT + 1
               IREP = IORD(N)

C  The following identifies the location of this rpt in the input file
C  -------------------------------------------------------------------

               MREC = REC_8(1,IREP)  !   MREC gives the message number
               MSUB = REC_8(2,IREP)  !   MSUB gives the subset number
cpppp
ccc            print *, 'For part number ',N,' IREP,MREC,MSUB: ',IREP,
ccc  $          MREC,MSUB
cpppp
 
               IF(MDUP(N).LT.2) THEN

C  If this part has not been identified as a dupl., try to merge it in
C  -------------------------------------------------------------------

                  ITBL_THIS = REC_8(3,IREP)
cpppp
ccc               print *, 'This part is unique and will be merged ',
ccc  $             'into the complete report via UARMRG'
ccc               print *, 'For part number ',N,' IREP,MREC,MSUB,
ccc  $             itbl_this,itbl_last: ',IREP,MREC,MSUB,itbl_this,
ccc  $             itbl_last
cpppp
                  IF(ITBL_THIS.NE.ITBL_LAST.AND.ITBL_LAST.NE.-99) THEN

C  This part was generated from a different BUFR table than the
C   previous part that was merged in
C  ------------------------------------------------------------

                     PRINT 1989, CRPID_IREC,(TAB_8(II,IREC),II=1,2),
     $                (NINT(TAB_8(II,IREC)),II=3,5)
 1989                FORMAT(' BUFR TABLE DISAGREEMENT THIS PART vs. ',
     $                   'PREVIOUS PART: STNID ',A8,'; LAT ',F6.2,
     $                   ' (N+,S-), LON ',F7.2,' (E+/W-); OBS DDHHMM ',
     $                   3I2.2)
                     IF(SKIP_BTBL_DISAGR) then

C  ... come here if code is set up to skip this part (never merged in)
C      ---------------------------------------------------------------

     
                        print *,' ... SKIP the merge of this part, ',
     $                   'move on to next part!!!'
     
                        NSKIP = NSKIP + 1
                     ELSE

C  ... come here if code is set up to make a new, unique report
C       starting with this part
C      --------------------------------------------------------
     
                        NCOUNT = NCOUNT - 1
                        EXIT
                     ENDIF
                  ELSE

C  This part was generated from the same BUFR table as the previous
C   part, or this is the first part - it will be merged in
C  ----------------------------------------------------------------

                     CALL UARMRG(MREC,MSUB)
                     ITBL_LAST = ITBL_THIS
                  endif
               ENDIF
            ENDDO

C  Encode the new complete (merged) report into the output file
C  ------------------------------------------------------------

            CALL UARWRT
            NREP(NTABLES) = NREP(NTABLES) + 1
            IF(NCOUNT.LT.NPAR+1) THEN

C  If a BUFR table disagreement was found during looping to merge (and
C   code is set up to make a new, unique report starting with this
C   part), go back and loop again through the unique parts - starting
c   with this one - in order to create a single new report (which will
C   have a duplicate location/obs-time with the previous report)
C --------------------------------------------------------------------

               PRINT'(" ... begin process of merging parts with new '//
     $          'BUFR table into a separate, new report")'
               NPAR = NPAR - NCOUNT
               GO TO 30
            ENDIF

C  All done with this group of "parts" with same location/obs-time,
C   move on to next group of like-parts in sorted input file
C  ----------------------------------------------------------------

            GO TO 10
         ELSE

C  This group of "parts" with same location/obs-time are outside time
C   window, keep a count then move on to next group of like-parts in
C   sorted input file
C  ------------------------------------------------------------------

            NDUP(3) = NDUP(3) + NPAR + 1
            GO TO 10
         ENDIF
      ENDDO
 
   20 CONTINUE
 
      CALL CLOSBF(LUBFI)
      CALL CLOSBF(LUBFJ)
      OPEN(LUBFJ,FILE=FILO(1:NBYTES_FILO),FORM='UNFORMATTED')
      CALL MESGBC(LUBFJ,MSGT,ICOMP)
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
      CLOSE(LUBFJ)
 
C  GENERATE REPORT
C  ---------------
 
      NREP_TOTAL = NREP(1)+NREP(2)+NREP(3)+NREP(4)+NREP(5)
      PRINT 300, NTAB,NDUP(0),NDUP(1),NDUP(2),NDUP(3),NREP_TOTAL
  300 FORMAT(/'BUFR_DUPMRG READ IN A TOTAL OF',I8,' REPORT PARTS'/
     .        '   NUMBER THAT ARE UNCORRECTED (AND USED IN MERGING ',
     .                   'PROCESS) .....',I7/
     .        '   NUMBER THAT ARE CORRECTED (AND USED IN MERGING ',
     .                   'PROCESS) .......',I7/
     .        '   NUMBER THAT ARE DUPLICATES (THESE ARE SKIPPED) ',
     .                   '................',I7/
     .        '   NUMBER THAT ARE OUTSIDE TIME WINDOW FOR ',
     .                   'TRIMMING (SKIPPED) ....',I7//
     .        'TOTAL NUMBER OF REPORTS WRITTEN OUT (SOME CREATED FROM ',
     .                   'THE MERGING OF INPUT REPORTS) ...',I7/)
      IF(NSKIP.GT.0) THEN
         PRINT 301, NSKIP
  301 FORMAT('NUMBER OF INSTANCES WHERE DUPLICATE REPORT PARTS COULD ',
     . 'NOT BE MERGED DUE TO THEIR HAVING'/'   BEEN GENERATED FROM ',
     . 'DIFFERENT BUFR TABLES .......................................',
     . '....',I7/)
      ENDIF


      DO II = 5,2,-1
         IF(NREP(II).GT.0)  THEN
         PRINT 305, NREP_TOTAL,
     $    NREP(II)-NREP(II+1)-NREP(II+2)-NREP(II+3),II
 305  FORMAT(' OF THESE ',I7, ' RPTS,',I5,' ARE DUPLICATES IN LOCATION',
     $ ' & TIME CREATED FROM "PARTS" UNIQUE TO ',I1,' DIFFERENT ',
     $ 'EMBEDDED BUFR TABLES'/)
         CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg'//
     $    ' "$jlogfile" "***WARNING: ONE OR MORE  REPORTS IN OUTPUT '//
     $    'DUMP FILE '//SUBSET//' ARE LOCATION-TIME DUPLICATES '//
     $    'CREATED FROM -PARTS- UNIQUE TO 2 OR MORE DIFFERENT '//
     $    'EMBEDDED BUFR TABLES"')
         ENDIF
      ENDDO

      CALL MRGINV
 
C  END OF PROGRAM
C  --------------
 
      CALL W3TAGE('BUFR_DUPMRG')
      STOP
 
C  ERROR EXITS
C  -----------
 
  900 CONTINUE
 
      PRINT *, '#####BUFR_DUPMRG - EOF/ERR READING STDIN'
      CALL W3TAGE('BUFR_DUPMRG')
      CALL ERREXIT(99)
 
  901 CONTINUE

      PRINT *, '#####BUFR_DUPMRG - UNABLE TO ALLOCATE ARRAYS'
      CALL W3TAGE('BUFR_DUPMRG')
      CALL ERREXIT(99)

  902 CONTINUE

      PRINT'("#####BUFR_DUPMRG - BAD RETURN FROM BUFRLIB ROUTINE '//
     . 'RDMEMM - REQ. MEMORY MSG NO. TO READ IN (",I0,") > NO. OF '//
     . 'MSGS IN MEMORY")', I
      CALL W3TAGE('BUFR_DUPMRG')
      CALL ERREXIT(99)

      END
 
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    UARMERG
C   PRGMMR: KEYSER           ORG: NP22       DATE: 2014-11-07
C
C ABSTRACT: PERFORMS MERGING OF UPPER-AIR PARTS INTO A FINAL COMPLETE
C   REPORT. REMOVES PARTS WHICH ARE DUPLICATES. WRITES THE COMPLETE
C   REPORT TO THE OUTPUT BUFR FILE.  THIS IS ACCOMPLISHED THROUGH FOUR
C   ENTRIES "UARINI", "UARDUP", "UARMRG" AND "UARWRT".
C
C PROGRAM HISTORY LOG:
C 1996-09-06  J. WOOLLEN  ORIGINAL VERSION FOR IMPLEMENTATION
C 1996-11-27  J. WOOLLEN  ADDED NEW OUTPUT STACKS FOR BID AND UARID
C 1996-11-28  J. WOOLLEN  FIXED PROBLEM WITH LAST REPORT NOT BEING
C     MERGED
C 1996-12-04  J. WOOLLEN  FIXED PROBLEM WITH KEEPING DUMP TIME MESSAGES
C 1996-12-12  J. WOOLLEN  DON'T COPY DUMP DATE MESSAGES IF THEY DON'T
C     EXIST
C 1997-06-17  J. WOOLLEN  CHANGE CRITERIA FOR MERGING DROPSONDE PARTS
C 1999-06-03  D. KEYSER   MODIFIED TO PORT TO IBM SP AND RUN IN 4 OR
C     8 BYTE STORAGE
C 2002-03-05  D. KEYSER   IMPROVED DOCUMENTATION; ACCOUNTS FOR CHANGES
C     IN INPUT UPPER-AIR PROFILE BUFR DUMP FILES AFTER 3/2002: MNEMONIC
C     "HBLCS" REPLACES "HOCB" IN REPLICATED "UACLD" SEQUENCE; MNEMONIC
C     "BORG" REPLACES "ICLI" IN REPLICATED "BID" SEQUENCE (WILL STILL
C     WORK PROPERLY FOR INPUT UPPER-AIR PROFILE BUFR DUMP FILES PRIOR
C     TO 3/2002)
C 2010-02-22  D. KEYSER   ADDED NEW ENTRY "UARDUP" WHICH IDENTIFIES
C     DUPLICATE PARTS (THIS USED TO BE DONE IN ENTRY "UARMRG").
C     MODIFIED ENTRY "UARMRG" TO REMOVE DUPLICATE PART TESTING (SINCE
C     THIS IS NOW DONE IN NEW ENTRY "UARDUP"), IT NOW ASSUMES THAT ONLY
C     UNIQUE PARTS CAN CALL IT.  NOTE: THESE CHANGES ARE NEEDED TO
C     PREVENT MERGING OF PARTS GENERATED FROM DIFFERENT EMBEDDED BUFR
C     TABLES (WHICH CAUSES A CODE FAILURE).
C 2013-01-14  J. WHITING  REPLACED TESTS VS BMISS W/ IBFMS FUNCTION;
C     REPLACED EXPLICIT ASSIGNMENT OF BMISS W/ GETBMISS() FUNCTION.
C 2014-11-07  D. KEYSER   REMOVE ASSIGNMENT OF BMISS W/ GETBMISS()
C     FUNCTION AND INSTEAD PASS IN VALUE SET IN MAIN PROGRAM VIA
C     COMMON; DECLARE BMISS AS REAL*8 TO GET A RELIABLE VALUE IN PRINT
C     STATEMENTS.
C
C USAGE:    CALL UARMERG
C
C$$
C
C ENTRY:         UARINI
C
C ABSTRACT: ENTRY POINT UARINI INITIALIZES THE ACCUMULATION SPACE.
C
C USAGE:    CALL UARINI
C
C$$
C
C ENTRY:         UARDUP
C
C ABSTRACT: ENTRY POINT UARDUP IDENTIFIES DUPLICATE PARTS OF AN UPPER
C   AIR REPORT.
C
C USAGE:    CALL UARDUP(RCTS,CORN,MREC,MSUB,IDUP)
C   INPUT ARGUMENT LIST:
C     RCTS     - RECEIPT TIME SIGNIFICANCE (SEE REMARKS)
C     CORN     - CORRECTED REPORT INDICATOR (SEE REMARKS)
C     MREC     - THE BUFR MESSAGE NUMBER IN THE INPUT FILE CONTAINING
C              - THIS REPORT
C     MSUB     - THE SUBSET NUMBER IN "MREC" CONTAINING THIS REPORT
C
C   OUTPUT ARGUMENT LIST:
C     IDUP     - DUPLICATE INDICATOR (SEE REMARKS)
C
C$$
C
C ENTRY:         UARMRG
C
C ABSTRACT: ENTRY POINT UARMRG ACCUMULATES UNIQUE PARTS OF AN UPPER
C   AIR REPORT.
C
C USAGE:    CALL UARMRG UARMRG(MREC,MSUB)
C   INPUT ARGUMENT LIST:
C     MREC     - THE BUFR MESSAGE NUMBER IN THE INPUT FILE CONTAINING
C              - THIS REPORT
C     MSUB     - THE SUBSET NUMBER IN "MREC" CONTAINING THIS REPORT
C
C$$
C
C ENTRY:         UARWRT
C
C ABSTRACT: ENTRY UARWRT WRITES THE MERGED SEQUENCES TO THE MERGED
C   REPORT,
C
C USAGE:    CALL UARWRT
C
C$$
C
C   INPUT FILES:
C     UNIT "LUBFI"  - UNCHECKED AND UNMERGED TIME WINDOWED BUFR FILE
C                   - ("LUBFI" PASSED IN THROUGH COMMON)
C
C   OUTPUT FILES:
C     UNIT "LUBFJ"  - DUPLICATE CHECKED AND MERGED TIME WINDOWED BUFR
C                   - FILE ("LUBFJ" PASSED IN THROUGH COMMON)
C
C   SUBPROGRAMS CALLED (incomplete list0:
C     LIBRARY:
C       BUFRLIB  - IBFMS
C
C REMARKS:
C   RECEIPT TIME SIGNIFICANCE (RCTS):
C         0.      General decoder receipt time
C         1.      NCEP receipt time
C         2.      OSO  receipt time
C         3.      ARINC ground station receipt time
C         4.      Radiosonde TEMP AA part receipt time
C         5.      Radiosonde TEMP BB part receipt time
C         6.      Radiosonde TEMP CC part receipt time
C         7.      Radiosonde TEMP DD part receipt time
C         8.      Radiosonde PILOT AA part receipt time
C         9.      Radiosonde PILOT BB part receipt time
C        10.      Radiosonde PILOT CC part receipt time
C        11.      Radiosonde PILOT DD part receipt time
C      12.-62.    Reserved for future use
C        63.      Missing value
C
C   CORRECTED REPORT INDICATOR (CORN):
C         0.      Not corrected
C         1.      Corrected by report originator
C         2.      Corrected by NCEP SDM
C        3.-6.    Reserved
C         7.      Missing value
C
C   DUPLICATE INDICATOR (IDUP):
C         0       Uncorrected report part merged into a complete report
C         1       Corrected report part merged into a complete report
C         2       Duplicate to a previous report part - toss
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  WCOSS
C
C$$$
      SUBROUTINE UARMERG
 
      COMMON /UNITS / LUBFI,LUBFJ
      COMMON /VERSION_FLAGS/ IBORG,IHBLCS
      COMMON /UARCOM/ UARLV_8(8,255),UARCT_8(6,255),UABID_8(5,255),
     .                UARID_8(4,255),UACLD_8(4,255),UARAW_8(  255),
     .                NUARLV,NUARCT,NUABID,NUARID,NUACLD,NUARAW
      COMMON /BUFRLIB_MISSING/BMISS
 
      CHARACTER*80 UARSTR,RCTSTR,RAWSTR,RIDSTR,BIDSTR_N,BIDSTR_O,
     .             CLDSTR_N,CLDSTR_O
      CHARACTER*8  SUBSET
      CHARACTER*8  PART
      REAL(8)      DATA_8(10,255),UARLV_8,UARCT_8,UABID_8,UARID_8,
     .             UACLD_8,UARAW_8,RPART_8,BMISS
      EQUIVALENCE (PART,RPART_8)
 
      DATA UARSTR  /'VSIG PRLC GP07 GP10 TMDB TMDP WDIR WSPD  '/
      DATA RCTSTR  /'RCTS RCYR RCMO RCDY RCHR RCMI            '/
      DATA BIDSTR_N/'SEQNUM BUHD BORG BULTIM BBB              '/
      DATA BIDSTR_O/'SEQNUM BUHD ICLI BULTIM BBB              '/
      DATA RIDSTR  /'RATP A4ME CORN UAPART                    '/
      DATA CLDSTR_N/'CLTP QMCA CLAM HBLCS                     '/
      DATA CLDSTR_O/'CLTP QMCA CLAM HOCB                      '/
      DATA RAWSTR  /'RRSTG                                    '/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
C  ENTRY POINT UARINI INITIALIZES THE ACCUMULATION SPACE
C  -----------------------------------------------------
 
      ENTRY UARINI
      UARLV_8 = BMISS
      UARCT_8 = BMISS
      UABID_8 = BMISS
      UARID_8 = BMISS
      UACLD_8 = BMISS
      UARAW_8 = BMISS
      NUARLV = 0
      NUARCT = 0
      NUABID = 0
      NUARID = 0
      NUACLD = 0
      NUARAW = 0
      RETURN
 
C  ENTRY POINT UARDUP CHECKS TO SEE IF THIS PART IS A DUPLICATE OF A
C   PART ALREADY IDENTIFIED
C  -----------------------------------------------------------------
 
      ENTRY UARDUP(RCTS,CORN,MREC,MSUB,IDUP)
 
C  FIRST CHECK TO SEE IF THIS IS A CORRECTED REPORT
C  ------------------------------------------------
 
      IDUP = MIN(1,NINT(CORN))
      IF(IBFMS(CORN).EQ.1) IDUP = 0                  ! data missing
 
C  NOW CHECK TO SEE IF THIS PART IS A DUPLICATE
C  --------------------------------------------
 
cpppp
ccc   print *, 'NUARCT = ',NUARCT
cpppp
      IF(NUARCT.GT.0) THEN
         DO N=1,NUARCT
            UARCT=UARCT_8(1,N)
            IF(RCTS.EQ.UARCT) THEN
               IDUP = 2    ! The receipt time significance agrees with
                           !  a previous part meaning this is a
                           !  fully duplicate report -- don't process
cpppp
ccc   print *, 'The receipt time significance for this part (',
ccc  $ nint(RCTS),') agrees with a previous part meaning this is a ',
ccc  $ 'duplicate report -- do not process'
cpppp

               RETURN
            ENDIF
         ENDDO
      ENDIF
      
C  THIS PART IS NOT A DUPLICATE, SO IT WILL BE USED IN THE MERGING
C   PROCESS LATER ON
C  ---------------------------------------------------------------

C  READ THE PART OUT OF MEMORY IN ORDER TO ACCUMULATE ITS RECEIPT TIME
C   DATA (FOR TEST OF RECEIPT TIME SIGNIFICANCE FOR THE NEXT PART
C   COMING INTO THIS ENTRY POINT)
C  -------------------------------------------------------------------
 
      CALL UFBMMS(MREC,MSUB,SUBSET,IDATE)

      CALL UFBINT(LUBFI,DATA_8,10,255,NLEV,RCTSTR)
 
      NUARCT = MIN(255,NUARCT+1)
      UARCT_8(1:6,NUARCT) = DATA_8(1:6,1)

      RETURN

C  ENTRY POINT UARMRG ACCUMULATES (MERGES) THE UNIQUE PARTS OF AN UPPER
C   AIR REPORT
C  --------------------------------------------------------------------
 
      ENTRY UARMRG(MREC,MSUB)
 
C  READ THE PART OUT OF MEMORY
C  ---------------------------
 
      CALL UFBMMS(MREC,MSUB,SUBSET,IDATE)
 
cpppp
ccc   print *, 'NUARCT = ',NUARCT
cpppp
      IF(NUARCT.EQ.0) CALL OPENMB(LUBFJ,SUBSET,IDATE)
      CALL INVMRG(LUBFI,LUBFJ)
 
C  ACCUMULATE THE LEVEL DATA FROM THIS PART
C  ----------------------------------------
 
      CALL UFBINT(LUBFI,DATA_8,10,255,NLEV,UARSTR)
 
      DO M=1,NLEV
         if(NUARLV+1.gt.255) then
           print *, 'WARNING!!!! - this level exceeds limit of 255,',
     $              'cannnot add it to accumulation'
         end if
         NUARLV = MIN(255,NUARLV+1)
         UARLV_8(1:8,NUARLV) = DATA_8(1:8,M)
      ENDDO
 
C  ACCUMULATE THE RECEIPT TIME DATA FROM THIS PART
C  -----------------------------------------------
 
      CALL UFBINT(LUBFI,DATA_8,10,255,NLEV,RCTSTR)
 
      NUARCT = MIN(255,NUARCT+1)
      UARCT_8(1:6,NUARCT) = DATA_8(1:6,1)
 
C  ACCUMULATE THE BULLETIN ID DATA FROM THIS PART
C  ----------------------------------------------
 
      IF(IBORG.GT.0)  THEN
         CALL UFBINT(LUBFI,DATA_8,10,255,NLEV,BIDSTR_N)  ! > 3/2002
      ELSE
         CALL UFBINT(LUBFI,DATA_8,10,255,NLEV,BIDSTR_O)  ! < 3/2002
      ENDIF
 
      NUABID = MIN(255,NUABID+1)
      UABID_8(1:5,NUABID) = DATA_8(1:5,1)
 
C  ACCUMULATE THE RADIOSONDE ID DATA FROM THIS PART
C  ------------------------------------------------
 
      CALL UFBINT(LUBFI,DATA_8,10,255,NLEV,RIDSTR)
 
      NUARID = MIN(255,NUARID+1)
      UARID_8(1:4,NUARID) = DATA_8(1:4,1)
 
C  ACCUMULATE THE CLOUD DATA FROM THIS PART
C  ----------------------------------------
 
      IF(IHBLCS.GT.0)  THEN
         CALL UFBINT(LUBFI,DATA_8,10,255,NLEV,CLDSTR_N)  ! > 3/2002
      ELSE
         CALL UFBINT(LUBFI,DATA_8,10,255,NLEV,CLDSTR_O)  ! < 3/2002
      ENDIF
 
      DO M=1,NLEV
         NUACLD = MIN(255,NUACLD+1)
         UACLD_8(1:4,NUACLD) = DATA_8(1:4,M)
      ENDDO
 
C  ACCUMULATE THE RAW BULLETIN DATA FROM THIS PART
C  -----------------------------------------------
 
      CALL UFBINT(LUBFI,DATA_8,10,255,NLEV,RAWSTR)
 
      DO M=1,NLEV
         NUARAW = MIN(255,NUARAW+1)
         UARAW_8(NUARAW) = DATA_8(1,M)
      ENDDO
 
      RETURN
 
C  ENTRY UARWRT WRITES THE MERGED SEQUENCES TO THE MERGED REPORT
C  -------------------------------------------------------------
 
      ENTRY UARWRT
 
C  SOME WORK INVOLVED IN GETTING THE RATP DESCRIPTOR CORRECTLY FROM TTBB
C  ---------------------------------------------------------------------
 
      DO N=1,NUARID
         RPART_8=UARID_8(4,N)
         IF(IBFMS(UARID_8(1,N)).EQ.0 .AND. PART.EQ.'TTBB    ') THEN  ! UARID_8(1,N) not missing
            DO M=1,NUARID
               UARID_8(1,M) = UARID_8(1,N)
            ENDDO
            GO TO 10
         ENDIF
      ENDDO
 
C  IF TTBB DOES NOT REPORT A RATP CODE THEN USE THE FIRST AVAILABLE
C  ----------------------------------------------------------------
 
      DO N=1,NUARID
         IF(IBFMS(UARID_8(1,N)).EQ.0) THEN            ! data not missing
            DO M=1,NUARID
               UARID_8(1,M) = UARID_8(1,N)
            ENDDO
            GO TO 10
         ENDIF
      ENDDO
 
C  OVERWRITE THE MERGED SEQUENCES INTO THE SUBSET
C  ----------------------------------------------
 
   10 CONTINUE
 
      IF(NUARLV.GT.0)  CALL UFBOVR(LUBFJ,UARLV_8,8,NUARLV,IRET,UARSTR)
      IF(NUARCT.GT.0)  CALL UFBOVR(LUBFJ,UARCT_8,6,NUARCT,IRET,RCTSTR)
      IF(NUABID.GT.0) THEN
         IF(IBORG.GT.0)  THEN
            CALL UFBOVR(LUBFJ,UABID_8,5,NUABID,IRET,BIDSTR_N)
         ELSE
            CALL UFBOVR(LUBFJ,UABID_8,5,NUABID,IRET,BIDSTR_O)
         ENDIF
      ENDIF
      IF(NUARID.GT.0) CALL UFBOVR(LUBFJ,UARID_8,4,NUARID,IRET,RIDSTR)
      IF(NUACLD.GT.0)  THEN
         IF(IHBLCS.GT.0)  THEN
            CALL UFBOVR(LUBFJ,UACLD_8,4,NUACLD,IRET,CLDSTR_N)
         ELSE
            CALL UFBOVR(LUBFJ,UACLD_8,4,NUACLD,IRET,CLDSTR_O)
         ENDIF
      ENDIF
      IF(NUARAW.GT.0) CALL UFBOVR(LUBFJ,UARAW_8,1,NUARAW,IRET,RAWSTR)
      CALL WRITSB(LUBFJ)
 
      RETURN
 
      END
