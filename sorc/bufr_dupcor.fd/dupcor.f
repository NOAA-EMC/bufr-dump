C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_DUPCOR
C   PRGMMR: DONG             ORG: NP22        DATE: 2021-09-02
C
C ABSTRACT: PROCESSES NON PROFILE DATABASE REPORTS WITH CORRECTION
C   CHOOSING, DUPLICATE CHECKING (DEPENDING UPON TYPE) AND
C   (OPTIONAL) TRIMMING TO EXACT TIME WINDOW (MONTH DOWN TO MINUTE).
C   WHEN DUPLICATE CHECKING IS PERFORMED (CURRENTLY FOR ALL TYPES
C   EXCEPT FOR THOSE AIRCRAFT TYPES THAT ARE LATER DUPLICATE CHECKED IN
C   PROGRAM BUFR_DUPAIR) THE ALGORITHM SORTS THE REPORTS IN ASCENDING
C   ORDER OF LAT, LON, OBS TIME (MONTH DOWN TO MINUTE), CORRECTION
C   INDICATOR, REPORT ID (FOR SOME TYPES) AND RECEIPT TIME (YEAR DOWN
C   TO MINUTE).  IN THE DUPLICATE CHECKING, THE REPORT USUALLY SELECTED
C   IS THE BULLETIN LAST RECEIVED OR CORRECTED.  FOR TYPES BEING
C   DUPLICATE-CHECKED, REPORTS ARE CHECKED FOR LAT, LON AND OBS TIME
C   (MONTH DOWN TO MINUTE) ALL INSIDE THE TOLERANCE LIMITS.  THE FILE
C   PATH/NAMES OF THE INPUT AND OUTPUT FILES, (OPTIONALLY) THE TIME
C   WINDOW TO TRIM TO AND (OPTIONALLY) DEFAULT OVERRIDE DUP-CHECKING
C   TOLERANCE LIMITS ARE READ FROM STANDARD INPUT AT THE START OF THIS
C   PROGRAM.  IF THE TIME WINDOW RECORD IS MISSING, THEN NO TIME WINDOW
C   TRIMMING IS PERFORMED.  ALL FILE CONNECTIONS (EXCEPT STANDARD INPUT
C   WHICH IS PRE-CONNECTED) ARE MADE THROUGH THE FORTRAN OPEN STATEMENT.
C
C PROGRAM HISTORY LOG:
C 1996-09-06  J. WOOLLEN  ORIGINAL VERSION FOR IMPLEMENTATION
C 1996-02-01  J. WOOLLEN  MODIFIED OPERATION FOR 004001, 004002, 004003
C 1998-12-02  J. WOOLLEN  Y2K/F90 VERSION
C 1999-06-03  D. KEYSER   MODIFIED TO PORT TO IBM SP AND RUN IN 4 OR
C     8 BYTE STORAGE
C 2000-10-11  D. KEYSER   INCREASED MXTB FROM 50000 TO 100000 TO
C     HANDLE LARGER VOLUMES OF DATA (ESP. SURFACE SYNOPTIC)
C 2000-12-05  D. KEYSER   INCREASED LIMIT FOR I/O FILENAME LENGTH
C     FROM 80 CHARACTERS TO 500 CHARACTERS
C 2002-09-18  D. KEYSER   ADDED INLINE VERSION OF BUFRLIB ROUTINE
C     UFBTAB WHICH WILL NOT ABORT WHEN THERE ARE TOO MANY REPORTS
C     (> MXTB) INPUT - RATHER IT WILL JUST PROCESS MXTB REPORTS BUT
C     PRINT A DIAGNOSTIC (NOTE: THIS IS A TEMPORARY CHANGE UNTIL THE
C     NEXT VERSION OF THE BUFRLIB IS IMPLEMENTED WITH THE UPDATED
C     UFBTAB)
C 2003-09-02  D. STOKES   ADDED MONTH TO TIME-WINDOW CHECK IN ORDER TO
C     ALLOW WINDOWS GREATER THAN 1-MONTH TO BE PROPERLY CHECKED (DELTA-
C     MONTH HARDWIRED TO ZERO IN DUPLICATE CHECK)
C 2003-09-02  J. WHITING  INCREASED MXTB FROM 100000 TO 300000 TO
C     HANDLE LARGER VOLUMES OF DATA (ESP. GFO CALTIMETER, NGFOAL);
C     ADDED HI-RES ALTIMETERS (TYPES NC03110[1-4]) TO LIST OF TYPES
C     THAT KEY ON HI-ACCURACY LAT/LON (CLATH/CLONH) (SEE CHANGE BELOW)
C 2003-09-02  D. KEYSER   NOW USES CLATH AND CLONH IN DUPLICATE
C     CHECK FOR E-ADAS REPORTS (BUFR TYPE 004, SUBTYPE 006); SET-UP TO
C     UNDERSTAND THAT E-ADAS REPORTS WILL LATER BE CHECKED BY
C     BUFR_DUPAIR; MODIFICATIONS TO PREVENT ARRAY OVERFLOW WHEN THERE
C     ARE > MXTB REPORTS; IMPROVED DIAGNOSTIC PRINT, CORRECTED ERRORS
C     IN ACCUMULATING COUNTS FOR FINAL PRINT SUMMARY; ADDED CALL TO NEW
C     BUFRLIB ROUTINE MESGBC TO INDICATE IF INPUT/OUTPUT FILES ARE
C     COMPRESSED OR UNCOMPRESSED; REMOVED IN-LINE VERSION OF UFBTAB
C     (CHANGES NOTED IN PREVIOUS IMPLEMENTATION ARE NOW IN BUFRLIB
C     VERSION)
C 2003-12-11  D. KEYSER   NO LONGER TESTS A REPORT'S BUFR TYPE/SUBTYPE
C     TO SEE IF IT SHOULD KEY ON HI-ACCURACY LAT/LON (CLATH/CLONH) IN
C     DUPLICATE CHECKS, RATHER JUST CHECKS IF FIRST SUBSET HAS MISSING
C     LOW-ACCURACY LAT (CLAT) - IF SO ASSUMES ALL REPORTS ENCODE CLATH/
C     CLONH - GENERALIZES THIS CHECK AND ALLOWS FOR REPORT TYPES WHICH
C     MAY CHANGE FROM CLAT/CLON TO CLATH/CLONH AT SOME POINT IN TIME
C     (AND WILL STILL WORK FOR HISTORICAL RUNS PRIOR TO CHANGEOVER)
C 2004-09-12  D. KEYSER   IF MINUTES ARE MISSING, SET TO ZERO (NEEDED
C     FOR AIRNOW DATA); NO LONGER FAILS IF INPUT FILE HAS COMPRESSED
C     BUFR MESSAGES, INSTEAD UNCOMPRESSES MESSAGES AND COPIES THEM TO A
C     SCRATCH FILE (UNIT 91) WHICH UFBTAB CAN THEN READ (NOTE: OUTPUT
C     DUP-CHECKED FILE WILL REMAIN UNCOMPRESSED), NECESSARY BECAUSE
C     AIRNOW FILES CONTAIN COMPRESSED BUFR MESSAGES
C 2005-02-02  D. KEYSER   MODIFIED NAME OF SCRATCH FILE CONTAINING
C     UNCOMPRESSED BUFR MESSAGES TO BE UNIQUE FOR THE PARTICULAR BUFR
C     TYPE AND SUBTYPE BEING PROCESSED, THIS PREVENTS PARALLEL RUNS OF
C     THIS PROGRAM FROM READING TO/WRITING FROM THE SAME FILE - THIS
C     SCRATCH FILE IS PRESENT ONLY IF THE INPUT BUFR FILE CONTAINS
C     COMPRESSED BUFR MESSAGES
C 2005-10-19  D. KEYSER   INTRODUCED ALLOCATABLE ARRAYS TO AVOID ARRAY
C     OVERFLOW PROBLEMS, DETERMINES SIZE OF ARRAYS BY CALLING UFBTAB
C     WITH NEGATIVE UNIT NUMBER TO SIMPLY COUNT SUBSETS; FOR TYPES
C     STORED IN COMPRESSED BUFR MESSAGES (CURRENTLY ONLY AIRNOW)
C     ENABLED CAPACITY TO MORE EFFICIENTLY PERFORM DUP-CHECK VIA DIRECT
C     CALL TO BUFRLIB ROUTINE UFBTAB RATHER THAN HAVING TO FIRST
C     UNCOMPRESS THE ENTIRE FILE BEFORE CALLING UFBTAB, DONE THROUGH
C     TEMPORARY IN-LINE VERSIONS OF BUFRLIB ROUTINES UFBTAB AND COPYSB
C     WHICH CAN NOW HANDLE COMPRESSED MESSAGES (THESE WILL BE
C     IMPLEMENTED IN NEXT VERSION OF BUFRLIB AFTERWHICH THEY SHOULD BE
C     REMOVED HERE); IF TIME WINDOW RECORD (LINE 3) IS EMPTY IN
C     STANDARD INPUT, TIME WINDOW TRIMMING LOOP BYPASSED, BASED ON
C     TEST FOR DEFAULT VALUE OF LATEST REQUESTED DATE WHICH IS NOW
C     CORRECTED TO BE 99999999.00_8 (WAS 99999999.00 BUT IN 4-BYTE REAL
C     MACHINES THIS COULD BE ROUNDED UP TO 100000000.00 MEANING LASTEST
C     DDHH.hh IN TIME TESTS, BEFORE TIME WINDOW TRIMMING LOOP WAS
C     BYPASSED, WOULD BE 0000 AND ALL REPORTS WOULD BE TRIMMED RATHER
C     THAN RETAINED)
C 2006-03-02  D. KEYSER   CHECKS TO SEE IF INPUT BUFR FILE CONTAINS
C     "DUMMY" MESSAGES CONTAINING DUMP CENTER TIME AND PROCESSING TIME,
C     RESP. IN FIRST TWO MESSAGES OF INPUT FILE (AFTER TABLE MSGS) BY
C     CHECKING THE VALUE OF DUMPJB SCRIPT VARIABLE "DUMMY_MSGS" (READ
C     IN VIA "GETENV") - IF NOT WILL NOT PROCESS INPUT BUFR MESSAGES
C     WITH ZERO SUBSETS AND WILL CALL BUFRLIB ROUTINE CLOSMG WITH A
C     NEGATIVE UNIT NUMBER ARGUMENT PRIOR TO ALL PROCESSING IN ORDER TO
C     SIGNAL IT THAT ANY OUTPUT BUFR MESSAGES WITH ZERO SUBSETS SHOULD
C     BE SKIPPED (NOT WRITTEN OUT) - CODE HAD BEEN HARDWIRED TO ALWAYS
C     ASSUME DUMMY MESSAGES WERE IN THE INPUT FILE; NOW STOPS
C     IMMEDIATELY IN MESSAGE COMPRESSION CHECK IF INPUT FILE DOES NOT
C     EXIST (BEFORE DID DO AFTER OPENBF, TRIGGERING A BUFRLIB WARNING
C     MESSAGE); REMOVES IN-LINE VERSIONS OF UFBTAB AND COPYSB (UPDATED
C     TO HANDLE COMPRESSED BUFR MESSAGES IN 1/31/2006 VERSION OF
C     BUFRLIB)
C 2007-03-23  D. KEYSER   CORRECTED METHOD OF CALLING UFBTAB WITH
C     NEGATIVE UNIT NUMBER TO COUNT SUBSETS FOR USE WITH EXISTING
C     ALLOCATABLE ARRAYS, PREVIOUS LOGIC COULD HAVE LEAD TO ARRAY
C     OVERFLOW PROBLEMS - STREAMLINED THIS LOGIC AS WELL
C 2008-05-05  D. KEYSER   TEST FOR LAT/LON TYPE (HIGH-RES OR LOW-RES)
C     NOW LOOPS THROUGH UP TO FIRST 25 REPORTS LOOKING FOR FIRST REPORT
C     WITH EITHER A NON-MISSING HIGH-RES LATITUDE OR A NON-MISSING LOW-
C     RES LATITUDE - THIS THEN DETERMINES IF THIS REPORT TYPE ENCODES
C     LOW- OR HIGH-RES LAT/LON, BEFORE ALWAYS ASSUMED A REPORT TYPE'S
C     LAT/LON WAS HIGH-RES IF FIRST REPORT'S LOW-RES LATITUDE WAS
C     MISSING, A PROBLEM IF THE FIRST FEW REPORTS IN A LOW-RES LAT/LON
C     REPORT TYPE JUST HAPPEN TO HAVE MISSING LATITUDES BUT ALL OTHERS
C     ARE VALID (NOTE: POSTS A WARNING MESSAGE TO THE PRODUCTION JOBLOG
C     IF FIRST 1 OR MORE REPORTS HAVE A MISSING LATITUDE); IF A
C     REPORT'S MONTH AND DAY ARE MISSING, LOOKS FOR DAY-OF-YEAR WHICH
C     MIGHT BE STORED INSTEAD (E.G., FOR GOME REPORTS) AND CONVERTS IT
C     TO MONTH AND DAY VIA NEW IN-LINE SUBROUTINE REMTDY - DUPLICATE
C     CHECKING AND, ESPECIALLY, TIME TRIMMING CAN THEN PROCEED PROPERLY
C 2008-10-10  D. KEYSER   REMOVED LOGIC ADDED IN LAST IMPLEMENTATION
C     WHICH CALLS BUFRLIB ROUTINE OPENBF FOR THE INPUT BUFR FILE PRIOR
C     TO CHECKING TO SEE IF THE INPUT FILE IS EMPTY (IF THE INPUT FILE
C     IS EMPTY, THIS RESULTS IN A BUFRLIB WARNING MESSAGE POSTED TO
C     STDOUT WHICH IS NOT DESIRABLE)
C 2010-12-10  D. KEYSER   ADDED LOGIC SIMILAR TO THAT IN BUFR_DUPMAR TO
C     ALLOW REPORTS TO BE PROPERLY CHECKED IN CASES WHERE THIS CODE IS
C     EXECUTED AFTER BUFR_DUPMAR {THIS EXECUTION AFTER BUFR_DUPMAR HAS
C     OCCURRED SINCE 4/2004 FOR MESONET REPORTS OVER ALL SITUATIONS AND
C     WILL NOW ALSO OCCUR IN CASES WHERE BUFR_DUPMAR DETECTS MORE THAN
C     ONE EMBEDDED BUFR TABLE IN THE INPUT DATABASE FILE (INCLUDING THE
C     TABLE AT THE TOP) FOR METAR OR SURFACE MARINE REPORTS WHICH ARE
C     ALSO PROCESSED FIRST BY BUFR_DUPMAR (SEE BUFR_DUPMAR FOR MORE
C     INFORMATION ON WHY BUFR_DUPCOR CAN NOW BE CALLED AS A FOLLOW-UP
C     DUP-CHECK) - SUCH CHANGES INCLUDE: 1) FOR METARS AND SURFACE
C     MARINE REPORTS ONLY, ADDING REPORT ID TO THE SORT AND DUP-CHECK
C     (AS IS DONE IN BUFR_DUPMAR TO RETAIN TWO DIFFERENT REPORT ID'S AT
C     THE SAME LOCATION AND TIME - DO NOT WANT TO DO THIS FOR MESONETS
C     AS BUFR_DUPCOR UP UNTIL NOW HAS RUN AS A FOLLOW-UP DUP-CHECK FOR
C     THEM PRECISELY TO REMOVE LOCATION-TIME DUPLICATES WITH DIFFERENT
C     REPORT IDS), AND 2) FOR METARS ONLY, MODIFYING THE CORRECTION
C     INDICATOR TO TAKE INTO ACCOUNT BOTH WHETHER OR NOT THE REPORT IS
C     CORRECTED (BASED ON "CORN", AS BEFORE FOR ALL REPORTS) BUT ALSO
C     FOR METARS WHETHER OR NOT THE TYPE OF HOURLY REPORT IS "METAR"
C     VS. SOMETHING ELSE (MOST LIKELY "SPECI") (BASED ON THRPT) (SEE
C     CHANGE 2004-09-08 IN THE BUFR_DUPMAR HISTORY LOG AND ALSO
C     REMARKS-1 IN ITS DOCBLOCK FOR MORE INFORMATION)
C 2012-01-23  D. KEYSER   CALL MAXOUT TO INCREASE LENGTH OF OUTPUT BUFR
C     MESSAGES FROM 10K BYTES (DEFAULT) TO 25K BYTES FOR MESSAGE TYPES
C     NC031001 (BATHYTHERMAL), NC031002 (TESAC) AND NC031003 (TRACKOB)
C     - ALL ARE COMING IN WITH 20K BYTE MESSAGE LENGTHS AND MAY HAVE
C     SUBSETS > 10K BYTES (KNOWN TO HAPPEN FOR TESAC CASE IN JAN 2012)
C 2012-11-20  J. WOOLLEN  INITIAL PORT TO WCOSS -- ADAPTED IBM/AIX
C       GETENV SUBPROGRAM CALL TO INTEL/LINUX SYNTAX
C 2013-01-13  J. WHITING  PORT TO WCOSS - UPDATED DOC BLOCKS, 
C       REPLACED TESTS VS BMISS W/ IBFMS FUNCTION; REPLACED EXPLICIT
C       ASSIGNMENT OF BMISS W/ GETBMISS() FUNCTION.
C 2013-02-07  J. Whiting  Port to WCOSS - Updated DUPES variable
C       assignment statement w/ 8-btye integer intrinsic functions 
C       (KIDNNT) so as to properly handle large (global missing) values
C       declared BMISS, GETBMISS and CORN_8 as 8-byte reals.
C 2015-06-15  S. Melchior -
C      - No longer tosses duplicates for MDCRS types (004.004 or
C        004.007) which will later be part of the "aircar" dump.
C        Duplicate checking/tossing is now done in downstream "dupair"
C        code where height and obs time down to the second are
C        considered.  Prevents near-duplcate MDCRS reports from being
C        tossed as before.
C      - No longer tosses duplicates for Canadian AMDARs (004.009)
C        which will later be part of the "aircft" dump.  This corrects
C        an oversight since these are supposed to be duplicate checked/
C        tossed only in downstream "dupair" code, like most other
C        aircraft types.  Prevents near-duplcate Canadian AMDAR reports
C        from being tossed (although few actually were here).
C 2015-06-16  D. Keyser
C      - Additional changes related to above:
C         - If no time window trimming is being performed and aircraft
C           type does not need to be duplicate checked here, this
C           program now simply stops (with r.c. zero) because it has
C           nothing to do.  Eliminates wasted run time.
C         - If file is an aircraft type for which duplicate tossing is
C           not performed but time trimming is still needed, skips
C           logic which was still sorting reports and performing
C           duplicate checking.  This is not needed and wastes time.
C         - Will continue to perform duplicate checking/tossing for
C           MDCRS types (004.004 or 004.007) which will later be part
C           of the "aircar" dump if obs date is prior to January 1,
C           2010. These are NOT duplicate checked by downstream
C           "dupair" code.
C         - Updated report summary at end - e.g., no more references to
C           "duplcates not removed".
C 2015-07-28  S. Melchior Added ability to process (i.e., time-trim if
C     requested) new AIRCFT types containing Korean AMDAR (from BUFR)
C     in NC004011 and catch-all AMDAR (from BUFR) in NC004103 if they
C     are present.
C 2016-04-18  D. Keyser
C         - For AIRCAR types with obs date prior to January 1, 2010,
C           added logic to check if operator descriptors are used to
C           increase the scale factor for "CLAT" from the standard
C           value of 2 to something larger.  If so, will later round
C           CLAT (and CLON by assumption) values read in for each
C           subset to their scale factor 2 values.  This is done since
C           duplicate-check logic in this case expects CLAT and CLON to
C           have scale factor of 2.
C         - Failback on 2015-06-15 change 1 for now.  Will treat MDCRS
C           types (004.004 or 004.007) the same as before.  Will not
C           remove logic added in 2015-06-15, 2015-06-16 and 2015-07-28
C           changes above, but rather change date check such that logic
C           operates as though obs date is prior to January 1, 2010
C           regardless of obs date (done by setting control date to
C           1/1/3000 rather than 1/1/2010).  (This also invokes change
C           just above since V7 AIRCAR reports use operator descriptors
C           to change scale factor for CLAT and CLON from 2 to 5.)
C           Necessary because testing showed that near-dups here degrade
C           the GFS forcast at 5 days.  This needs to be examined
C           carefully.  GSI may need to be updated to include thinning
C           of aircraft reports.  More to come on this...
C         - For aircraft types only: add diagnostic print for pairs of
C           reports read into duplicate check and diagnostic print for
C           pairs in cases when duplicate check is satisfied (normally
C           commented out).  Pull out aircraft registration number
C           (ACRN) to use in this duplicate print.  Other diagnostic
C           print (also normally commented out) added.
C 2016-08-09  S. Melchior/D. Keyser
C         - Added ability to process TAMDAR BUFR data from both AirDAT
C           (historical reruns) and from Panasonic (current). These are
C           part of the "aircft" dump file and are treated like all
C           other types in this dump.
C         - Instead of using the bogus control date of 1/1/3000 to
C           "fool" the code into treating MDCRS types (004.004 or
C           004.007) the same as before 2015-06-15 change 1 (see
C           2016-04-18 change 2), will revert back to original/actual
C           date of 1/1/2010 to define the point when this code will no
C           longer toss duplicates for MDCRS types (004.004 or 004.007)
C           {and will check scale factor for CLAT/CLON (see 2016-04-18
C           change 1)}, but force this change to NOT occur after this
C           date IF the new script environment variable
C           KEEP_NEARDUP_ACFT is imported as "NO" (default for anything
C           other than "YES").
C           BENEFIT: Allows different networks to invoke or not invoke
C                    this change for MDCRS reports after 1/1/2010, and
C                    allows for future changes in setting of
C                    KEEP_NEARDUP_ACFT without any need to change this
C                    source code.  Also removes clumsy temporary date
C                    setting (1/1/3000) to retain old logic.
C 2020-08-20  J. DONG  --  ADDED SETBMISS CALL TO SET BMISS TO 10E8
C           TO AVOID INTEGER OVERFLOW
C 2021-09-02  D. STOKES -- COMMENTED SYSTEM CALLS OF POSTMSG TO JLOGFILE
C 2022-03-25  I. Genkova  Added check for 0 reports in input file and
C           allows for graceful continue in the event of 0 reports.
C     
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT - FIRST RECORD CONTAINS INPUT FILE
C                NAME, SECOND RECORD CONTAINS OUTPUT FILE NAME,
C                OPTIONAL THIRD RECORD CONTAINS TIME WINDOW TRIMMING
C                SPECIFICATIONS (THE YYYYMMDDHH<.HH> DATE OF THE
C                EARLIEST TIME TO DUMP AND THE YYYYMMDDHH<.HH> DATE OF
C                THE LATEST TIME TO DUMP), OPTIONAL FOURTH RECORD
C                CONTAINS DUP-CHECKING TOLERANCE LIMITS (IF FOURTH
C                RECORD IS MISSING, DEFAULT DUP-CHECKING TOLERANCE
C                LIMITS ARE USED, IF THIRD RECORD IS ALSO MISSING, NO
C                TIME WINDOW TRIMMING IS PERFORMED)
C     UNIT 20  - UNCHECKED, UNCORRECTED AND UNWINDOWED BUFR DUMP FILE
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 50  - DUPLICATE CHECKED (FOR MOST TYPES), CORRECTED (FOR
C                CONVENTIONAL TYPES) AND (OPTIONAL) TIME WINDOW TRIMMED
C                BUFR DUMP FILE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - REMTDY
C     SYSTEM     - GETENV   SYSTEM
C     LIBRARY:
C       W3NCO    - W3TAGB   W3TAGE  ERREXIT W3FS26  IW3JDN
C       W3EMC    - ORDERS
C       BUFRLIB  - DATELEN  OPENBF  COPYMG  UFBTAB   OPENMB  COPYSB
C                  READMG   IREADMG CLOSMG  CLOSBF   MESGBC  MAXOUT
C                  IBFMS    COPYBF  UFBINT  GETBMISS NEMSPECS
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
      PROGRAM BUFR_DUPCOR
 
      PARAMETER (MXTS=8)

      REAL(8),ALLOCATABLE :: TAB_8(:,:)
      REAL(8),ALLOCATABLE :: RAB_8(:,:)
      REAL(8),ALLOCATABLE :: THRPT_8(:)
      REAL(8),ALLOCATABLE :: ACRN_8(:)
      REAL(8),ALLOCATABLE :: DOYR_8(:,:)
      INTEGER,ALLOCATABLE :: IWORK(:)
      INTEGER,ALLOCATABLE :: IORD(:)
      INTEGER,ALLOCATABLE :: JDUP(:)

      REAL(8)       BMISS, GETBMISS, CORN_8

      CHARACTER*500 FILI,FILO
      CHARACTER*80  TSTR,TSTRH,RSTR
      CHARACTER*8   SUBSET,CAB8_IREC,CAB8_JREC,CACRN8_IREC,CACRN8_JREC
      CHARACTER*6   LATLON_TYPE
      CHARACTER*3   DUMMY_MSGS,KEEP_NEARDUP_ACFT
      CHARACTER*2   CITIMESm1

      DIMENSION     NDUP(0:5)

      REAL(8)       ADATE,BDATE,CDATE,DDATE,RDATE,UFBTAB_8
      REAL(8)       TAB8_IREC_8,TAB8_JREC_8,rdate_8(3)
      REAL(8)       ACRN_IREC_8,ACRN_JREC_8

      LOGICAL       DUPES,AIRCFT,METAR

      EQUIVALENCE   (TAB8_IREC_8,CAB8_IREC),(TAB8_JREC_8,CAB8_JREC)
      EQUIVALENCE   (ACRN_IREC_8,CACRN8_IREC),(ACRN_JREC_8,CACRN8_JREC)

      DATA TSTR  /'CLAT  CLON  MNTH DAYS HOUR MINU CORN RPID '/
      DATA TSTRH /'CLATH CLONH MNTH DAYS HOUR MINU CORN RPID '/
      DATA RSTR  /'RCYR  RCMO  RCDY RCHR RCMI                '/

      DATA ADATE /00000000.00_8/
      DATA BDATE /99999999.00_8/
      DATA DEXY  /0/
      DATA DMON  /0/
      DATA DDAY  /0/
      DATA DOUR  /0/
      DATA DMIN  /0/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_DUPCOR',2022,0084,1100,'NP22') 

      print *
      print * ,'---> Welcome to BUFR_DUPCOR - Version 03-25-2022'
      print *

      CALL DATELEN(10)

ccccc CALL OPENBF(0,'QUIET',2) ! Uncomment for extra print from bufrlib

C  Override current BUFRLIB maximum number of data values in an
C   uncompressed BUFR subset (80000) (due to hi-vert res raobs)
C -------------------------------------------------------------
      IRET=ISETPRM('MAXSS',300000 )  ! must use DA version of BUFRLIB
      IF(IRET.EQ.0)  THEN
         IMAXSS=IGETPRM('MAXSS')
         PRINT'(/" MAXIMUM NUMBER OF DATA VALUES IN AN UNCOMPRESSED",
     $    " BUFR SUBSET (MAXSS) SET TO ",I0)', IMAXSS
      ELSE
         PRINT'(/" ATTEMPT TO SET MAXSS IN DUPCOR FAILED")'
c        PRINT'(/25("*"),"ABORT",25("*")/"ATTEMPT TO SET MAXSS FAILED ",
c    $    " -- STOP 94"/25("*"),"ABORT",25("*")/)'
c        CALL CW3TAGE('BUFR_DUPCOR')
c        call errexit(94)
      ENDIF
C  Above block copied and adapted from EDTBFR.F   CH 03/15/2022

C  ASSIGN DEFAULT VALUE FOR 'MISSING' TO LOCAL BMISS VARIABLE
C  ----------------------------------------------------------

      CALL SETBMISS(10E8_8)
      BMISS = GETBMISS()     ! assign default value for "missing"

C  SET THE COUNTERS TO INITIAL VALUES
C  ----------------------------------

      ISUB  = 0
      NDUP  = 0
      LUBFI = 20
      LUBFJ = 50

C  READ I/O FILENAMES AND ANY OVERRIDE VALUES FOR THINNING PARAMETERS
C  ------------------------------------------------------------------
C     DEFAULT PARAMETERS:
C     ADATE = 00000000.00  LOWER LIMIT (YYYYMMDDHH.hh) FOR TIME WINDOW
C                          TRIMMING
C     BDATE = 99999999.00  UPPER LIMIT (YYYYMMDDHH.hh) FOR TIME WINDOW
C                          TRIMMING
C                          (I.E., NO TIME WINDOW TRIMMING IS PERFORMED)
C     DEXY  = 0.0  TOLERANCE FOR LAT/LON CHECKS
C     DOUR  = 0.0  TOLERANCE FOR HOUR CHECK
C     DMIN  = 0.0  TOLERANCE FOR MINUTE CHECK
C
C     HARDWIRED PARAMETERS:
C     DMON  = 0.0  TOLERANCE FOR MONTH CHECK
C     DDAY  = 0.0  TOLERANCE FOR DAY CHECK
C  ------------------------------------------------------------------
 
      READ(5,'(Q,A)',END=900,ERR=900) NBYTES_FILI,FILI(1:NBYTES_FILI)
      READ(5,'(Q,A)',END=900,ERR=900) NBYTES_FILO,FILO(1:NBYTES_FILO)

      KEEP_NEARDUP_ACFT = 'NO'

cppppp
ccc   print *, 'file fili is ',nbytes_fili,' bytes long'
ccc   print *, 'file filo is ',nbytes_filo,' bytes long'
cppppp

C  OPEN FILE TEMPORARILY TO CHECK FOR COMPRESSION AND TO SEE WHAT THE
C   BUFR MESSAGE TYPE IS (SUBSET), ALSO CHECKS DATE FOR 1ST OBS FOUND
C   FOR MOST AIRCRAFT TYPES (DETERMINES IF DUP-CHECKING IS PERFORMED
C   FOR THESE TYPES)
C  ------------------------------------------------------------------

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
         PRINT'(/"INPUT BUFR FILE DOES NOT EXIST - STOP"/)'
         CALL W3TAGE('BUFR_DUPCOR')
         CALL ERREXIT(00)
      ELSE  IF(ICOMP.EQ.-2)  THEN
         PRINT'(/"INPUT BUFR FILE HAS NO DATA MESSAGES"/"FIRST '//
     .    'MESSAGE TYPE FOUND IS",I5/)', MSGT
      ENDIF

      CALL OPENBF(LUBFI,'IN',LUBFI)
      aircft = .false.
      iflg_acars_latlon_sf = 0
      if(ireadmg(lubfi,subset,idate).ne.0) then
         print *, '===> BUFR_DUPCOR - NO DATA IN INPUT FILE'
         print *, '===> THIS PROGRAM IS DONE - STOP'
         print *
         call closbf(lubfi)
         open(lubfi,file=fili(1:nbytes_fili),form='UNFORMATTED')
         open(lubfj,file=filo(1:nbytes_filo),form='UNFORMATTED')
         call copybf(lubfi,lubfj)
         call closbf(lubfi)
         call closbf(lubfj)
         call w3tage('BUFR_DUPCOR')
         call errexit(00)
      else
         READ(SUBSET,'(2X,2I3)') MTP,MST

C  For all types of aircraft rpts with obs time after January 01, 2010
C  -AND- when script environment variable KEEP_NEARDUP_ACFT is imported
C  as "YES":
C    AIRCFT types NC004001, NC004002, NC004003, NC004006, NC004009,
C    NC004010, NC004011 and NC004103 as well as AIRCAR types NC004004
C    and NC004007 are NOT duplicate checked here (that will be done
C    later in bufr_dupair), however, they will be time trimmed if that
C    option is invoked.  Logical AIRCFT is TRUE for all of the above
C    types. {Note that TAMDAR from MADIS are dupl. checked here while
C    TAMDAR from Panasonic or AirDAT are not dupl. checked here).
C  For all types of aircraft rpts with obs time before January 01, 2010
C  -OR- when script environment variable KEEP_NEARDUP_ACFT is imported
C  as "NO" (default for anything other than "YES"):
C    AIRCFT types NC004001, NC004002, NC004003, NC004006, NC004009 and
C     NC004010 are NOT duplicate checked here (that will be done later
C     in bufr_dupair), however, they will be time trimmed if that option
C     is invoked.  (AIRCFT types NC004011 and NC004103 were not present
C     then, but if they had been present then they too would not have
C     been duplicate checked here, note that TAMDAR from MADIS are dupl.
C     checked here while TAMDAR from Panasonic or AirDAT are not dupl.
C     checked here.)
C     Logical AIRCFT remains TRUE for all of the above types.
C    AIRCAR types NC004004 and NC004007 ARE duplicate checked here (they
C     are not considered by bufr_dupair), and they are time trimmed if
C     that option is invoked. Logical AIRCFT is now FALSE for these two
C     types.
C      Check to see if these AIRCAR types use operator descriptors to
C      increase the scale factor for "CLAT" from the standard value of 2
C      to something larger.  If so (e.g., NC004004 after move to version
C      7 BUFR), will later round CLAT (and CLON by assumption) values
C      read in for each subset to their scale factor 2 values prior to
C      calling ORDERS and then duplicate-checking.  This is done so that
C      dup-check of version 7 BUFR rpts (with scale factor 5 in lat/lon,
C      although only storing to .001 precision) will toss ~same number
C      of duplicates as before when scale factor was 2.  (Otherwise many
C      more near-duplicates would be retained.)  This is needed for
C      cases when KEEP_NEARDUP_ACFT = NO (or anything other then "YES")
C      because this can only occur for version 7 BUFR (which did not
C      appear until the fall of 2016).
C  ---------------------------------------------------------------------

         AIRCFT = MTP.EQ.4.AND.(MST.LE.4.OR.MST.EQ.6.or.mst.eq.7.or.
     .                          mst.eq.9.or.mst.eq.10.or.mst.eq.11.or.
     .                          mst.eq.103)
         if(aircft) then
            CALL GETENV('KEEP_NEARDUP_ACFT',KEEP_NEARDUP_ACFT)
            IF(KEEP_NEARDUP_ACFT.NE.'YES') KEEP_NEARDUP_ACFT = 'NO'
            if(ireadsb(lubfi).eq.0) then
               call ufbint(lubfi,rdate_8,3,1,nlv,'YEAR MNTH DAYS')
               jndcntrl = iw3jdn(2010,1,1)
               jnddata =
     .          iw3jdn(int(rdate_8(1)),int(rdate_8(2)),int(rdate_8(3)))
               if(jnddata.lt.jndcntrl.or.KEEP_NEARDUP_ACFT.eq.'NO') then
                  aircft = mtp.eq.4.and.(mst.le.3.or.mst.eq.6.OR.mst.eq.
     .             9.or.mst.eq.10.or.mst.eq.11.or.mst.eq.103)
                  if(.not.aircft) then
                     call NEMSPECS(lubfi,'CLAT',1,NSCL,NREF,NBTS,IRET)
                     if(iret.eq.0) then
cpppppppppp
                        print *
                        print *, '---> For CLAT, NSCL = ',NSCL
                        print *
cpppppppppp
                        iflg_acars_latlon_sf = NSCL
                     else
                        print *
                        print *,'##WARNING: NSCL for CLAT could not be',
     .                          ' returned by BUFRLIB routine',
     .                          'NEMSPECS, NSCL remains 2.'
                        print *
                     endif
                  endif
               endif
            endif
cpppppppppp
            print *
            print *, '---> KEEP_NEARDUP_ACFT = ',KEEP_NEARDUP_ACFT
            print *
cpppppppppp
         endif
      ENDIF

      CALL CLOSBF(LUBFI)

      READ(5,*,END=1) ADATE,BDATE
      READ(5,*,END=1) DEXY,DOUR,DMIN

    1 CONTINUE

      IF(BDATE.NE.99999999.00_8) THEN
         PRINT 200, ADATE,BDATE
      ELSE
         if(.not.aircft) PRINT 201
      ENDIF
  200 FORMAT(/'REQUESTED EARLIEST DATE IS ....... ',F15.2/
     .        'REQUESTED LATEST   DATE IS ....... ',F15.2)
  201 FORMAT(/'@@@@ AS REQUESTED, NO TIME WINDOW TRIMMING IS PERFORMED'/
     .        '@@@@ ALL NON-DUPLICATES ARE RETAINED REGARDLESS OF TIME')
      if(aircft) then
         print'(/"--> This type is not dup-checked here -- it will '//
     .    'be dup-checked later by BUFR_DUPAIR"//
     .    "INPUT FILE IS         "/5x,a/"OUTPUT FILE IS"/5x,a/)',
     .    fili(1:nbytes_fili),filo(1:nbytes_filo)
         if(bdate.eq.99999999.00_8) then
            print 203
  203 format(/'@@@@ AS REQUESTED, NO TIME WINDOW TRIMMING IS PERFORMED'/
     .        '@@@@ THIS PROGRAM IS DONE - STOP')
            open(lubfi,file=fili(1:nbytes_fili),form='UNFORMATTED')
            open(lubfj,file=filo(1:nbytes_filo),form='UNFORMATTED')
            call copybf(lubfi,lubfj)
            call closbf(lubfi)
            call closbf(lubfj)
            call w3tage('BUFR_DUPCOR')
            call errexit(00)
         endif
      else
         PRINT 202, FILI(1:NBYTES_FILI),FILO(1:NBYTES_FILO),DEXY,DMON,
     .    DDAY,DOUR,DMIN
  202 FORMAT(/'UNCHECKED AND UNCORRECTED INPUT FILE IS         '/5X,A/
     .        'DUPLICATE CHECKED AND CORRECTED OUTPUT FILE IS'/5X,A//
     .        'BUFR_DUPCOR PARAMETERS:'/
     .        3X,'TOLERANCE FOR LAT/LON CHECKS (IN DEGREES) .. ',F5.1/
     .        3X,'TOLERANCE FOR YEAR CHECK (** NOT CHECKED **) '/
     .        3X,'TOLERANCE FOR MONTH CHECK (IN MONTHS) ...... ',F5.1/
     .        3X,'TOLERANCE FOR DAY CHECK (IN DAYS) .......... ',F5.1/
     .        3X,'TOLERANCE FOR HOUR CHECK (IN HOURS) ........ ',F5.1/
     .        3X,'TOLERANCE FOR MINUTE CHECK (IN MINUTES) .... ',F5.1/
     .        3X,'TOLERANCE FOR SECOND CHECK (** NOT CHECKED **) ')
      endif

C  COUNT THE NUMBER OF SUBSETS IN THE FILE TO ALLOCATE SPACE
C  ---------------------------------------------------------

      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      CALL OPENBF(0,'QUIET',1) ! will generate diagnostic print if an
                               ! embedded BUFR table is read
      CALL UFBTAB(-LUBFI,UFBTAB_8,1,1,MXTB,' ')
      CALL OPENBF(0,'QUIET',0) ! return to default wrt degree of print

      IF(MXTB.EQ.0) THEN
         PRINT *
         PRINT *, '### WARNING: A total of ZERO input reports //dupcor'
         PRINT *
         GO TO 400
      ENDIF

      ALLOCATE(TAB_8(MXTS,MXTB),STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(RAB_8(MXTS,MXTB),STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(THRPT_8(MXTB),STAT=I);   IF(I.NE.0) GOTO 901
      ALLOCATE(ACRN_8(MXTB),STAT=I);    IF(I.NE.0) GOTO 901
      ALLOCATE(DOYR_8(2,MXTB)  ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(IWORK(MXTB)     ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(IORD(MXTB)      ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(JDUP(MXTB)      ,STAT=I);IF(I.NE.0) GOTO 901

      TAB_8  = BMISS
      THRPT_8 = BMISS
      ACRN_8 = BMISS
      RAB_8  = BMISS
      DOYR_8 = BMISS
      JDUP   = 0
      IORD   = 0

      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')

C  MAKE A TABLE OUT OF THE LATS, LONS, OBS TIME COORDINATES AND
C   RECEIPT TIME COORDINATES
C  ------------------------------------------------------------

C  CHECK TABLE A ENTRY SINCE HI-RES ALTIMETER DATA AND AIRCRAFT TYPES
C   E-ADAS, CANADIAN AMDAR AND TAMDAR (all types) ALL HAVE HIGH
C   ACCURACY LAT/LON
C  ------------------------------------------------------------------
 
      LATLON_TYPE='NONE  '

      DO ITIMES=1,MXTB ! Look thru up to 25 rpts to find a valid lat
         IF(ITIMES.EQ.26.OR.ITIMES.EQ.MXTB) THEN
            IF(ITIMES.EQ.26) THEN
               PRINT 1858
 1858 FORMAT(/'##WARNING: THE FIRST 25 REPORTS IN INPUT FILE HAVE A ',
     $ 'MISSING LATITUDE, ALL REPORTS MAY HAVE MISSING LAT/LON'/)
C              CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg'//
C    $          ' "$jlogfile" "***WARNING: THE FIRST 25 REPORTS IN '//
C    $          'INPUT FILE HAVE A MISSING LAT, ALL REPORTS MAY HAVE '//
C    $          'MISSING LAT/LON, TYPE="'//SUBSET)
            ELSE
               PRINT 1859
 1859 FORMAT(/'##WARNING: ALL REPORTS IN INPUT FILE HAVE MISSING LAT/',
     $ 'LON'/)
C              CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg'//
C    $          ' "$jlogfile" "***WARNING: ALL REPORTS IN INPUT FILE '//
C    $          'HAVE MISSING LAT/LON, TYPE="'//SUBSET)
            ENDIF
            EXIT
         ENDIF
         CALL UFBTAB(LUBFI,TAB_8,MXTS,MXTB,NTAB,TSTR)
         LATLON_TYPE='LO-RES'
         SCALE = 100.
         IF(IBFMS(TAB_8(1,ITIMES)).EQ.1) THEN         ! data missing
             ! low-res lat missing for this report, try high-res lat
cpppppppppp
ccc         print *, 'For LATLON_TYPE = ',LATLON_TYPE,', TAB_8(1,
ccc  $       ',ITIMES,') missing - try next LATLON_TYPE'
cpppppppppp

            OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
            CALL UFBTAB(LUBFI,TAB_8,MXTS,MXTB,NTAB,TSTRH)
            LATLON_TYPE='HI-RES'
            SCALE = 10000.
            IF(IBFMS(TAB_8(1,ITIMES)).EQ.1) THEN    ! data missing
             ! both low- & high-res lat missing for this rpt,
             !  try high-res lat for next rpt
cpppppppppp
ccc            print *, 'For LATLON_TYPE = ',LATLON_TYPE,', TAB_8(1,',
ccc  $          ITIMES,') missing - try previous ','LATLON_TYPE again'
cpppppppppp
               OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
               CYCLE
            ENDIF
             ! high-res lat valid for this rpt, this is lat/lon type
            IF(ITIMES.GT.1) THEN
               PRINT 1860, ITIMES-1
 1860 FORMAT(/'##WARNING: THE FIRST ',I2,' REPORT(S) IN INPUT FILE ',
     $ 'HAVE A MISSING LATITUDE (HIGH-RESOLUTION LAT/LON TYPE)'/)
               WRITE(CITIMESm1,'(I2)') ITIMES-1
C              CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg'//
C    $          ' "$jlogfile" "***WARNING: THE FIRST '//CITIMESm1//
C    $          ' REPORT(S) IN INPUT FILE HAVE A MISSING LATITUDE'//
C    $          ' (HIGH-RESOLUTION LAT/LON TYPE), TYPE="'//SUBSET)
            ENDIF
cpppppppppp
ccc         print *, 'For LATLON_TYPE = ',LATLON_TYPE,', TAB_8(1,',
ccc  $       ITIMES,') valid - move on with this LATLON_TYPE'
cpppppppppp
            EXIT
         ELSE
            ! low-res lat valid for this rpt, this is lat/lon type
            IF(ITIMES.GT.1) THEN
               PRINT 1861, ITIMES-1
 1861 FORMAT(/'##WARNING: THE FIRST ',I2,' REPORT(S) IN INPUT FILE ',
     $ 'HAVE A MISSING LATITUDE (LOW-RESOLUTION LAT/LON TYPE)'/)
               WRITE(CITIMESm1,'(I2)') ITIMES-1
C              CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg'//
C    $          ' "$jlogfile" "***WARNING: THE FIRST '//CITIMESm1//
C    $          ' REPORT(S) IN INPUT FILE HAVE A MISSING LATITUDE'//
C    $          ' (LOW-RESOLUTION LAT/LON TYPE), TYPE="'//SUBSET)
            ENDIF
cpppppppppp
ccc         print *, 'For LATLON_TYPE = ',LATLON_TYPE,', TAB_8(1,',
ccc  $       ITIMES,') valid - move on with this LATLON_TYPE'
cpppppppppp
            EXIT
         ENDIF
      ENDDO
cpppppppppp
ccc   print *, 'iflg_acars_latlon_sf,SCALE = ',
ccc  $ iflg_acars_latlon_sf,SCALE
cpppppppppp
      METAR = .FALSE.
      IF(SUBSET.EQ.'NC000007')  THEN

C  METAR reports can be dup-checked in this code as a follow-up to
C   their primary dup-check code BUFR_DUPMAR - in such cases the type
C   of hourly report ("THRPT") must be obtained for all reports as it
C   is used in the dup-checking process
C  ------------------------------------------------------------------

         METAR = .TRUE.
         OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
         CALL UFBTAB(LUBFI,THRPT_8,1,MXTB,NTAB,'THRPT')
      ELSE IF(SUBSET(1:5).NE.'NC001') THEN

C Only METAR and surface marine reports (which can sometimes be follow-
C  up dup-checked by this code) consider report id in the dup-checking
C  process so set it missing for all other type of reports
C ---------------------------------------------------------------------

         TAB_8(8,:) = BMISS
         IF(SUBSET(1:5).EQ.'NC004') THEN
cpppppppppp

C For aircraft reports (any type), pull out tail number ("ACRN") for
C  possible use in diagnostic print
C ------------------------------------------------------------------

ccc         OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
ccc         CALL UFBTAB(LUBFI,ACRN_8,1,MXTB,NTAB,'ACRN')
cpppppppppp

            if(.not.aircft) then

C For reports from AIRCAR aircraft types NC004004 and NC004007 which are
C  duplicate checked here {since either obs time is before January 01,
C  2010 or KEEP_NEARDUP_ACFT = NO (or anything other than "YES")}, if
C  scale factor on CLAT (and assumed also CLON) is > 2, round CLAT and
C  CLON values read in for each subset to their scale factor 2 values
C  prior to calling ORDERS and then duplicate-checking.  The reasoning
C  for this was explained earlier.
C ----------------------------------------------------------------------

               if(iflg_acars_latlon_sf.gt.2) then
                  do i = 1,ntab
                     tab_8(1,i) = kidnnt(tab_8(1,i)*100)/100._8 ! add "_8"
                     tab_8(2,i) = kidnnt(tab_8(2,i)*100)/100._8 ! add "_8"
cpppppppppp
ccc   if(i.eq.1)
ccc  . print *, 'tab_8(1,1),tab_8(2,1): ',tab_8(1,i),tab_8(2,i)
cpppppppppp
                  enddo
               endif
            endif
         ENDIF
      ENDIF
      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      CALL UFBTAB(LUBFI,RAB_8,MXTS,MXTB,NTAB,RSTR)

C  MODIFY THE VALUE OF THE CORRECTION INDICATOR BASED ON THE VALUE
C   READ IN FOR "CORN" (CORRECTION INDICATOR) IN COMBINATION, FOR
C   METAR REPORTS (ONLY), WITH THE VALUE READ IN FOR "THRPT" (TYPE OF
C   HOURLY REPORT) (SEE REMARKS-1 IN BUFR_DUPMAR FOR MORE DETAILS);
C   SET MISSING MINU TO ZERO; IF DAY-OF-YEAR ENCODED IN PLACE OF
C   MONTH AND DAY, CONVERT IT TO MONTH AND DAY
C  ------------------------------------------------------------------

      DO N=1,NTAB
         CORN_8 = TAB_8(7,N)
         IF(IBFMS(CORN_8).EQ.1) THEN                         ! data missing
            CORN_8 = 0 ! Zero values for missing CORN_8
         ELSE IF(CORN_8.GE.1) THEN
            CORN_8 = CORN_8 + 1  ! Bump up non-zero CORN_8 by 1
         ENDIF
         IF(CORN_8.EQ.0) THEN
            IF(THRPT_8(N).EQ.0)  CORN_8 = 1 ! Set TYPE="METAR" to 1
         ENDIF
         IF(CORN_8.GT.0) JDUP(N) = 1
         TAB_8(7,N) = CORN_8
         IF(IBFMS(TAB_8(6,N)).EQ.1) TAB_8(6,N) = 0      ! TAB_8(6,N) missing
         IF(TAB_8(3,N).GT.12) THEN
            IF(TAB_8(4,N).GT.31) THEN
               IF(N.EQ.1) THEN
                 OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
                  CALL UFBTAB(LUBFI,DOYR_8,2,MXTB,NTAB,'YEAR DOYR')
                  IF(IBFMS(DOYR_8(2,N)).EQ.0) PRINT 8860   ! DOYR_8(2,N) not missing
 8860 FORMAT(/'##WARNING: REPORTS IN INPUT FILE STORE DAY-OF-YEAR IN ',
     $ 'PLACE OF MONTH AND YEAR, CONVERT FORMER TO LATTER'/11X,'FOR ',
     $ 'DUP-CHECKING AND POSSIBLE TIME-TRIMMING'/)
               ENDIF
               IY    = NINT(DOYR_8(1,N))
               IDOYR = NINT(DOYR_8(2,N))
               IF((IY.GT.1583.AND.IY.LT.3300).AND.IDOYR.LT.367) THEN
                  CALL REMTDY(IY,IDOYR,IM,ID)
                  TAB_8(3,N) = IM
                  TAB_8(4,N) = ID
cppppp
ccc               write(6,'(" CONVERTED DOYR (",I3,") TO MNTH (",
ccc  .             I2.2,") AND DAYS (",I2.2,")")') NINT(DOYR_8(2,N)),
ccc  .             NINT(TAB_8(3,N)),NINT(TAB_8(4,N))
cppppp
               ENDIF
            ENDIF
         ENDIF
      ENDDO
 
      if(.not.aircft) then

C  GET A SORTED INDEX OF THE REPORTS KEYED IN THIS ORDER: LAT, LON, OBS
C   TIME, CORRECTION INDICATOR (%), REPORT ID (*), RECEIPT TIME
C  --------------------------------------------------------------------
C    * - report id is missing for all subsets except for type METAR or
C        surface land
C    % - correction takes into account type of hourly report for METARs
 
      CALL ORDERS( 2,IWORK,RAB_8(5,1),IORD,NTAB,MXTS,8,2) ! rcpt minute
      CALL ORDERS(12,IWORK,RAB_8(4,1),IORD,NTAB,MXTS,8,2) ! rcpt hour
      CALL ORDERS(12,IWORK,RAB_8(3,1),IORD,NTAB,MXTS,8,2) ! rcpt day
      CALL ORDERS(12,IWORK,RAB_8(2,1),IORD,NTAB,MXTS,8,2) ! rcpt month
      CALL ORDERS(12,IWORK,RAB_8(1,1),IORD,NTAB,MXTS,8,2) ! rcpt year
      CALL ORDERS(10,IWORK,TAB_8(8,1),IORD,NTAB,MXTS,8,2) ! report id-*
      CALL ORDERS(12,IWORK,TAB_8(7,1),IORD,NTAB,MXTS,8,2) ! correction-%
      CALL ORDERS(12,IWORK,TAB_8(6,1),IORD,NTAB,MXTS,8,2) ! obs minute
      CALL ORDERS(12,IWORK,TAB_8(5,1),IORD,NTAB,MXTS,8,2) ! obs hour
      CALL ORDERS(12,IWORK,TAB_8(4,1),IORD,NTAB,MXTS,8,2) ! obs day
      CALL ORDERS(12,IWORK,TAB_8(3,1),IORD,NTAB,MXTS,8,2) ! obs month
      CALL ORDERS(12,IWORK,TAB_8(2,1),IORD,NTAB,MXTS,8,2) ! longitude
      CALL ORDERS(12,IWORK,TAB_8(1,1),IORD,NTAB,MXTS,8,2) ! latitude

C  GO THROUGH THE REPORTS IN ORDER, MARKING DUPLICATES AND CORRECTIONS
C  -------------------------------------------------------------------
 
cpppppppppppppppppppppppppppppppppp
ccc   IF(SUBSET.EQ.'NC004004') THEN
ccc   DO K=1,NTAB
ccc      I = IORD(K)
ccc      print'("I =",I9,", LAT = ",F10.6,", LON = ",F11.6,", MON = ",
ccc  $    F9.6,", DAY = ",F9.6,", HOUR = ",F9.6,", MIN = ",F9.6)',
ccc  $ I,TAB_8(1,I),TAB_8(2,I),TAB_8(3,I),TAB_8(4,I),TAB_8(5,I),
ccc  $ TAB_8(6,I)
ccc   END DO
ccc   ENDIF
cpppppppppppppppppppppppppppppppppp

      DO K=1,NTAB-1
         IREC = IORD(K)
         JREC = IORD(K+1)
cpppppppppp
ccc      IF(SUBSET(1:5).EQ.'NC004') THEN
ccc         ACRN_IREC_8 = ACRN_8(IREC)
ccc         ACRN_JREC_8 = ACRN_8(JREC)
ccc      ELSE
cpppppppppp
            TAB8_IREC_8 = TAB_8(8,IREC)
            TAB8_JREC_8 = TAB_8(8,JREC)
cpppppppppp
ccc      ENDIF
cpppppppppp

c Need to use the KIDNNT() intrinsic function here, with 8byte integer
c output, in order to deal w/ the case when potentially large (ie,
c greater than 10e7) "missing" values are encountered.

cpppppppppp
c  if this is uncommented, must uncomment references to ACRN above!
ccc      IF(SUBSET(1:5).EQ.'NC004') THEN
ccc      print 1789, irec,CACRN8_IREC,(tab_8(ii,irec),ii=1,2),
ccc  .    (nint(tab_8(ii,irec)),ii=4,6),(nint(rab_8(ii,irec)),ii=1,5)
c1789    format('----------'/'1st of 2  --> IREC: ',I6,
ccc  .    ';{ ID: ',A8,'; LAT: ',F6.2,'; LON: ',F7.2,
ccc  .    '; RPRT DD HH MM ',3I2.2,'; RCPT YYYYMMDDHHMM: ',I4,4I2.2,
ccc  .    '}')
ccc      print 1790, jrec,CACRN8_JREC,(tab_8(ii,jrec),ii=1,2),
ccc  .    (nint(tab_8(ii,jrec)),ii=4,6),(nint(rab_8(ii,jrec)),ii=1,5)
c1790    format('2nd of 2  --> JREC: ',I6,';{ ID: ',A8,'; LAT: ',
ccc  .    F6.2,'; LON: ',F7.2,'; RPRT DD HH MM ',3I2.2,
ccc  .    '; RCPT YYYYMMDDHHMM: ',I4,4I2.2,'}')
ccc      ENDIF
cpppppppppp
         DUPES = KIDNNT(DABS(TAB_8(1,IREC)-TAB_8(1,JREC))*SCALE) 
     .      .LE.NINT(DEXY*SCALE)
     .     .AND. KIDNNT(DABS(TAB_8(2,IREC)-TAB_8(2,JREC))*SCALE) 
     .      .LE.NINT(DEXY*SCALE)
     .     .AND. KIDNNT(DABS(TAB_8(3,IREC)-TAB_8(3,JREC))*100.) 
     .      .LE.NINT(DMON*100.)
     .     .AND. KIDNNT(DABS(TAB_8(4,IREC)-TAB_8(4,JREC))*100.) 
     .      .LE.NINT(DDAY*100.)
     .     .AND. KIDNNT(DABS(TAB_8(5,IREC)-TAB_8(5,JREC))*100.) 
     .      .LE.NINT(DOUR*100.)
     .     .AND. KIDNNT(DABS(TAB_8(6,IREC)-TAB_8(6,JREC))*100.) 
     .      .LE.NINT(DMIN*100.)
     .     .AND.
     .      CAB8_IREC.EQ.CAB8_JREC
         IF(DUPES) then
            JDUP(IREC) = 2
cpppppppppp
c  if this is uncommented, must uncomment references to ACRN above!
ccc         IF(SUBSET(1:5).EQ.'NC004') THEN
ccc         print 1799, irec,CACRN8_IREC,(tab_8(ii,irec),ii=1,2),
ccc  .       (nint(tab_8(ii,irec)),ii=4,6),(nint(rab_8(ii,irec)),ii=1,5)
c1799       format('===> DUPL. FOUND:'/'TOSSED:   --> IREC: ',I6,
ccc  .       ';{ ID: ',A8,'; LAT: ',F6.2,'; LON: ',F7.2,
ccc  .       '; RPRT DD HH MM ',3I2.2,'; RCPT YYYYMMDDHHMM: ',I4,4I2.2,
ccc  .       '}')
ccc         print 1800, jrec,CACRN8_JREC,(tab_8(ii,jrec),ii=1,2),
ccc  .       (nint(tab_8(ii,jrec)),ii=4,6),(nint(rab_8(ii,jrec)),ii=1,5)
c1800       format('KEPT:     --> JREC: ',I6,';{ ID: ',A8,'; LAT: ',
ccc  .       F6.2,'; LON: ',F7.2,'; RPRT DD HH MM ',3I2.2,
ccc  .       '; RCPT YYYYMMDDHHMM: ',I4,4I2.2,'}')
ccc         ENDIF
cpppppppppp
         endif
      ENDDO
      endif ! .not.aircft
 
C  TRIM THE EXCESS DATA FROM THE EXACT TIME WINDOW (IF REQUESTED)
C  --------------------------------------------------------------
 
      IF(BDATE.NE.99999999.00_8) THEN
         CDATE = MOD(ADATE,1000000._8)
         DDATE = MOD(BDATE,1000000._8)
         DO K=1,NTAB
            RDATE =
     .     TAB_8(3,K)*1E4 + TAB_8(4,K)*1E2 + TAB_8(5,K) + TAB_8(6,K)/60.
            IF(CDATE.LE.DDATE) THEN
               IF(RDATE.LT.CDATE .OR.  RDATE.GT.DDATE) JDUP(K) = 4
            ELSE
               IF(RDATE.LT.CDATE .AND. RDATE.GT.DDATE) JDUP(K) = 4
            ENDIF
         ENDDO
      ENDIF
 
C  WRITE A DUP-CHECKED, CORRECTED AND TIME-WINDOWED FILE
C  -----------------------------------------------------
 
      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      OPEN(LUBFJ,FILE=FILO(1:NBYTES_FILO),FORM='UNFORMATTED')
      CALL OPENBF(LUBFI,'IN ',LUBFI)
      CALL OPENBF(LUBFJ,'OUT',LUBFI)
      IF(SUBSET.EQ.'NC031001'.OR.SUBSET.EQ.'NC031002'.OR.
     $   SUBSET.EQ.'NC031003') CALL MAXOUT(25000)

      CALL GETENV('DUMMY_MSGS',DUMMY_MSGS)

C  If input file doesn't contain dummy center and dump time messages 1
C   and 2 (after table messages), before doing anything call CLOSMG
C   with a negative unit number to signal routine that it should not
C   write out ANY messages with zero subsets in them - this holds for
C   all subsequent calls to CLOSMG in this routine, even those done
C   through other bufrlib routines (and even for those calls where the
C   sign of the unit number is positive)
C  --------------------------------------------------------------------

      IF(DUMMY_MSGS.NE.'YES') CALL CLOSMG(-LUBFJ)
 
      DO WHILE(IREADMG(LUBFI,SUBSET,IDATE).EQ.0)
         NSUBS = NMSUB(LUBFI)

C  If no subsets in msg & dummy msgs not expected, loop to nxt input msg
C  ---------------------------------------------------------------------

         IF(NSUBS.LE.0.AND.DUMMY_MSGS.NE.'YES')  CYCLE

         DUPES = .FALSE.
         IF(NSUBS.GT.0) THEN
            DO N=1,NSUBS
               IDUP = JDUP(ISUB+N)
               IF(IDUP.GT.1) DUPES = .TRUE.
            ENDDO
         ENDIF
         IF(DUPES) THEN
            CALL OPENMB(LUBFJ,SUBSET,IDATE)
            DO WHILE(IFBGET(LUBFI).EQ.0)
               ISUB = ISUB+1
               IDUP = JDUP(ISUB)
               if(idup.le.1) then
                  CALL COPYSB(LUBFI,LUBFJ,IRET)
               ELSE
                  CALL COPYSB(LUBFI,00000,IRET)
               ENDIF
               NDUP(IDUP) = NDUP(IDUP)+1
            ENDDO
         ELSE
            IF(NSUBS.GT.0) THEN
               DO N=1,NSUBS
                  IDUP = JDUP(ISUB+N)
                  NDUP(IDUP) = NDUP(IDUP)+1
               ENDDO
            ENDIF

C  In the event that the input file contains dummy center and dump time
C    messages 1 and 2 (after table messages), call CLOSMG with a
C    positive unit number to signal routine that it should write out
C    these messages even though they have zero subsets in them
C  If the input file does not contain dummy messages, a positive unit
C    number here is immaterial because CLOSMG was already called with
C    a negative unit number immediately after the output file was
C    opened (and this holds for all subsequent calls to CLOSMG
C    regardless of the sign of the unit number)
C  -------------------------------------------------------------------

            CALL CLOSMG(LUBFJ)
            CALL COPYMG(LUBFI,LUBFJ)
            ISUB = ISUB+NSUBS
         ENDIF
      ENDDO
 
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

  400 CONTINUE

C  GENERATE REPORT
C  ---------------
 
      if(aircft)  then
         print 302, isub,ntab,ndup(0)+ndup(1),ndup(0),ndup(1),ndup(4)
  302 format(/'BUFR_DUPCOR READ IN A TOTAL OF',i12,' REPORTS'/
     .        'BUFR_DUPCOR CHECKED A TOTAL OF',i12,' REPORTS'//
     .        'NUMBER OF REPORTS WRITTEN OUT ..................',i7/
     .        '   INCLUDES ',i8,' NON-DUP CHECKED REPORTS'/
     .        '   INCLUDES ',i8,' CORRECTED REPORTS'/
     .        'NUMBER OF REPORTS SKIPPED DUE TO:'/
     .        '   BEING OUTSIDE TIME WINDOW FOR TRIMMING ......',i7/)
      else IF(.NOT.METAR)  THEN
         PRINT 300, ISUB,NTAB,NDUP(0)+NDUP(1),NDUP(0),NDUP(1),NDUP(2),
     .              NDUP(4)
  300 FORMAT(/'BUFR_DUPCOR READ IN A TOTAL OF',I12,' REPORTS'/
     .        'BUFR_DUPCOR CHECKED A TOTAL OF',I12,' REPORTS'//
     .        'NUMBER OF REPORTS WRITTEN OUT ..................',I7/
     .        '   INCLUDES ',I8,' UNIQUE REPORTS'/
     .        '   INCLUDES ',I8,' CORRECTED REPORTS'/
     .        'NUMBER OF REPORTS SKIPPED DUE TO:'/
     .        '   FAILING DUPLICATE CHECK .....................',I7/
     .        '   BEING OUTSIDE TIME WINDOW FOR TRIMMING ......',I7/)
      ELSE
         PRINT 301, ISUB,NTAB,NDUP(0)+NDUP(1),NDUP(0),NDUP(1),NDUP(2),
     .              NDUP(4)
  301 FORMAT(/'BUFR_DUPCOR READ IN A TOTAL OF',I12,' REPORTS'/
     .        'BUFR_DUPCOR CHECKED A TOTAL OF',I12,' REPORTS'//
     .        'NUMBER OF REPORTS WRITTEN OUT ..................',I7/
     .        '   INCLUDES ',I8,' UNIQUE REPORTS'/
     .        '   INCLUDES ',I8,' CORRECTED OR THRPT=METAR REPORTS'/
     .        'NUMBER OF REPORTS SKIPPED DUE TO:'/
     .        '   FAILING DUPLICATE CHECK .....................',I7/
     .        '   BEING OUTSIDE TIME WINDOW FOR TRIMMING ......',I7/)
      ENDIF

C  END OF PROGRAM
C  --------------

      CALL W3TAGE('BUFR_DUPCOR')
      STOP

C  ERROR EXITS
C  -----------

  900 CONTINUE

      PRINT *, '#####BUFR_DUPCOR - EOF/ERR READING STDIN'
      CALL W3TAGE('BUFR_DUPCOR')
      CALL ERREXIT(99)

  901 CONTINUE

      PRINT *, '#####BUFR_DUPCOR - UNABLE TO ALLOCATE ARRAYS'
      CALL W3TAGE('BUFR_DUPCOR')
      CALL ERREXIT(99)

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    REMTDY
C   PRGMMR: SAGER            ORG: NP12       DATE: 2001-03-20
C
C ABSTRACT: DETERMINES MONTH-OF-YEAR AND DAY-OF-MONTH GIVEN FOUR-DIGIT
C   YEAR AND DAY-OF-YEAR.
C
C PROGRAM HISTORY LOG:
C 2001-03-20  L. SAGER -- ORIGINAL AUTHOR
C
C USAGE:    CALL REMTDY(IYEAR,IDOY,MON,IDAY)
C   INPUT ARGUMENT LIST:
C     IYEAR    - YEAR (YYYY)
C     IDOY     - DAY-OF-YEAR
C
C   OUTPUT ARGUMENT LIST:
C     MON      - MONTH-OF-YEAR
C     IDAY     - DAY-OF-MONTH
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C
C   SUBPROGRAMS CALLED (incomplete list):
C     UNIQUE:    - 
C     LIBRARY:
C       W3NCO    - W3DOXDAT W3FS26
C
C REMARKS: THIS SUBROUTINE WILL WORK FROM 1583 A.D. TO 3300 A.D.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  WCOSS
C$$$
      SUBROUTINE REMTDY(IYEAR,IDOY,MON,IDAY)

      INTEGER    IDAT(8)

      DATA IDAT  /0,1,1,5*0/

C     First, calculate the Julian day on Jan. 1 of year.

ccccc print *,' remtdy   iyear dayyr = ',iyear,idoy
      IDAT(1) = IYEAR
      CALL W3DOXDAT(IDAT,JDOW,JDOY,JDAY)

ccccc print *,' dox-dow doy day ',jdow,jdoy,jday

C     Add the day-of-year to Julian day.

      jday = jday + idoy - 1
ccccc print *,' updated jday idoy are ',jday,idoy

C     Call W3FS26 to get month/day from the Julian day.

      CALL W3FS26(JDAY,IYEAR,MON,IDAY,IDAYWK,IDAYYR)
ccccc print *,' year, month, day = ',iyear,mon,iday

      RETURN
      END
