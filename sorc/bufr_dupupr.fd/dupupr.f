C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_DUPUPR
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2018-02-20
C
C ABSTRACT: PROCESSES HIGH-VERTICAL RESOLUTION UPPER-AIR PROFILE REPORTS
C   DECODED FROM NATIVE BUFR.  INCLUDES CORRECTION CHOOSING, DUPLICATE
C   CHECKING AND (OPTIONAL) TRIMMING TO EXACT TIME WINDOW (MONTH DOWN TO
C   MINUTE).  INITALLY, THE UNCHECKED, UNCORRECTED AND UNWINDOWED BUFR
C   DUMP FILE COMING IN IS EXAMINED AND ALL SUBSETS (REPORTS)
C   ORIGINATING FROM SITES WITH A WMO BLOCK NUMBER NOT INCLUDED IN THE
C   CURRENT LIST OF ACCEPTED BUFR-FEED REPORT WMO BLOCK NUMBERS (DUE TO
C   KNOWN IMPROPER CODING TECHNIQUES) ARE THROWN OUT.  NEXT. THE
C   ALGORITHM SORTS THE REMAINING ACCEPTED REPORTS IN ASCENDING ORDER OF
C   LAT, LON, REPORT ID, OBS TIME (MONTH DOWN TO MINUTE), RECEIPT TIME
C   (YEAR DOWN TO MINUTE), AND CORRECTION INDICATOR.  IN THE DUPLICATE
C   CHECKING, THE REPORT USUALLY SELECTED IS THE BULLETIN LAST RECEIVED
C   (AND IF TWO BULLETINS TIE HERE, THE ONE THAT IS CORRECTED).  REPORTS
C   ARE CHECKED FOR LAT, LON, REPORT ID AND OBS TIME (MONTH DOWN TO
C   MINUTE) ALL INSIDE THE TOLERANCE LIMITS.  THE FILE PATH/NAMES OF THE
C   INPUT AND OUTPUT FILES, (OPTIONALLY) THE TIME WINDOW TO TRIM TO AND
C   (OPTIONALLY) DEFAULT OVERRIDE DUP-CHECKING TOLERANCE LIMITS ARE READ
C   FROM STANDARD INPUT AT THE START OF THIS PROGRAM. IF THE TIME WINDOW
C   RECORD IS MISSING, THEN NO TIME WINDOW TRIMMING IS PERFORMED. ALL
C   FILE CONNECTIONS (EXCEPT STANDARD INPUT WHICH IS PRE-CONNECTED) ARE
C   MADE THROUGH THE FORTRAN OPEN STATEMENT.
C
C PROGRAM HISTORY LOG:
C 2018-02-20  D.A. KEYSER --  ORIGINAL VERSION FOR IMPLEMENTATION
C
C 2018-12-21  C. Hill
C             - Included additional (13) station block numbers based on
C               verified quality of the station data.
C
C 2019-11-21  C. Hill
C             - Verified program function on Dell machines (no changes).
C
C 2020-03-20  C. Hill
C             - Added (15) more station block numbers based on verified
C               data quality and their significance; includes
C               China stations {5[0-9]} and some West Africa stations
C               {6[0-1]}.
C
C 2022-05-25  C. Hill
C             -  Added station blocks 91 (Pacific islands) and 93 (NZ).
C
C 2022-11-30  C. Hill
C             -  Added station blocks 40 (IL, JD, SD)
C                and 48 (TH, MY, SG, VN).
C             -  Added capability to retrieve RPID value from WGOSLID
C                (local, 4th component of WIGOS ID) if RPID is otherwise
C                missing and WGOSLID conforms to five-digit format,
C                allowing preservation of profile data record.  The
C                capability to formally fill RPID with valid WGOSLID for
C                prepobs is added, but commented out; it is available
C                for any future use (may need character variable).
C
C     
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT - FIRST RECORD CONTAINS INPUT FILE NAME,
C                SECOND RECORD CONTAINS OUTPUT FILE NAME, OPTIONAL THIRD
C                RECORD CONTAINS TIME WINDOW TRIMMING SPECIFICATIONS
C                (THE YYYYMMDDHH<.HH> DATE OF THE EARLIEST TIME TO DUMP
C                AND THE YYYYMMDDHH<.HH> DATE OF THE LATEST TIME TO
C                DUMP), OPTIONAL FOURTH RECORD CONTAINS DUP-CHECKING
C                TOLERANCE LIMITS (IF FOURTH RECORD IS MISSING, DEFAULT
C                DUP-CHECKING TOLERANCE LIMITS ARE USED, IF THIRD RECORD
C                IS ALSO MISSING, NO TIME WINDOW TRIMMING IS PERFORMED)
C     UNIT 20  - UNCHECKED, UNCORRECTED AND UNWINDOWED BUFR DUMP FILE
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 50  - DUPLICATE CHECKED, CORRECTED AND (OPTIONAL) TIME WINDOW
C                TRIMMED BUFR DUMP FILE (CONTAINING ONLY REPORTS WITH
C                ACCEPTED WMO BLOCK NUMBERS)
C
C   SUBPROGRAMS CALLED:
C     SYSTEM     - GETENV   SYSTEM
C     LIBRARY:
C       W3NCO    - W3TAGB   W3TAGE  ERREXIT
C       W3EMC    - ORDERS
C       BUFRLIB  - DATELEN  OPENBF  COPYMG  UFBTAB   OPENMB  COPYSB
C                  IREADMG  CLOSMG  CLOSBF  MESGBC   MAXOUT
C                  IBFMS    COPYBF  ISETPRM IGETPRM  GETBMISS 
C                  UFBCNT   NMSUB   IREADSB UFBINT   UFBCPY  WRITSB
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
      PROGRAM BUFR_DUPUPR
 
      PARAMETER (MXTS=8)

      REAL(8),ALLOCATABLE :: TAB_8(:,:)
      REAL(8),ALLOCATABLE :: RAB_8(:,:)
CH    REAL(8),ALLOCATABLE :: WGIDL(:)
      INTEGER,ALLOCATABLE :: IWORK(:)
      INTEGER,ALLOCATABLE :: IORD(:)
      INTEGER,ALLOCATABLE :: JDUP(:)

      REAL(8)       BMISS, GETBMISS
      real(8)       RPID_8
      character*8   CRPID
      equivalence (crpid,rpid_8)

      CHARACTER*500 FILI,FILO
      CHARACTER*80  TSTRH,RSTR
      CHARACTER*8   SUBSET,CAB8_IREC,CAB8_JREC
      CHARACTER*3   DUMMY_MSGS
      CHARACTER*2   CITIMESm1

      DIMENSION     NDUP(0:5)

      REAL(8)       ADATE,BDATE,CDATE,DDATE,RDATE,UFBTAB_8
      REAL(8)       TAB8_IREC_8,TAB8_JREC_8

      LOGICAL       DUPES

      EQUIVALENCE   (TAB8_IREC_8,CAB8_IREC),(TAB8_JREC_8,CAB8_JREC)

      DATA TSTRH /'CLATH CLONH MNTH DAYS HOUR MINU CORN RPID '/
      DATA RSTR  /'RCYR  RCMO  RCDY RCHR RCMI                '/

      DATA ADATE /00000000.00_8/
      DATA BDATE /99999999.00_8/
      DATA DEXY  /0/
      DATA DMON  /0/
      DATA DDAY  /0/
      DATA DOUR  /0/
      DATA DMIN  /0/
C-----IG_TIME_sections_of_this_code
      REAL(8)     TT1,TT2,TT3,TT4,TT5,TT6,TT7,TT8,TT9,TT10,TT11,TT12
C-----------------------------------------------------------------------

C IBLK_NUM determines which WMO block numbers will be accepted for
c processing here (=1) and which will be rejected (=0)

      DIMENSION IBLK_NUM(00:99)
      DATA IBLK_NUM /

C -> BLK #: 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19
C           -----------------------------------------------------------
     .       0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0,

C -> BLK #: 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39
C           -----------------------------------------------------------
     .       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,

C -> BLK #: 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59
C           -----------------------------------------------------------
     .       1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,

C -> BLK #: 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
C           -----------------------------------------------------------
     .       1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0,

C -> BLK #: 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99
C           -----------------------------------------------------------
     .       0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0/


C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_DUPUPR',2020,0080,0054,'NP22') 

      print *
      print * ,'---> Welcome to BUFR_DUPUPR - Version 03-20-2020'
      print *

C-----IG_TIME_sections_of_this_code
      call cpu_time(TT1)

C  Override current BUFRLIB maximum number of data values in an
C   uncompressed BUFR subset (80000) (due to hi-vert res raobs)
C -------------------------------------------------------------
      IRET=ISETPRM('MAXSS',300000 )  ! must use DA version of BUFRLIB
      IF(IRET.EQ.0)  THEN
         IMAXSS=IGETPRM('MAXSS')
         PRINT'(/" MAXIMUM NUMBER OF DATA VALUES IN AN UNCOMPRESSED",
     .    " BUFR SUBSET (MAXSS) SET TO ",I0)', IMAXSS
      ELSE
         PRINT'(/25("*"),"ABORT",25("*")/"#####BUFR_DUPUPR - ATTEMPT ",
     .    "TO SET MAXSS FAILED"/25("*"),"ABORT",25("*")/)'
         CALL W3TAGE('BUFR_DUPUPR')
         call errexit(99)
      ENDIF

      CALL DATELEN(10)

ccccc CALL OPENBF(0,'QUIET',2) ! Uncomment for extra print from bufrlib

C  ASSIGN DEFAULT VALUE FOR 'MISSING' TO LOCAL BMISS VARIABLE
C  ----------------------------------------------------------

      BMISS = GETBMISS()     ! assign default value for "missing"
      print *
      print *, 'BUFRLIB value for missing is: ',bmiss
      print *

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

cppppp
ccc   print *, 'file fili is ',nbytes_fili,' bytes long'
ccc   print *, 'file filo is ',nbytes_filo,' bytes long'
cppppp

      READ(5,*,END=1) ADATE,BDATE
      READ(5,*,END=1) DEXY,DOUR,DMIN

    1 CONTINUE

C-----IG_TIME_sections_of_this_code
      call cpu_time(TT2)

      IF(BDATE.NE.99999999.00_8) THEN
         PRINT 200, ADATE,BDATE
      ELSE
         PRINT 201
      ENDIF
  200 FORMAT(/'REQUESTED EARLIEST DATE IS ....... ',F15.2/
     .        'REQUESTED LATEST   DATE IS ....... ',F15.2)
  201 FORMAT(/'@@@@ AS REQUESTED, NO TIME WINDOW TRIMMING IS PERFORMED'/
     .        '@@@@ ALL NON-DUPLICATES ARE RETAINED REGARDLESS OF TIME')
      PRINT 202, FILI(1:NBYTES_FILI),FILO(1:NBYTES_FILO),DEXY,DMON,DDAY,
     . DOUR,DMIN
  202 FORMAT(/'UNCHECKED AND UNCORRECTED INPUT FILE IS         '/5X,A/
     .        'DUPLICATE CHECKED AND CORRECTED OUTPUT FILE IS'/5X,A//
     .        'BUFR_DUPUPR PARAMETERS:'/
     .        3X,'TOLERANCE FOR LAT/LON CHECKS (IN DEGREES) .. ',F5.1/
     .        3X,'TOLERANCE FOR YEAR CHECK (** NOT CHECKED **) '/
     .        3X,'TOLERANCE FOR MONTH CHECK (IN MONTHS) ...... ',F5.1/
     .        3X,'TOLERANCE FOR DAY CHECK (IN DAYS) .......... ',F5.1/
     .        3X,'TOLERANCE FOR HOUR CHECK (IN HOURS) ........ ',F5.1/
     .        3X,'TOLERANCE FOR MINUTE CHECK (IN MINUTES) .... ',F5.1/
     .        3X,'TOLERANCE FOR SECOND CHECK (** NOT CHECKED **) ')


C  OPEN FILE TEMPORARILY TO CHECK FOR COMPRESSION AND TO SEE WHAT THE
C   BUFR MESSAGE TYPE IS (SUBSET)
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
         CALL W3TAGE('BUFR_DUPUPR')
         CALL ERREXIT(00)
      ELSE  IF(ICOMP.EQ.-2)  THEN
         PRINT'(/"INPUT BUFR FILE HAS NO DATA MESSAGES"/"FIRST '//
     .    'MESSAGE TYPE FOUND IS",I5/)', MSGT
      ENDIF

C-----IG_TIME_sections_of_this_code
      call cpu_time(TT3)

      CALL OPENBF(LUBFI,'IN',LUBFI)
      if(ireadmg(lubfi,subset,idate).ne.0) then
         print *, '===> BUFR_DUPUPR - NO DATA IN INPUT FILE'
         print *, '===> THIS PROGRAM IS DONE - STOP'
         print *
         call closbf(lubfi)
         open(lubfi,file=fili(1:nbytes_fili),form='UNFORMATTED')
         open(lubfj,file=filo(1:nbytes_filo),form='UNFORMATTED')
         call copybf(lubfi,lubfj)
         call closbf(lubfi)
         call closbf(lubfj)
         call w3tage('BUFR_DUPUPR')
         call errexit(00)
      endif

      CALL CLOSBF(LUBFI)
c=======================================================================
c=======================================================================

c  this section tosses all subsets (reports) with an id not included in
c   the current list of accepted BUFR-feed WMO block numbers -
c   (see array IBLK_NUM above) --
c   subsequent dup-checking and time window trimming in this code and
c   then dump processing downstream of this code will operate only on
c   reports with accepted WMO block numbers
c ----------------------------------------------------------------------

      print *
      print'("===> Toss all reports with a WMO block number that is ",
     . "currently not accepted")'
      print *
      print'("     Current accepted WMO block numbers are: ")'
      do k = 00,99
         if(IBLK_NUM(k).eq.1) print'(15x,i3.2)', k
      enddo
      print *

C-----IG_TIME_sections_of_this_code
      call cpu_time(TT4)

      kount = 0
      kept = 0
      ktossed = 0

      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      OPEN(LUBFJ,FILE=FILO(1:NBYTES_FILO),FORM='UNFORMATTED')

      CALL OPENBF(LUBFI,'IN',LUBFI)
      CALL OPENBF(LUBFJ,'OUT',LUBFI)
      call maxout(200000)
      DO WHILE(IREADMG(LUBFI,SUBSET,IDATE).EQ.0)
         CALL UFBCNT(LUBFI,IRECI,ISUBI)
         ISUBI = NMSUB(LUBFI)
         IF(IRECI.LE.2.AND.ISUBI.EQ.0)  THEN
            PRINT 111
  111 FORMAT(' This is a dummy message at the top of a data dump BUFR ',
     . 'file containing the center dump time (record 1) or the dump'/
     . ' processing time (record 2); copy it, intact, to output BUFR ',
     . 'file')
            CALL CLOSMG(LUBFJ)
            CALL COPYMG(LUBFI,LUBFJ)
            CYCLE
         ENDIF
         DO WHILE(IREADSB(LUBFI).EQ.0)
            CALL UFBINT(LUBFI,RPID_8,1,1,NLV,'RPID')
            IF(NLV.EQ.0.OR.IBFMS(RPID_8).NE.0) THEN
              CALL UFBINT(LUBFI,RPID_8,1,1,NLV,'WGOSLID')
C             If RPID is absent, then read the local component of WIGOS ID
C             (if valid - see below) to preserve the data record.
              IF(NLV.EQ.0.OR.IBFMS(RPID_8).NE.0) THEN
               PRINT *, '#####BUFR_DUPUPR - COULDN''T OBTAIN REPORT ID'
               CALL W3TAGE('BUFR_DUPUPR')
               CALL ERREXIT(99)
              ENDIF
            ENDIF
            kount = kount + 1
            if(SUBSET(3:8) == "002101") then
            read(CRPID(1:2),'(I2)') ithis_blk_num
            if(ithis_blk_num.lt.00.or.ithis_blk_num.gt.99) then
               PRINT *, '#####BUFR_DUPUPR - REPORT HAS INVALID WMO ',
     .                  'BLOCK NUMBER (',ithis_blk_num,')'
               CALL W3TAGE('BUFR_DUPUPR')
               CALL ERREXIT(99)
            endif
            if(IBLK_NUM(ithis_blk_num).ne.1) then
               ktossed = ktossed + 1
               cycle
            endif
            endif
            print *, 'retain accepted report with id: ',CRPID
            CALL OPENMB(LUBFJ,SUBSET,IDATE)
            CALL UFBCPY(LUBFI,LUBFJ)

            CALL WRITSB(LUBFJ)
            kept = kept + 1
         ENDDO
      ENDDO
      CALL CLOSBF(LUBFI)
      CALL CLOSMG(LUBFJ)
      CALL CLOSBF(LUBFJ)

C-----IG_TIME_sections_of_this_code
      call cpu_time(TT5)

      print *
      print *, ' ==> total number of reports read in            ',
     . '            = ',kount
      print *, ' ==> total number of reports with ID''s that are ',
     . ' considered = ',kept
      print *, ' ==> total number of reports with ID''s that are ',
     . ' rejected   = ',ktossed
      print *

      open(lubfi,file=fili(1:nbytes_fili),form='UNFORMATTED')
      open(lubfj,file=filo(1:nbytes_filo),form='UNFORMATTED')
      call copybf(lubfj,lubfi)
      call closbf(lubfi)
      call closbf(lubfj)

      print *
      print *, ' ===> Move on to dup-checking'
      print *
C-----IG_TIME_sections_of_this_code
      call cpu_time(TT6)
c=======================================================================
c=======================================================================

C  COUNT THE NUMBER OF SUBSETS IN THE FILE TO ALLOCATE SPACE
C  ---------------------------------------------------------

      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      CALL OPENBF(0,'QUIET',1) ! will generate diagnostic print if an
                               ! embedded BUFR table is read
      CALL UFBTAB(-LUBFI,UFBTAB_8,1,1,MXTB,' ')
      CALL OPENBF(0,'QUIET',0) ! return to default wrt degree of print

      ALLOCATE(TAB_8(MXTS,MXTB),STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(RAB_8(MXTS,MXTB),STAT=I);IF(I.NE.0) GOTO 901
CH    ALLOCATE(WGIDL(MXTB)     ,STAT=I)
      ALLOCATE(IWORK(MXTB)     ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(IORD(MXTB)      ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(JDUP(MXTB)      ,STAT=I);IF(I.NE.0) GOTO 901

      TAB_8  = BMISS
      RAB_8  = BMISS
      WGIDL  = BMISS
      JDUP   = 0
      IORD   = 0

      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')

C-----IG_TIME_sections_of_this_code
      call cpu_time(TT7)
C      IF(0.EQ.1) THEN ! IG TEST 1 start
C  MAKE A TABLE OUT OF THE LATS, LONS, ID'S, OBS TIME COORDINATES AND
C   RECEIPT TIME COORDINATES
C  ------------------------------------------------------------------
      print * ,'ILIANA in first slow down' 
      DO ITIMES=1,MXTB ! Look thru up to 25 rpts to find a valid lat
         !IF(ITIMES.EQ.26.OR.ITIMES.EQ.MXTB) THEN
            !IF(ITIMES.EQ.26) THEN
         IF(ITIMES.EQ.5.OR.ITIMES.EQ.MXTB) THEN
            IF(ITIMES.EQ.5) THEN
              PRINT 1858
 1858 FORMAT(/'##WARNING: THE FIRST 25 REPORTS IN INPUT FILE HAVE A ',
     . 'MISSING LATITUDE, ALL REPORTS MAY HAVE MISSING LAT/LON'/)
               CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg'//
     .          ' "$jlogfile" "***WARNING: THE FIRST 25 REPORTS IN '//
     .          'INPUT FILE HAVE A MISSING LAT, ALL REPORTS MAY HAVE '//
     .          'MISSING LAT/LON, TYPE="'//SUBSET)
            ELSE
               PRINT 1859
 1859 FORMAT(/'##WARNING: ALL REPORTS IN INPUT FILE HAVE MISSING LAT/',
     . 'LON'/)
               CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg'//
     .          ' "$jlogfile" "***WARNING: ALL REPORTS IN INPUT FILE '//
     .          'HAVE MISSING LAT/LON, TYPE="'//SUBSET)
            ENDIF
            EXIT
         ENDIF
         OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
         CALL UFBTAB(LUBFI,TAB_8,MXTS,MXTB,NTAB,TSTRH)
CH       CALL UFBTAB(LUBFI,WGIDL,1,MXTB,NTAB,'WGOSLID')
         IF(IBFMS(TAB_8(1,ITIMES)).EQ.1) THEN    ! data missing
          ! lat missing for this rpt, try lat for next rpt
cpppppppppp
ccc         print *, ' TAB_8(1,'ITIMES,') missing - try next report'
cpppppppppp
            OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
            CYCLE
         ENDIF
         ! lat valid for this rpt
         IF(ITIMES.GT.1) THEN
            PRINT 1860, ITIMES-1
 1860 FORMAT(/'##WARNING: THE FIRST ',I2,' REPORT(S) IN INPUT FILE ',
     . 'HAVE A MISSING LATITUDE'/)
            WRITE(CITIMESm1,'(I2)') ITIMES-1
            CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg'//
     .       ' "$jlogfile" "***WARNING: THE FIRST '//CITIMESm1//
     .       ' REPORT(S) IN INPUT FILE HAVE A MISSING LATITUDE'//
     .       ', TYPE="'//SUBSET)
         ENDIF
cpppppppppp
ccc      print *, 'TAB_8(1,',ITIMES,') valid - move on'
cpppppppppp
         EXIT
      ENDDO

C      ENDIF ! IG TEST 1 end

C-----IG_TIME_sections_of_this_code
      call cpu_time(TT8)
      IF(0.EQ.1) THEN ! IG TEST 2 start !turn off CORN rewrite
      print * ,'ILIANA in second slow down'
              OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      CALL UFBTAB(LUBFI,RAB_8,MXTS,MXTB,NTAB,RSTR)


      DO N=1,NTAB

C  SET MISSING CORRECTION INDICATOR ("CORN") TO ZERO (NOT CORRECTED)
C  -----------------------------------------------------------------
         IF(IBFMS(TAB_8(7,N)).EQ.1) TAB_8(7,N) = 0
         IF(TAB_8(7,N).GT.0) JDUP(N) = 1  ! mark corrected reports

C  SET MISSING MINUTES ("MINU") TO ZERO
C  ------------------------------------
         IF(IBFMS(TAB_8(6,N)).EQ.1) TAB_8(6,N) = 0

C  IF AVAILABLE (AND IF NECESSARY), SUBSTITUTE WGOSLID FOR MISSING RPID
C  ------------------------------------
c       IF(IBFMS(TAB_8(8,N)).NE.0) THEN
c        IF(IBFMS(WGIDL(N)).EQ.0) THEN
c         IF ((WGIDL(N).GE.100).AND.(WGIDL(N).LE.99998))
c    +       TAB_8(8,N) = WGIDL(N)
c        ENDIF
c       ENDIF
      ENDDO

      ENDIF ! IG TEST 2 end

C-----IG_TIME_sections_of_this_code
      call cpu_time(TT9)

C  GET A SORTED INDEX OF THE REPORTS KEYED IN THIS ORDER: LAT, LON,
C   REPORT ID, OBS TIME, RECEIPT TIME, CORRECTION INDICATOR
C  ----------------------------------------------------------------

C-----IG_TIME_sections_of_this_code
      call cpu_time(TT10)

      print * ,'ILIANA= runs the MINIMUM (6)  out of 13 ORDERS' 
      CALL ORDERS( 2,IWORK,TAB_8(7,1),IORD,NTAB,MXTS,8,2) ! correction
      !IG skip unnecessary sorting by receipt time 
      !CALL ORDERS(12,IWORK,RAB_8(5,1),IORD,NTAB,MXTS,8,2) ! rcpt minute
      !CALL ORDERS(12,IWORK,RAB_8(4,1),IORD,NTAB,MXTS,8,2) ! rcpt hour
      !CALL ORDERS(12,IWORK,RAB_8(3,1),IORD,NTAB,MXTS,8,2) ! rcpt day
      !CALL ORDERS(12,IWORK,RAB_8(2,1),IORD,NTAB,MXTS,8,2) ! rcpt month
      !CALL ORDERS(12,IWORK,RAB_8(1,1),IORD,NTAB,MXTS,8,2) ! rcpt year
      CALL ORDERS(12,IWORK,TAB_8(6,1),IORD,NTAB,MXTS,8,2) ! obs minute
      CALL ORDERS(12,IWORK,TAB_8(5,1),IORD,NTAB,MXTS,8,2) ! obs hour
      CALL ORDERS(12,IWORK,TAB_8(4,1),IORD,NTAB,MXTS,8,2) ! obs day
      CALL ORDERS(12,IWORK,TAB_8(3,1),IORD,NTAB,MXTS,8,2) ! obs month
      CALL ORDERS(10,IWORK,TAB_8(8,1),IORD,NTAB,MXTS,8,2) ! report id
      CALL ORDERS(12,IWORK,TAB_8(2,1),IORD,NTAB,MXTS,8,2) ! longitude
      CALL ORDERS(12,IWORK,TAB_8(1,1),IORD,NTAB,MXTS,8,2) ! latitude

C-----IG_TIME_sections_of_this_code
      call cpu_time(TT11)

C  GO THROUGH THE REPORTS IN ORDER, MARKING DUPLICATES AND CORRECTIONS
C  -------------------------------------------------------------------
 
      DO K=1,NTAB-1
         IREC = IORD(K)
         JREC = IORD(K+1)
cpppp
ccc      print'("New input report number (k) ",i0," found in record ",
ccc  .    "(irec) ",i0)',k,irec
ccc      print'("Next input report number (k+1) ",i0," found in record",
ccc  .    " (jrec) ",i0)',k+1,jrec
cpppp
         TAB8_IREC_8 = TAB_8(8,IREC)
         TAB8_JREC_8 = TAB_8(8,JREC)
cpppp
ccc      print 1789, irec,CAB8_IREC,(tab_8(ii,irec),ii=1,2),
ccc  .    (nint(tab_8(ii,irec)),ii=3,6),(nint(rab_8(ii,irec)),ii=1,5),
ccc  .    nint(tab_8(7,irec))
c1789    format('IREC: ',I5,'; STNID: ',A8,';{ LAT: ',F9.5,'; LON: ',
ccc  .    F10.5,'; RPRT MMDDHHmm: ',4I2.2,' RCPT YYYYMMDDHHMM: ',I4,
ccc  .    4I2.2,'; CORN: ',I3,' }')
ccc      print 1790, jrec,CAB8_JREC,(tab_8(ii,jrec),ii=1,2),
ccc  .    (nint(tab_8(ii,jrec)),ii=3,6),(nint(rab_8(ii,jrec)),ii=1,5),
ccc  .    nint(tab_8(7,jrec))
c1790    format('JREC: ',I5,'; STNID: ',A8,';{ LAT: ',F9.5,'; LON: ',
ccc  .    F10.5,'; RPRT MMDDHHmm: ',4I2.2,' RCPT YYYYMMDDHHMM: ',I4,
ccc  .    4I2.2,'; CORN: ',I3,' }')
cpppp

c Need to use the KIDNNT() intrinsic function here, with 8byte integer
c output, in order to deal w/ the case when potentially large (ie,
c greater than 10e7) "missing" values are encountered.
         DUPES = KIDNNT(DABS(TAB_8(1,IREC)-TAB_8(1,JREC))*10000.) 
     .      .LE.NINT(DEXY*10000.)
     .     .AND. KIDNNT(DABS(TAB_8(2,IREC)-TAB_8(2,JREC))*10000.) 
     .      .LE.NINT(DEXY*10000.)
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
         IF(DUPES) THEN
            JDUP(IREC) = 2
cpppppppppp
            print 1799, irec,CAB8_IREC,(tab_8(ii,irec),ii=1,2),
     .       (nint(tab_8(ii,irec)),ii=4,6),
     .       (nint(rab_8(ii,irec)),ii=1,5),nint(tab_8(7,irec))
 1799       format('===> DUPL. FOUND:'/'TOSSED:   --> IREC: ',I6,
     .       ';{ ID: ',A8,'; LAT: ',F9.5,'; LON: ',F10.5,
     .       '; RPRT DD HH MM ',3I2.2,'; RCPT YYYYMMDDHHMM: ',I4,4I2.2,
     .       '; CORN: ',I3,'}')
            print 1800, jrec,CAB8_JREC,(tab_8(ii,jrec),ii=1,2),
     .       (nint(tab_8(ii,jrec)),ii=4,6),
     .       (nint(rab_8(ii,jrec)),ii=1,5),nint(tab_8(7,jrec))
 1800       format('KEPT:     --> JREC: ',I6,';{ ID: ',A8,'; LAT: ',
     .       F9.5,'; LON: ',F10.5,'; RPRT DD HH MM ',3I2.2,
     .       '; RCPT YYYYMMDDHHMM: ',I4,4I2.2,'; CORN: ',I3,'}')
cpppppppppp
         ENDIF
      ENDDO
 
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
      call maxout(200000)

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

C-----IG_TIME_sections_of_this_code
      call cpu_time(TT12)
      print *, 'ILIANA IG TIMERS TT 1-12: '
      print *, 'ILIANA',TT1,TT2,TT3,TT4,TT5,TT6,TT7,TT8,TT9,TT10,TT11,TT12
C  GENERATE REPORT
C  ---------------
 
      PRINT 300, ISUB,NTAB,NDUP(0)+NDUP(1),NDUP(0),NDUP(1),NDUP(2),
     .           NDUP(4)
  300 FORMAT(/'BUFR_DUPUPR READ IN A TOTAL OF',I12,' REPORTS'/
     .        'BUFR_DUPUPR CHECKED A TOTAL OF',I12,' REPORTS'//
     .        'NUMBER OF REPORTS WRITTEN OUT ..................',I7/
     .        '   INCLUDES ',I8,' UNIQUE REPORTS'/
     .        '   INCLUDES ',I8,' CORRECTED REPORTS'/
     .        'NUMBER OF REPORTS SKIPPED DUE TO:'/
     .        '   FAILING DUPLICATE CHECK .....................',I7/
     .        '   BEING OUTSIDE TIME WINDOW FOR TRIMMING ......',I7/)

C  END OF PROGRAM
C  --------------

      CALL W3TAGE('BUFR_DUPUPR')
      STOP

C  ERROR EXITS
C  -----------

  900 CONTINUE

      PRINT *, '#####BUFR_DUPUPR - EOF/ERR READING STDIN'
      CALL W3TAGE('BUFR_DUPUPR')
      CALL ERREXIT(99)

  901 CONTINUE

      PRINT *, '#####BUFR_DUPUPR - UNABLE TO ALLOCATE ARRAYS'
      CALL W3TAGE('BUFR_DUPUPR')
      CALL ERREXIT(99)

      END
