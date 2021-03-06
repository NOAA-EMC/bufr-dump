C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_DUMPMD
C   PRGMMR: DONG             ORG: NP22        DATE: 2020-08-20
C
C ABSTRACT: DUMPS DATA FROM A DATABASE FILE OR FILES WHICH FALL WITHIN
C   A USER SUPPLIED TIME WINDOW, BY LOOKING ONLY AT THE MESSAGE DATE
C   IN THE BUFR SECTION ONE HEADER RECORDS.  SINCE VIRTUALLY NO
C   UNPACKING NEEDS BE DONE FOR THIS PROCESS, IT IS VERY FAST.
C   HOWEVER, BECAUSE THE MESSAGE DATE IS ONLY ACCURATE TO THE HOUR,
C   ADDITIONAL "TRIMMING" OF DATA MAY BE NECESSARY IN A SUSBSEQUENT
C   PROCESSING STEP, SINCE THE USER TIME WINDOW IS ACCURATE TO
C   HUNDREDTHS OF AN HOUR.  (FOR THE DUMP SCRIPT SEQUENCE, THE
C   TRIMMING IS DONE IN THE DUPLICATE CHECKING STEP, WHERE THE
C   OBSERVATION TIME IS UNPACKED FROM BUFR SECTION FOUR ANYWAY.)  THE
C   TIME WINDOW TO DUMP, THE FILE PATH/NAMES OF TWO DATABASE FILES,
C   AND THE FILE PATH/NAME OF THE OUTPUT DUMP FILE, AMONG OTHER THINGS,
C   ARE READ FROM STANDARD INPUT (UNIT 5) AT THE START OF THIS PROGRAM.
C   ALL OTHER FILE CONNECTIONS ARE MADE THROUGH THE FORTRAN OPEN
C   STATEMENT.  OPTIONALLY, GENERATES TWO DUMMY MESSAGES AT THE
C   BEGINNING OF THE OUTPUT DUMP FILE WHICH CONTAIN ONLY THE DUMP
C   CENTER TIME AND THE CURRENT PROCESSING TIME ("DUMP" TIME) IN
C   SECTION ONE.
C
C PROGRAM HISTORY LOG:
C 1996-09-06  J. WOOLLEN  ORIGINAL VERSION FOR IMPLEMENTATION
C 1999-06-03  D. KEYSER   MODIFIED TO PORT TO IBM SP AND RUN IN 4 OR
C     8 BYTE STORAGE
C 2000-12-05  D. KEYSER   INCREASED LIMIT FOR I/O FILENAME LENGTH
C     FROM 80 CHARACTERS TO 500 CHARACTERS
C 2002-03-05  D. KEYSER   IMPROVED DOCUMENTATION; ADDED CALL TO
C     COMPRESS_CHECK TO INDICATE IF INPUT/OUTPUT FILES ARE COMPRESSED
C     OR UNCOMPRESSED
C 2004-02-02  D. KEYSER   REPLACED CALL TO IN-LINE SUBROUTINE
C     COMPRESS_CHECK WITH CALL TO NEW BUFRLIB ROUTINE MESGBC
C 2006-02-02  D. KEYSER - REPLACED CALL TO BUFRLIB ROUTINE IREADFT WITH
C     CALL TO BUFRLIB ROUTINE IREADMG WITH THE NEGATIVE OF THE LOGICAL
C     UNIT NUMBER PASSED IN (IREADFT OBSOLETE WITH 1/31/2006 VERSION OF
C     BUFRLIB) (SEE COMMENTS IN CODE FOR MORE INFORMATION)
C 2006-03-02  D. KEYSER   NO LONGER HARDWIRED TO WRITE "DUMMY" MESSAGES
C     CONTAINING DUMP CENTER TIME AND PROCESSING TIME, RESP. INTO FIRST
C     TWO MESSAGES OF OUTPUT WINDOWED DUMP FILE (AFTER TABLE MSGS),
C     WILL ONLY DO SO IF DUMPJB SCRIPT VARIABLE "DUMMY_MSGS" (READ IN
C     VIA "GETENV") IS "YES" AND THE DUMP PROCESSING TIME IS
C     SUCCESSFULLY READ FROM UNIT 17 (THE DUMP PROCESSING TIME IS NO
C     LONGER READ IN FROM STANDARD INPUT) - NORMALLY THIS PROGRAM WILL
C     NOT PERFORM THIS FUNCTION ANY LONGER FOR ANY DATA TYPE (IT HAS
C     BEEN MOVED TO PROGRAM BUFR_COMBFR) BECAUSE THE LEVEL 2 RADAR DUMP
C     PROCESSING NO LONGER EXECUTES THIS PROGRAM IN ORDER TO SAVE TIME
C     (BECAUSE THERE IS SO MUCH DATA), THE EXCEPTION IS FOR CASES WHERE
C     DUMPJB SCRIPT VARIABLE "FORM" IS SET TO "copy" (IN THIS CASE,
C     BUFR_COMBFR DOES NOT RUN SO THIS PROGRAM MUST WRITE THE DUMMY
C     MESSAGES TO THE TOP OF THE DUMP FILE); ADDED PRINT INDICATING THE
C     TOTAL NUMBER OF BUFR MESSAGES READ IN FROM DATABASE
C 2010-05-25  D. KEYSER   AS A RESULT OF THE NEW BUFRLIB WHICH CAN
C     HANDLE EMBEDDED DICTIONARY MESSAGES, INCREASES AMOUNT OF BUFRLIB
C     PRINTOUT DURING (ONLY) THE POINT WHEN READING IN MESSAGES IN
C     ORDER TO DETECT ANY EMBEDDED DICTIONARY MESSAGES
C 2012-11-20  J. WOOLLEN  INITIAL PORT TO WCOSS -- ADAPTED IBM/AIX
C       GETENV SUBPROGRAM CALL TO INTEL/LINUX SYNTAX;
C 2013-01-13  J. WHITING  READIED FOR IMPLEMENTATION ON WCOSS LINUX
C       (UPDATED DOC-BLOCK, ETC.; NO LOGIC CHANGES)
C 2020-08-20  J. DONG   CHANGE THE CODE TO FIX OUTPUT CONVERSION ERROR
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT - FIRST RECORD CONTAINS THE
C                YYYYMMDDHH<.HH> DATE OF THE EARLIEST TIME TO
C                DUMP,  SECOND RECORD CONTAINS THE YYYYMMDDHH<.HH> DATE
C                OF THE LATEST TIME TO DUMP, THIRD RECORD CONTAINS THE
C                FIRST INPUT FILE NAME, FOURTH RECORD CONTAINS THE
C                SECOND INPUT FILE NAME (SET TO "NONE" IS THERE IS ONLY
C                ONE INPUT FILE), FIFTH RECORD CONTAINS OUTPUT FILE
C                NAME, SIXTH RECORD CONTAINS THE YYYYMMDDHH<.HH> DATE
C                OF THE DUMP CENTER TIME
C     UNIT 17  - IF PRESENT, CONTAINS THE YYYYMMDDHHMM DATE OF THE
C                CURRENT WALLCLOCK TIME; THE ABSENCE OF THIS FILE IS
C                A SIGNAL THAT THIS PROGRAM SHOULD NOT WRITE CENTER AND
C                DUMP TIME DUMMY MESSAGES TO THE TOP OF THE OUTPUT DUMP
C                FILE
C     UNIT 20  - DATABASE BUFR FILE(S)
C
C   OUTPUT FILES:
C     UNIT 50  - "ROUGH" TIME WINDOWED BUFR DUMP FILE, POSSIBLY WITH
C                CENTER TIME AND DUMP TIME DUMMY MESSAGES AT THE
C                BEGINNING (TOP)
C
C   SUBPROGRAMS CALLED:
C     UNIQUE     - NONE
C     SYSTEM     - GETENV
C     LIBRARY:
C       W3NCO    - W3TAGB   W3TAGE   ERREXIT
C       BUFRLIB  - MESGBC   DATELEN  OPENBF   COPYMG   OPENMG
C                  IREADMG  MINIMG   CLOSMG   CLOSBF
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C            > 0 - ABNORMAL RUN
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  WCOSS
C
C$$$
      PROGRAM BUFR_DUMPMD
 
      CHARACTER*500 FILI(2),FILO
      CHARACTER*8   SUBSET
      CHARACTER*3   DUMMY_MSGS
      DIMENSION     TIME(5)
      LOGICAL       OPENOT,COPY_DUMMY_MSGS
      REAL(8)       ADATE,BDATE,CDATE,DDATE
      INTEGER(8)    LDATE_8,MDATE_8
 
      DATA OPENOT/.TRUE./
      DATA LUNIN /20/
      DATA LUNOT /50/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_DUMPMD',2020,0233,0062,'NP22') 

      print *
      print * ,'---> Welcome to BUFR_DUMPMD - Version 08-20-2020'
      print *

      CALL DATELEN(10)

ccccc CALL OPENBF(0,'QUIET',2) ! Uncomment for extra print from bufrlib

      ICPY = 0
      ITOT = 0
 
C  READ THE DATE/TIME/FILE PARAMETERS
C  ----------------------------------
 
      READ(5,*,END=900,ERR=900) ADATE
      READ(5,*,END=900,ERR=900) BDATE
      READ(5,'(Q,A)',END=900,ERR=900) NBYTES_FILI1,
     $ FILI(1)(1:NBYTES_FILI1)
      READ(5,'(Q,A)',END=900,ERR=900) NBYTES_FILI2,
     $ FILI(2)(1:NBYTES_FILI2)
      READ(5,'(Q,A)',END=900,ERR=900) NBYTES_FILO,FILO(1:NBYTES_FILO)

cppppp
ccc   print *, 'file fili(1) is ',nbytes_fili1,' bytes long'
ccc   print *, 'file fili(2) is ',nbytes_fili2,' bytes long'
ccc   print *, 'file filo is    ',nbytes_filo,' bytes long'
cppppp

      READ(5,*,END=900,ERR=900) CDATE

      PRINT *,'REQUESTED EARLIEST DATE IS ... ',ADATE
      PRINT *,'REQUESTED LATEST   DATE IS ... ',BDATE
      PRINT *,'REQUESTED CENTER   DATE IS ... ',CDATE
      PRINT *
      PRINT *,'DATABASE INPUT FILE 1 IS'
      PRINT *,'     ',FILI(1)(1:NBYTES_FILI1)
      PRINT *,'DATABASE INPUT FILE 2 IS'
      PRINT *,'     ',FILI(2)(1:NBYTES_FILI2)
      PRINT *,'TIME WINDOWED OUTPUT DUMP FILE IS'
      PRINT *,'     ',FILO(1:NBYTES_FILO)
      PRINT *

      JDATE = ADATE
      KDATE = BDATE

      COPY_DUMMY_MSGS = .FALSE.

      CALL GETENV('DUMMY_MSGS',DUMMY_MSGS)
      IF(DUMMY_MSGS.EQ.'YES') THEN

C  Pgm expected to generate "Dummy" msgs containing center & dump times
C  --------------------------------------------------------------------

         READ(17,*,END=8,ERR=8) DDATE
         PRINT *,'DUMP PROCESSING    DATE IS ... ',DDATE
         LDATE_8 = INT(CDATE)*100_8 + NINT((CDATE-INT(CDATE))*60.)
         MDATE_8 = DDATE
         LMINS = MOD(LDATE_8,100_8)
         MMINS = MOD(MDATE_8,100_8)
         LDATE = LDATE_8/100
         MDATE = MDATE_8/100
         COPY_DUMMY_MSGS = .TRUE.
      ENDIF

      GO TO 9

8     CONTINUE

C  Dump time not found in unit 17, "dummy" messages can't be generated
C  -------------------------------------------------------------------

      PRINT *
      PRINT'("+++ WARNING: CENTER AND/OR DUMP DATE NOT FOUND IN UNIT '//
     $ '17 - ""DUMMY"" MESSAGES NOT WRITTEN TO TOP OF OUTPUT FILE")'
      PRINT *

9     CONTINUE

C  OPEN THE OUTPUT BUFR FILE WITH AN INPUT FILE BUFR TABLE
C  -------------------------------------------------------
 
      OPEN(LUNOT,FILE=FILO(1:NBYTES_FILO),FORM='UNFORMATTED')

      IF(FILI(1)(1:4).NE.'NONE') THEN
         OPEN(LUNIN,FILE=FILI(1)(1:NBYTES_FILI1),FORM='UNFORMATTED')
      ELSEIF(FILI(2)(1:4).NE.'NONE') THEN
         OPEN(LUNIN,FILE=FILI(2)(1:NBYTES_FILI2),FORM='UNFORMATTED')
      ELSE
         PRINT *,'#####BUFR_DUMPMD - NEITHER INPUT DATABASE FILE ',
     .    'SPECIFIED - STOP'
         CALL W3TAGE('BUFR_DUMPMD')
         CALL ERREXIT(0)
      ENDIF

C  OUTPUT FILE OPENED WITH INPUT BUFR TABLE
C  ----------------------------------------
 
      CALL OPENBF(LUNIN,'IN ',LUNIN)
      CALL OPENBF(LUNOT,'OUT',LUNIN)

C  CALL IREADMG WITH THE NEGATIVE OF THE LOGICAL UNIT NUMBER PASSED IN
C   ARGUMENT 1 - THIS SIGNALS READMG TO TREAT A READ ERROR AS AN END-
C   OF-FILE CONDITION RATHER THAN AS AN ABORT, WHICH READMG WOULD
C   OTHERWISE DO (ALLOWS READMG TO REPLACE OBSOLETE BUFRLIB ROUTINE
C   READFT)
C  -------------------------------------------------------------------

      IF(IREADMG(-LUNIN,SUBSET,IDATE).NE.0) THEN
         PRINT *, '#####BUFR_DUMPMD - NO DATA IN INPUT DATABASE FILE -',
     .    ' STOP'
         CALL W3TAGE('BUFR_DUMPMD')
         CALL ERREXIT(0)
      ENDIF

      IF(COPY_DUMMY_MSGS)  THEN

C  GENERARE "DUMMY" MESSAGES CONTAINING CENTER AND DUMP TIMES
C  ----------------------------------------------------------

C  First message in output file contains only dump center time in Sec 1
C  --------------------------------------------------------------------
 
         CALL OPENMG(LUNOT,SUBSET,LDATE)
         CALL MINIMG(LUNOT,LMINS)

C  Second message in output file contains only current time in Sec 1
C  -----------------------------------------------------------------
 
         CALL OPENMG(LUNOT,SUBSET,MDATE)
         CALL MINIMG(LUNOT,MMINS)
         CALL CLOSMG(LUNOT)
         PRINT 102
  102 FORMAT(/' ==> "Dummy" messages containing dump center time and ',
     $ 'wall-clock processing time successfully written to top of ',
     $ 'output file'/)
      ENDIF

      CALL CLOSBF(LUNIN)
 
      CALL OPENBF(0,'QUIET',1) ! will generate diagnostic print if an
                               ! embedded BUFR table is read

C  WINDOW DATA FROM THE INPUT FILE(S) INTO THE OUTPUT
C  --------------------------------------------------
 
      DO INFILE=1,2
      IF(FILI(INFILE)(1:4).NE.'NONE') THEN
         OPEN(LUNIN,FILE=FILI(INFILE),FORM='UNFORMATTED')
         CALL MESGBC(LUNIN,MSGT,ICOMP)
         IF(ICOMP.EQ.1) THEN
            PRINT'(/"INPUT BUFR FILE",I2," MESSAGES   '//
     .       'C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND IS",I5/)',
     .       INFILE,MSGT
         ELSE  IF(ICOMP.EQ.0) THEN
            PRINT'(/"INPUT BUFR FILE",I2," MESSAGES   '//
     .       'U N C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND IS",'//
     .       'I5/)', INFILE,MSGT
         ELSE IF(ICOMP.EQ.-1)  THEN
            PRINT'(//"ERROR READING INPUT BUFR FILE",I2," - MESSAGE '//
     .       'COMPRESSION UNKNOWN"/)', INFILE
         ELSE  IF(ICOMP.EQ.-3)  THEN
            PRINT'(/"INPUT BUFR FILE",I2," DOES NOT EXIST"/)', INFILE
         ELSE  IF(ICOMP.EQ.-2)  THEN
            PRINT'(/"INPUT BUFR FILE",I2," HAS NO DATA MESSAGES"/'//
     .       '"FIRST MESSAGE TYPE FOUND IS",I5/)', INFILE,MSGT
         ENDIF
CCCCCCCCCCALL OPENBF(LUNIN,'IN',LUNOT) ! this causes an extra set of
                                       !  embedded tables at the top
                                       !  of the input file
         CALL OPENBF(LUNIN,'IN',LUNIN) ! this does not cause the above

C  CALL IREADMG WITH THE NEGATIVE OF THE LOGICAL UNIT NUMBER PASSED IN
C   ARGUMENT 1 - THIS SIGNALS READMG TO TREAT A READ ERROR AS AN END-
C   OF-FILE CONDITION RATHER THAN AS AN ABORT, WHICH READMG WOULD
C   OTHERWISE DO (ALLOWS READMG TO REPLACE OBSOLETE BUFRLIB ROUTINE
C   READFT)
C  -------------------------------------------------------------------

         DO WHILE(IREADMG(-LUNIN,SUBSET,IDATE).EQ.0)
         ITOT = ITOT+1
         IF(IDATE.GE.JDATE .AND. IDATE.LE.KDATE) THEN
            CALL COPYMG(LUNIN,LUNOT)
            ICPY = ICPY+1
         ENDIF
         ENDDO
         CALL CLOSBF(LUNIN)
      ENDIF
      ENDDO
 
      CALL OPENBF(0,'QUIET',0) ! return to default wrt degree of print

C  CLOSE THE OUTPUT FILE AND REPORT ON NORMAL EXIT
C  -----------------------------------------------
 
      PRINT'(/I9," MESSAGES READ IN  FROM DATABASE"//
     .        I9," MESSAGES WINDOWED FROM DATABASE"/)', ITOT,ICPY

      CALL CLOSBF(LUNOT)
      OPEN(LUNOT,FILE=FILO(1:NBYTES_FILO),FORM='UNFORMATTED')
      CALL MESGBC(LUNOT,MSGT,ICOMP)
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
      CLOSE(LUNOT)
      CALL W3TAGE('BUFR_DUMPMD')
      STOP

C  ERROR EXITS
C  -----------
 
  900 CONTINUE

      PRINT *, '#####BUFR_DUMPMD - EOF/ERR READING STDIN'
      CALL W3TAGE('BUFR_DUMPMD')
      CALL ERREXIT(99)

      END

