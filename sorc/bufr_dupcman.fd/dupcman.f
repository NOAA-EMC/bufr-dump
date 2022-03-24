C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_DUPCMAN
C   PRGMMR: DONG             ORG: NP22        DATE: 2021-08-28
C
C ABSTRACT: PROCESSES DUMP FILES CONTAINING LAND-BASED CMAN STATIONS
C   DECODED FROM CMAN FORMAT IN BUFR MESSAGE TYPE 001, SUBTYPES 004 AND
C   104, RESPECTIVELY, PERFORMING A CROSS SUBTYPE DUP-CHECK BETWEEN THE
C   DIFFERENT TYPES. INFORMATION IS READ SEPARATELY FROM EACH FILE THAT
C   IS PRESENT, AND IS THEN COMBINED INTO TABLES USED FOR THE DUP-
C   CHECK.  VARIOUS OPTIONS ARE EXERCISED IN IDENTIFYING DUPLICATES
C   DEPENDING ON THE COMBINATION OF SUBTYPES BEING COMPARED. THE
C   WORKING FILE NAMES OF THE INDIVIDUAL INPUT DUMP FILES (IN EITHER
C   THE "NEW" FORM x_ttt.sss, WHERE ttt IS BUFR TYPE, sss IS BUFR
C   SUBTYPE, AND x IS AN ORDERING INDEX; OR THE "OLD" FORM ttt.sss) ARE
C   READ FROM STANDARD INPUT (UNIT 5) AT THE START OF THIS PROGRAM. THE
C   OUTPUT DUP-CHECKED FILES WILL BE WRITTEN TO THE SAME FILE NAMES.
C   ALL OTHER FILE CONNECTIONS ARE MADE THROUGH THE FORTRAN OPEN
C   STATEMENT.
C
C PROGRAM HISTORY LOG:
C 2021-04-12  J. DONG  --  ADAPTED FROM BUFR_DUPSHP AND MODIFIED TO CHECK
c     DUPLICATIONS FOR C-MAN TAC FORMAT 001.004 AND C-MAN BUFR FORMAT
C     001.104. 
C 2021-08-28  D. STOKES -- MODIFIED IMST DATA STATEMENT TO INITIALIZE
C     ALL 113 ARRAY ELEMENTS
C 2022-03-22  I. GENKOVA -- ADDED CHECK FOR 0 REPORTS IN INPUT FILE AND
C     ALLOWS FOR GRACEFUL CONTINUE IN THE EVENT OF 0 REPORTS.
C     CORRECTLY INITIALIZED IMST.
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT - RECORDS CONTAINING THE WORKING INPUT
C                FILE NAMES FOR ALL SURFACE MARINE AIRCRAFT TYPES
C                EVENTUALLY BEING COMBINED INTO A SINGLE DUMP FILE
C                (usually called "cman") - THE ONLY FILE NAMES
C                CONSIDERED BY THIS PROGRAM ARE *001.004 (TAC C-MAN)
C                AND *001.104 (BUFR CMAN) - OTHER FILES
C                MAY BE INCLUDED HERE, BUT THEY WILL NOT BE MODIFIED BY
C                THIS PROGRAM; THE OUTPUT FILE NAMES WILL BE THE SAME
C                AS THE INPUT NAMES HERE
C     UNIT 20  - UNCHECKED BUFR FILE(S)
C
C   OUTPUT FILES:
C     UNIT 20  - DUPLICATE CHECKED BUFR FILE(S)
C     UNIT 50  - WORKSPACE (SCRATCH) FILE(S)
C     UNIT 60  - STATUS FILE WHOSE PRESENCE INDICATES THIS PROGRAM
C                COMPLETED SUCCESSFULLY
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       W3NCO    - W3TAGB  W3TAGE ERREXIT
C       W3EMC    - ORDERS 
C       BUFRLIB  - DATELEN OPENBF COPYMG UFBTAB OPENMB COPYBF STATUS
C                  COPYSB  CLOSMG CLOSBF NEMTAB MESGBC IBFMS  GETBMISS
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
      PROGRAM BUFR_DUPCMAN
 
      PARAMETER (MXTS=8)
      PARAMETER (NFILES=6)  ! Number of input files being considered
 
      REAL(8),ALLOCATABLE :: TAB_8(:,:)
      REAL(8),ALLOCATABLE :: RAB_8(:,:)
      INTEGER,ALLOCATABLE :: IWORK(:)
      INTEGER,ALLOCATABLE :: IORD(:)
      INTEGER,ALLOCATABLE :: JDUP(:)

      CHARACTER*80  TSTR,TSTH,RSTR,FILE,FILI(NFILES),FILO
      CHARACTER*8   SUBSET,crpidI,crpidJ
      CHARACTER*3   DUMMY_MSGS

      DIMENSION     NDUP(0:2),IPTR(2,NFILES),IMST(113)

      REAL(8)       UFBTAB_8,rpidI,rpidJ,BMISS,GETBMISS

      LOGICAL       DUPES,CMAN

      EQUIVALENCE   (crpidI,rpidI),(crpidJ,rpidJ)

      DATA TSTR  /'CLAT  CLON  MNTH DAYS HOUR MINU RPID '/
      DATA TSTH  /'CLATH CLONH MNTH DAYS HOUR MINU RPID '/
      DATA RSTR  /'RCYR  RCMO  RCDY RCHR RCMI           '/

      DATA DEXY  /0.005/
      DATA DMON  /0/
      DATA DDAY  /0/
      DATA DOUR  /0/
      DATA DMIN  /0/

      DATA IMST  /  3*-99, 1, 99*-99, 2, 9*-99 /
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_DUPCMAN',2022,0081,2250,'NP22')

      print *
      print * ,'---> Welcome to BUFR_DUPCMAN - Version 03-22-2022'
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

      NDUP = 0
      LUBFI = 20
      LUBFJ = 50

C  STORE THE FILENAMES TO PROCESS FROM STANDARD INPUT
C  --------------------------------------------------
 
      FILI(1:NFILES)(1:4) = 'NONE'

      CMAN = .FALSE.
 
1     CONTINUE

      READ(5,'(A80)',END=2) FILE
      DO  I=1,10
         IF(FILE(I:I+3).EQ.'001.') THEN
            READ(FILE(I+4:I+6),'(I3)') MST
            IF(MST.EQ.4.OR.MST.EQ.104) THEN
               FILI(IMST(MST)) = FILE
               CMAN = .TRUE.
               PRINT *, ' >> WILL CHECK ',FILE(I:I+6)
            ENDIF
            EXIT
         ENDIF
      ENDDO
      GOTO 1

2     CONTINUE
      IF(.NOT.CMAN) THEN
         PRINT*
         PRINT*,'BUFR_DUPCMAN: NO CMAN TO CHECK'
         PRINT*
         CALL W3TAGE('BUFR_DUPCMAN')
         STOP
      ELSE
         PRINT 200, DEXY,DDAY,DOUR
  200 FORMAT(/'BUFR_DUPCMAN PARAMETERS:'/
     .        3X,'TOLERANCE FOR LAT/LON CHECKS (IN DEGREES) .. ',F7.3/
     .        3X,'TOLERANCE FOR DAY CHECK (IN DAYS) .......... ',F7.3/
     .        3X,'TOLERANCE FOR HOUR CHECK (IN HOURS) ........ ',F7.3/)
      ENDIF
 
C  COUNT THE NUMBER OF SUBSETS AMONGST ALL FILES TO ALLOCATE SPACE
C  ---------------------------------------------------------------

      MXTB = 0
      DO I=1,NFILES
         IF(FILI(I)(1:4).EQ.'NONE') CYCLE
         CALL CLOSBF(LUBFI)
         OPEN(LUBFI,FILE=FILI(I),FORM='UNFORMATTED')
         CALL OPENBF(0,'QUIET',1) ! will generate diagnostic print if
                                  ! an embedded BUFR table is read
         CALL UFBTAB(-LUBFI,UFBTAB_8,1,1,NUM_SUBSETS,' ')
         CALL OPENBF(0,'QUIET',0) ! return to default wrt degree of prnt

         MXTB = MXTB + NUM_SUBSETS
      ENDDO

      IF(MXTB.EQ.0) THEN
         PRINT *
         PRINT *, '### WARNING: A total of ZERO input cman reports'
         PRINT *
         GO TO 400
      ENDIF

      ALLOCATE(TAB_8(MXTS,MXTB),STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(RAB_8(MXTS,MXTB),STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(IWORK(MXTB)     ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(IORD(MXTB)      ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(JDUP(MXTB)      ,STAT=I);IF(I.NE.0) GOTO 901

      TAB_8 = BMISS
      RAB_8 = BMISS
      JDUP  = 0
      IORD  = 0

C  MAKE A TABLE OUT OF THE LATS, LONS, HEIGHTS, AND TIME COORDINATES
C  -----------------------------------------------------------------
 
      IPTR = 0
      IPT  = 1

      DO I=1,NFILES
         IF(FILI(I)(1:4).NE.'NONE') THEN
            CALL CLOSBF(LUBFI)
            OPEN(LUBFI,FILE=FILI(I),FORM='UNFORMATTED')

            CALL MESGBC(LUBFI,MSGT,ICOMP)
            IF(ICOMP.EQ.1) THEN
               PRINT'(/"INPUT BUFR FILE",I2," MESSAGES   '//
     .          'C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND IS",'//
     .          'I5/)', I,MSGT
               PRINT'("#####BUFR_DUPCMAN (UFBTAB) CANNOT PROCESS '//
     .            'COMPRESSED BUFR MESSAGES -- FATAL ERROR")'
               CALL W3TAGE('BUFR_DUPCMAN')
               CALL ERREXIT(99)
            ELSE  IF(ICOMP.EQ.0) THEN
               PRINT'(/"INPUT BUFR FILE",I2," MESSAGES   '//
     .          'U N C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND '//
     .          'IS",I5/)', I,MSGT
            ELSE IF(ICOMP.EQ.-1) THEN
               PRINT'(//"ERROR READING INPUT BUFR FILE",I2," - '//
     .          'MESSAGE COMPRESSION UNKNOWN"/)', I
            ELSE  IF(ICOMP.EQ.-3) THEN
               PRINT'(/"INPUT BUFR FILE",I2," DOES NOT EXIST"/)', I
            ELSE  IF(ICOMP.EQ.-2) THEN
               PRINT'(/"INPUT BUFR FILE",I2," HAS NO DATA MESSAGES"/'//
     .          '"FIRST MESSAGE TYPE FOUND IS",I5/)', I,MSGT
            ENDIF
            CALL UFBTAB(LUBFI,TAB_8(1,IPT),MXTS,MXTB-IPT+1,NTAB,TSTR)
            IF(IBFMS(TAB_8(2,IPT)).EQ.1) THEN             ! CLON missing
               OPEN(LUBFI,FILE=FILI(I),FORM='UNFORMATTED')
               CALL UFBTAB(LUBFI,TAB_8(1,IPT),MXTS,MXTB-IPT+1,NTAB,TSTH)
            ENDIF
            OPEN(LUBFI,FILE=FILI(I),FORM='UNFORMATTED')
            CALL UFBTAB(LUBFI,RAB_8(1,IPT),MXTS,MXTB-IPT+1,NTAB,RSTR)
            IPTR(1,I) = IPT
            IPTR(2,I) = IPT+NTAB-1
            IPT = IPT+NTAB
         ENDIF
      ENDDO

      NTAB = IPT-1

C  SET MISSING MINU TO ZERO; STORE INPUT FILE INDEX
C  ------------------------------------------------

      DO N=1,NTAB
         IF(IBFMS(TAB_8(6,N)).EQ.1) TAB_8(6,N) = 0     ! data missing

         ! Store input file index in 8th index
         ! TAB_8(8,N) = 1  --> b001/xx004 (CMAN/TAC)
         !            = 2  --> b001/xx104 (CMAN/BUFR)

         DO I=1,NFILES
            IF(N.GE.IPTR(1,I) .AND. N.LE.IPTR(2,I)) THEN
               TAB_8(8,N) = I
               EXIT
            ENDIF
         ENDDO
      ENDDO
 
C  GET A SORTED INDEX OF THE REPORTS BY RECEIPT, OBS TIME, AND LON/LAT
C  -------------------------------------------------------------------

      CALL ORDERS( 2,IWORK,RAB_8(5,1),IORD,NTAB,MXTS,8,2) ! rcpt. minute
      CALL ORDERS(12,IWORK,RAB_8(4,1),IORD,NTAB,MXTS,8,2) ! rcpt. hour
      CALL ORDERS(12,IWORK,RAB_8(3,1),IORD,NTAB,MXTS,8,2) ! rcpt. day
      CALL ORDERS(12,IWORK,RAB_8(2,1),IORD,NTAB,MXTS,8,2) ! rcpt. month
      CALL ORDERS(12,IWORK,RAB_8(1,1),IORD,NTAB,MXTS,8,2) ! rcpt. year
      CALL ORDERS(12,IWORK,TAB_8(2,1),IORD,NTAB,MXTS,8,2) ! longitude
      CALL ORDERS(12,IWORK,TAB_8(1,1),IORD,NTAB,MXTS,8,2) ! latitude
      CALL ORDERS(10,IWORK,TAB_8(7,1),IORD,NTAB,MXTS,8,2) ! report id
      CALL ORDERS(12,IWORK,TAB_8(6,1),IORD,NTAB,MXTS,8,2) ! minute
      CALL ORDERS(12,IWORK,TAB_8(5,1),IORD,NTAB,MXTS,8,2) ! hour
      CALL ORDERS(12,IWORK,TAB_8(4,1),IORD,NTAB,MXTS,8,2) ! day
      CALL ORDERS(12,IWORK,TAB_8(3,1),IORD,NTAB,MXTS,8,2) ! month

C  GO THROUGH THE REPORTS IN ORDER, MARKING DUPLICATES
C  ---------------------------------------------------

      LOOP1: DO K=1,NTAB-1
         I = IORD(K)
         J = IORD(K+1)
         rpidI = TAB_8(7,I)
         rpidJ = TAB_8(7,J)

         if(jdup(i).ne.0) then
            print *, '---> this report in record (I) has already been ',
     .       'flagged as a duplicate - cycle'
            cycle LOOP1
         endif

         DUPES =
     . NINT(ABS(TAB_8(1,I)-TAB_8(1,J))*100).LE.NINT(DEXY*100)
     . .AND.
     . NINT(ABS(TAB_8(2,I)-TAB_8(2,J))*100).LE.NINT(DEXY*100)
     . .AND.
     . NINT(ABS(TAB_8(3,I)-TAB_8(3,J))*100.).LE.NINT(DMON*100.)
     . .AND.
     . NINT(ABS(TAB_8(4,I)-TAB_8(4,J))*100.).LE.NINT(DDAY*100.)
     . .AND.
     . NINT(ABS(TAB_8(5,I)-TAB_8(5,J))*100.).LE.NINT(DOUR*100.)
     . .AND.
     . NINT(ABS(TAB_8(6,I)-TAB_8(6,J))*100.).LE.NINT(DMIN*100.)
     . .AND.
     . (crpidI.EQ.crpidJ .OR. (crpidI .EQ. 'SHIP    ' .OR.
     . crpidJ .EQ. 'SHIP    '))
     . .AND.
     . NINT(TAB_8(8,I)).NE.NINT(TAB_8(8,J))
         IF(DUPES) THEN
cc               print 1799,I,crpidI,(tab_8(ii,I),ii=1,2),
cc     .          (nint(tab_8(ii,I)),ii=4,6),
cc     .          (nint(rab_8(ii,I)),ii=1,5),nint(tab_8(8,I))
cc 1799          format('===> DUPL. FOUND:'/'   --> I: ',I6,';{ ID: ',A8,
cc     .          '; LAT: ',F9.5,'; LON: ',F10.5,'; RPRT DD HH:mm ',
cc     .          I2.2,I2,':',I2,'; RCPT YYYYMMDDHHMM: ',I4,4I2.2,
cc     .          '; file #',I2,'}' )
cc               print 1800, J,crpidJ,(tab_8(ii,J),ii=1,2),
cc     .          (nint(tab_8(ii,J)),ii=4,6),
cc     .          (nint(rab_8(ii,J)),ii=1,5),nint(tab_8(8,J))
cc 1800          format('   --> J: ',I6,';{ ID: ',A8,'; LAT: ',F9.5,
cc     .          '; LON: ',F10.5,'; RPRT',' DD HH:mm ',I2.2,I2,':',I2,
cc     .          '; RCPT YYYYMMDDHHMM: ',I4,4I2.2,
cc     .          '; file #',I2,'}' )
CDONG            JDUP_I_ORIG = JDUP(I)
            IF(TAB_8(8,I).EQ.1 .AND. TAB_8(8,J).EQ.2) then
               JDUP(I) = 1
               print *, 'I tossed ==> from either receipt time '
     .                 ,'or less-preferred file-type'
            ELSE
               JDUP(J) = 1
               print *, 'J tossed ==> from either receipt time '
     .                 ,'or less-preferred file-type'
            ENDIF
         ENDIF
cpppppppppp
cc       if(dupes) then
cc       if(jdup(irec).eq.1) then
cc    print *,'this one'
cc    print *,'LATITUDE: tossed, saved : ',TAB_8(1,I),TAB_8(1,J)
cc    print *,'LONGITUDE: tossed, saved: ',TAB_8(2,I),TAB_8(2,J)
cc    print *,'MONTH: tossed, saved    : ',TAB_8(3,I),TAB_8(3,J)
cc    print *,'DAY: tossed, saved      : ',TAB_8(4,I),TAB_8(4,J)
cc    print *,'HOUR: tossed, saved     : ',TAB_8(5,I),TAB_8(5,J)
cc    print *,'MINUTE: tossed, saved   : ',TAB_8(6,I),TAB_8(6,J)
cc    print *,'STNID: tossed, saved    : ',crpidI,crpidJ
cc    print *,'FILE: tossed, saved     : ',TAB_8(8,I),TAB_8(8,J)
cc       endif
cc       if(jdup(jrec).eq.1) then
cc    print *,'next one'
cc    print *,'LATITUDE: tossed, saved : ',TAB_8(1,J),TAB_8(1,I)
cc    print *,'LONGITUDE: tossed, saved: ',TAB_8(2,J),TAB_8(2,I)
cc    print *,'MONTH: tossed, saved    : ',TAB_8(3,J),TAB_8(3,I)
cc    print *,'DAY: tossed, saved      : ',TAB_8(4,J),TAB_8(4,I)
cc    print *,'HOUR: tossed, saved     : ',TAB_8(5,J),TAB_8(5,I)
cc    print *,'MINUTE: tossed, saved   : ',TAB_8(6,J),TAB_8(6,I)
cc    print *,'STNID: tossed, saved    : ',crpidJ,crpidI
cc    print *,'FILE: tossed, saved     : ',TAB_8(8,J),TAB_8(8,I)
cc       endif
cc       endif
cpppppppppp
      ENDDO LOOP1
 
C  WRITE BACK THE DUP-CHECKED FILE(S)
C  ----------------------------------
 
      CALL GETENV('DUMMY_MSGS',DUMMY_MSGS)

      DO I=1,NFILES
         IF(FILI(I)(1:4).NE.'NONE') THEN
cpppppppppp
cc          idate_last = -9999
cpppppppppp
            FILO = '.'//FILI(I)
            ISUB = IPTR(1,I)-1
            CALL CLOSBF(LUBFI)
            CALL CLOSBF(LUBFJ)
            OPEN(LUBFI,FILE=FILI(I),FORM='UNFORMATTED')
            OPEN(LUBFJ,FILE=FILO   ,FORM='UNFORMATTED')
            CALL OPENBF(LUBFI,'IN ',LUBFI)
            CALL OPENBF(LUBFJ,'OUT',LUBFI)
 
C  If input file doesn't contain dummy center and dump time messages 1
C   and 2 (after table messages), before doing anything call closmg
C   with a negative unit number to signal routine that it should not
C   write out ANY messages with zero subsets in them - this holds for
C   all subsequent calls to closmg in this routine, even those done
C   through other bufrlib routines (and even for those calls where the
C   sign of the unit number is positive)
C  --------------------------------------------------------------------

            IF(DUMMY_MSGS.NE.'YES') CALL CLOSMG(-LUBFJ)

            DO WHILE(IREADMG(LUBFI,SUBSET,IDATE).EQ.0)
               NSUBS = NMSUB(LUBFI)
cpppppppppp
cc             print *, 'New message read in , NSUBS, IDATE = ',nsubs,
cc   .          idate
cpppppppppp

C  If no subsets in msg & dummy msgs not expected loop to next input msg
C  ---------------------------------------------------------------------

               IF(NSUBS.LE.0.AND.DUMMY_MSGS.NE.'YES')  CYCLE

               DUPES = .FALSE.

               IF(NSUBS.GT.0)  THEN
                  DO N=1,NSUBS
                     IF(ISUB+N.GT.NTAB) THEN
                        IDUP = 2
                     ELSE
                        IDUP = JDUP(ISUB+N)
                     ENDIF
                     NDUP(IDUP) = NDUP(IDUP)+1
                     IF(IDUP.GT.0) DUPES = .TRUE.
                  ENDDO
               ENDIF

cpppppppppp
cc             print *, 'DUPES = ',dupes
cpppppppppp

               IF(DUPES) THEN
                  CALL OPENMB(LUBFJ,SUBSET,IDATE)
cpppppppppp
cc                if(idate.ne.idate_last)
cc   .             print *, 'NEW MESSAGE OPENED'
cc                idate_last = idate
cpppppppppp
                  DO WHILE(IFBGET(LUBFI).EQ.0)
                     ISUB = ISUB+1
                     IF(ISUB.GT.NTAB) THEN
                        IDUP = 2
                     ELSE
                        IDUP = JDUP(ISUB)
                     ENDIF
                     IF(IDUP.EQ.0) THEN
                        CALL COPYSB(LUBFI,LUBFJ,IRET) ! Copy non-dups
                     ELSE
                        CALL COPYSB(LUBFI,00000,IRET) ! Skip dups
                     ENDIF
                  ENDDO
               ELSE

C  In the event that the input file contains dummy center and dump time
C    messages 1 and 2 (after table messages), call closmg with a
C    positive unit number to signal routine that it should write out
C    these messages even though they have zero subsets in them
C  If the input file does not contain dummy messages, a positive unit
C    number here is immaterial because closmg was already called with
C    a negative unit number immediately after the output file was
C    opened (and this holds for all subsequent calls to closmg
C    regardless of the sign of the unit number)
C  -------------------------------------------------------------------

                  CALL CLOSMG(LUBFJ)
                  CALL COPYMG(LUBFI,LUBFJ)
                  ISUB = ISUB+NSUBS
               ENDIF
            ENDDO

            CALL CLOSBF(LUBFI)
            CALL CLOSBF(LUBFJ)
            OPEN(LUBFI,FILE=FILI(I),FORM='UNFORMATTED')
            OPEN(LUBFJ,FILE=FILO   ,FORM='UNFORMATTED')
            CALL COPYBF(LUBFJ,LUBFI)
            OPEN(LUBFI,FILE=FILI(I),FORM='UNFORMATTED')

            CALL MESGBC(LUBFI,MSGT,ICOMP)
            IF(ICOMP.EQ.1) THEN
               PRINT'(/"OUTPUT BUFR FILE",I2," MESSAGES   '//
     .          'C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND IS",'//
     .          'I5/)', I,MSGT
            ELSE  IF(ICOMP.EQ.0) THEN
               PRINT'(/"OUTPUT BUFR FILE",I2," MESSAGES   '//
     .          'U N C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND '//
     .          'IS",I5/)', I,MSGT
            ELSE IF(ICOMP.EQ.-1) THEN
               PRINT'(//"ERROR READING OUTPUT BUFR FILE",I2," - '//
     .          'MESSAGE COMPRESSION UNKNOWN"/)', I
            ELSE  IF(ICOMP.EQ.-3) THEN
               PRINT'(/"OUTPUT BUFR FILE",I2," DOES NOT EXIST"/)', I
            ELSE  IF(ICOMP.EQ.-2) THEN
               PRINT'(/"OUTPUT BUFR FILE",I2," HAS NO DATA MESSAGES"/'//
     .          '"FIRST MESSAGE TYPE FOUND IS",I5/)', I,MSGT
            ENDIF

            CLOSE(LUBFI)
         ENDIF
      ENDDO

 400  CONTINUE
 
C  GENERATE REPORT
C  ---------------
 
      PRINT 300, ISUB,NDUP(2),MXTB,NTAB,NDUP(0),NDUP(1)
  300 FORMAT(/'BUFR_DUPCMAN READ IN A TOTAL OF',I8,' REPORTS'/
     .        ' A TOTAL OF ',I7,' REPORTS WERE SKIPPED DUE TO BEING ',
     .        'OVER THE LIMIT OF ',I7/
     .        'BUFR_DUPCMAN CHECKED A TOTAL OF',I8,' REPORTS'//
     .        'NUMBER OF UNIQUE REPORTS WRITTEN OUT ...........',I7/
     .        'NUMBER OF REPORTS SKIPPED DUE TO:'/
     .        '   FAILING DUPLICATE CHECK .....................',I7/)

C  END OF PROGRAM
C  --------------

      WRITE(60,'("ALL DONE")')
      CALL W3TAGE('BUFR_DUPCMAN')
      STOP

C  ERROR EXITS
C  -----------

  901 CONTINUE

      PRINT *, '#####BUFR_DUPCMAN - UNABLE TO ALLOCATE ARRAYS'
      CALL W3TAGE('BUFR_DUPCMAN')
      CALL ERREXIT(99)

      END
