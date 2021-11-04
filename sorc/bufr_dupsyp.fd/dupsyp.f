C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_DUPSYP
C   PRGMMR: MELCHIOR/DONG    ORG: NP22        DATE: 2020-08-20
C
C ABSTRACT: PROCESSES ANY COMBINATION UP TO SIX DUMP FILES CONTAINING
C   TYPES THAT ULTIMATELY GO  TO THE "ADPSFC" DUMP {BUFR MESSAGE
C   TYPE 000, SUBTYPES 000 (SYNOPR), 001 (SYNOP), 002 (SYNOPM),
C   100 (SYNPBR, BUFR), 101 (SYNOPB, BUFR), 102 (SYNOMB, BUFR)}.
C   PERFORMS A SINGLE DUP-CHECK FOR REPORTS ACROSS ALL APPLICABLE
C   FILES GOING INTO THE "ADPSFC" DUMP.  DOES NOT INCLUDE METAR IN
C   SUBTYPE 007 WHICH GO INTO "ADPSFC" DUMP.  INFORMATION IS READ
C   SEPARATELY FROM EACH FILE THAT IS PRESENT, AND IS THEN
C   COMBINED INTO TABLES USED FOR THE DUP-CHECK.  THE ALGORITHM
C   SORTS THE REPORTS IN ASCENDING ORDER OF LAT, LON, OBS TIME
C   (DAY DOWN TO SECOND), HEIGHT(??), AND RECEIPT TIME (YEAR DOWN
C   TO MINUTE).  IN THE DUPLICATE CHECKING LOGIC, ADJACENT, SORTED
C   REPORT PAIRS ARE CHECKED FOR LAT, LON, AND OBS TIME (TO THE
C   SECOND), ALL BASED ON TOLERANCE LIMITS WHICH CAN VARY BASED ON
C   WHETHER THIS IS A REAL-TIME OR HISTORICAL RUN.  THE REPORT 
C   USUALLY SELECTED IS THE BULLETIN LAST RECEIVED, HOWEVER IF ONE
C   REPORT IN THE PAIR IS IN TAC FORMAT, THE BUFR FORMAT (NON-TAC)
C   IS ALWAYS SELECTED.  THERE ARE OTHER RULES FOR SELECTING THE
C   "BEST" REPORT IN THE DUPLICATE PAIR (SEE CODE).  THE WORKING
C   FILE NAMES OF THE INPUT DUMP FILES (IN EITHER THE "NEW" FORM
C   x_ttt.sss, WHERE ttt IS BUFR TYPE, sss IS BUFR SUBTYPE, AND x IS AN
C   ORDERING INDEX; OR THE "OLD" FORM ttt.sss) ARE READ FROM STANDARD
C   INPUT (UNIT 5) AT THE START OF THIS PROGRAM. THE OUTPUT DUP-CHECKED
C   FILES WILL BE WRITTEN TO THE SAME FILE NAMES.  ALL OTHER FILE
C   CONNECTIONS ARE MADE THROUGH THE FORTRAN OPEN STATEMENT.
C
C PROGRAM HISTORY LOG:
C 2020-08-20  S. MELCHIOR  ORIGINAL VERSION FOR IMPLEMENTATION
C 2020-08-20  J. DONG  --
C              -  ADD A TEMPORARY SOLUTION TO KEEP TAC REPOERTS
C                 IF THE STATION HEIGHT IS MISSING IN THE BUFR REPORTS. 
C              -  ADD A TEMPORARY SOLUTION TO KEEP TAC REPOERTS
C                 IF THE PMSL IS MISSING IN THE BUFR REPORTS.
C              -  PUT THE LAT/LON SORT PRIORITY BEHIND THE
C                 OBSERVATIONAL TIME AND RPID FOR CONSIDERING THE SLIGHT
C                 LAT/LON DIFFERENCE BETWEEN TAC AND BUFR FORMAT REPORTS
C                 IN THE DUPLICATION CHECKS.
C              -  ADDED SETBMISS CALL TO SET BMISS TO 10E8 AND
C                 CHANGE THE CODE TO AVOID INTEGER OVERFLOW
C 2021-04-20  X. Su
C               -  adding statement for station with missing values
C 2021-04-20  J. DONG -- 
C              -  CHANGED DEXY TO 0.005.
C              -  REMOVED THE UNNESSARY CODES
C              -  FIXED THE BUGS FOR KEEPING THE REPORTS WITH
C                 DIFFERENT IDs BUT WITH CLOSE LAT/LON.
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT - RECORDS CONTAINING THE WORKING INPUT
C                FILE NAMES FOR ALL SYNOPTIC TYPES EVENTUALLY BEING
C                COMBINED INTO A SINGLE DUMP FILE (for "adpsfc" dumps)
C                - THE ONLY FILE NAMES CONSIDERED BY THIS PROGRAM ARE
C                *000.000 (SYNOPR), *000.001 (SYNOP),
C                *000.002 (SYNOPM), *000.100 (SYNPBR, BUFR),
C                *000.101 (SYNOPB, BUFR), *000.102 (SYNPMB, BUFR)
C                (all included in the "adpsfc" dump)
C                - OTHER FILES MAY BE INCLUDED HERE, BUT THEY WILL
C                NOT BE MODIFIED BY THIS PROGRAM; THE OUTPUT FILE
C                NAMES WILL BE THE SAME AS THE INPUT NAMES HERE.
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
C       W3NCO    - W3TAGB  W3TAGE ERREXIT IW3JDN
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
      PROGRAM BUFR_DUPSYP
 
      PARAMETER (MXTS=10)
      PARAMETER (NFILES=6)  ! Number of input files being considered
 
      REAL(8),ALLOCATABLE :: TAB_8(:,:)
      real(8),allocatable :: rab_8(:,:)
      INTEGER,ALLOCATABLE :: IWORK(:)
      INTEGER,ALLOCATABLE :: IORD(:)
      INTEGER,ALLOCATABLE :: JDUP(:),JJDUP(:)

      CHARACTER*80 TSTR,TSTH,rstr,FILE,FILI(NFILES),FILO
      CHARACTER*8  SUBSET,CTAB7,ctab7_i,ctab7_j,c_missing
      character*20 ctext_file(nfiles)
      CHARACTER*3  DUMMY_MSGS

      REAL(8) UFBTAB_8,TAB7_8,BMISS,GETBMISS,tab7_i_8,tab7_j_8

      DIMENSION    IMST(0:102),ntab_file(nfiles)
      DIMENSION    NDUP(0:2),IPTR(2,NFILES),ndup_file(0:2,nfiles)

      LOGICAL      DUPES,SYNOP

      EQUIVALENCE  (TAB7_8,CTAB7)
      equivalence  (tab7_i_8,ctab7_i),(tab7_j_8,ctab7_j),
     $ (c_missing,i_missing)

      DATA TSTR /'CLAT  CLON  MNTH DAYS HOUR MINU RPID SELV  PMSL'/
      DATA TSTH /'CLATH CLONH MNTH DAYS HOUR MINU RPID HSMSL PMSL'/
      data rstr /'RCYR RCMO  RCDY  RCHR RCMI           '/

      data ctext_file/
     $           'SYNOPR format       ',
     $           'SYNOP format        ',
     $           'SYNOPM format       ',
     $           'SYNPBR/BUFR         ',
     $           'SYNOPB/BUFR         ',
     $           'SYNPMB/BUFR         '/

      data c_missing/'missing-'/

C  Tolerance parameters for all reports:
C  (exact for the moment, but lat/lon and time might need some
C  tolearnce)
C  -------------------------------------

      data dexy    / 0.005/ ! lat/lon
      data dmon    / 0.0/ ! month
      DATA DDAY    / 0.0/ ! day
      data dour    / 0.0/ ! hour
      data dmin    / 0.0/ ! minute

C     IMST (index for message subtype) - must start at 0 rather than
C     default of 1 because first eligible synop tank is b000/xx000.
      DATA IMST  /   1,   2,   3,   97*-99,   4,   5,  6/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_DUPSYP',2021,0175,0054,'NP22')

      print *
      print * ,'---> Welcome to BUFR_DUPSYP - Version 06-24-2021'
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

      LUBFI = 20
      LUBFJ = 50

C  STORE THE FILENAMES TO PROCESS FROM STANDARD INPUT
C  --------------------------------------------------
 
      FILI(1:NFILES)(1:4) = 'NONE'

      SYNOP = .FALSE.
 
1     CONTINUE

      READ(5,'(A80)',END=2) FILE
      DO  I=1,10
         IF(FILE(I:I+3).EQ.'000.') THEN
            READ(FILE(I+4:I+6),'(I3)') MST
            IF(MST.LE.2.OR.MST.EQ.100.or.mst.eq.101.OR.
     .         MST.EQ.102) THEN
               FILI(IMST(MST)) = FILE
               SYNOP = .TRUE.
               PRINT *, ' >> WILL CHECK ',FILE(I:I+6)
            ENDIF
            EXIT
         ENDIF
      ENDDO
      GOTO 1

2     CONTINUE
      IF(.NOT.SYNOP) THEN
         PRINT *
         PRINT *,'BUFR_DUPSYP: NO SYNOPTIC REPORTS TO CHECK'
         PRINT *
         CALL W3TAGE('BUFR_DUPSYP')
         STOP
      ELSE
         print 200, dexy,dday,dour
  200 FORMAT(/'BUFR_DUPSYP PARAMETERS:'/
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

      ISUB = 0
      NTAB = 0

      IF(MXTB.EQ.0) THEN
         PRINT *
         PRINT *, '### WARNING: A total of ZERO input synoptic reports'
         PRINT *
         GO TO 400
      ENDIF

      ALLOCATE(TAB_8(MXTS,MXTB),STAT=I);IF(I.NE.0) GOTO 901
      allocate(rab_8(mxts,mxtb),stat=i);if(i.ne.0) goto 901
      ALLOCATE(IWORK(MXTB)     ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(IORD(MXTB)      ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(JDUP(MXTB)      ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(JJDUP(MXTB)     ,STAT=I);IF(I.NE.0) GOTO 901

      TAB_8 = BMISS
      rab_8 = bmiss

C  MAKE A TABLE OUT OF THE LATS, LONS, AND TIME COORDINATES
C  --------------------------------------------------------
 
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
               PRINT'("#####BUFR_DUPSYP (UFBTAB) CANNOT PROCESS '//
     .            'COMPRESSED BUFR MESSAGES -- FATAL ERROR")'
               CALL W3TAGE('BUFR_DUPSYP')
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
            call ufbtab(lubfi,rab_8(1,ipt),mxts,mxtb-ipt+1,ntab,rstr)
            IPTR(1,I) = IPT
            IPTR(2,I) = IPT+NTAB-1
            IPT = IPT+NTAB
         ENDIF
      ENDDO

      NTAB = IPT-1

C  The arrays containing receipt time coordinates are set to negative
C   values so that the key on them in sort routine ORDERS below will
C   order them from latest actual receipt time to earliest - this is
C   needed because logic is designed to normally toss the second report
C   in a duplicate pair and we want to retain the one with the latest
C   receipt time.
C  --------------------------------------------------------------------

      rab_8 = -rab_8

C  INITIAL VALUES FOR MARKERS AND COUNTERS
C  ---------------------------------------
 
      JDUP = 0
      JJDUP = 0
      IORD = 0
      NDUP = 0
      ndup_file = 0
      ntab_file = 0

C SET MISSING MINU TO ZERO; STORE INPUT FILE INDEX
C ------------------------------------------------

      DO N=1,NTAB

         IF(IBFMS(TAB_8(6,N)).EQ.1) then
            TAB_8(6,N) = 0    ! data missing
         endif

         ! Store input file index in 8th index 
         ! TAB_8(10,N) = 1  --> b000/xx000 (SYNOPR)
         !             = 2  --> b000/xx001 (SYNOP)
         !             = 3  --> b000/xx002 (SYNOPM)
         !             = 4  --> b000/xx100 (SYNPBR/BUFR)
         !             = 5  --> b000/xx101 (SYNOPB/BUFR)
         !             = 6  --> b000/xx102 (SYNPMB/BUFR)

         DO I=1,NFILES
            IF(N.GE.IPTR(1,I) .AND. N.LE.IPTR(2,I)) THEN
               TAB_8(10,N) = I
               ntab_file(i) =  ntab_file(i) + 1
               EXIT
            ENDIF
         ENDDO

      ENDDO ! end DO N=1,NTAB
 
C  GET A SORTED INDEX OF THE REPORTS KEYED IN THIS ORDER:
C  RECEIPT TIME, LAT/LON, RPID, OBS TIME
C  ----------------------------------------------------------------
 
      call orders( 2,iwork,rab_8(5,1),iord,ntab,mxts,8,2) ! rcpt minute
      call orders(12,iwork,rab_8(4,1),iord,ntab,mxts,8,2) ! rcpt hour
      call orders(12,iwork,rab_8(3,1),iord,ntab,mxts,8,2) ! rcpt day
      call orders(12,iwork,rab_8(2,1),iord,ntab,mxts,8,2) ! rcpt month
      CALL ORDERS(12,IWORK,RAB_8(1,1),IORD,NTAB,MXTS,8,2) ! rcpt year
      CALL ORDERS(12,IWORK,TAB_8(2,1),IORD,NTAB,MXTS,8,2) ! lon
      CALL ORDERS(12,IWORK,TAB_8(1,1),IORD,NTAB,MXTS,8,2) ! lat
      CALL ORDERS(10,IWORK,TAB_8(7,1),IORD,NTAB,MXTS,8,2) ! rpid
      CALL ORDERS(12,IWORK,TAB_8(6,1),IORD,NTAB,MXTS,8,2) ! minute 
      CALL ORDERS(12,IWORK,TAB_8(5,1),IORD,NTAB,MXTS,8,2) ! hour 
      CALL ORDERS(12,IWORK,TAB_8(4,1),IORD,NTAB,MXTS,8,2) ! day
      CALL ORDERS(12,IWORK,TAB_8(3,1),IORD,NTAB,MXTS,8,2) ! month
 
C  GO THROUGH THE REPORTS IN ORDER, MARKING DUPLICATES
C  ---------------------------------------------------
 
      LOOP1: DO L=1,NTAB-1
         K = L+1
         I = IORD(L)
         J = IORD(L+1)
         tab7_i_8 = tab_8(7,i)  ! rpid
         tab7_j_8 = tab_8(7,j)  ! rpid
         if(jdup(i).ne.0) then
            print *, '---> this report in record (I) has already been ',
     .       'flagged as a duplicate - cycle'
            cycle LOOP1
         endif

         DO WHILE(JDUP(I).EQ.0)
            if(jdup(j).ne.0) then
               print *, '---> this report in record (J) has already ',
     .          'been flagged as a duplicate - cycle'
               go to 800
            endif
            dell = dexy
            JJDUP(j)=0
               if(ctab7_i .eq. ctab7_j  ) then   
                  if(NINT(ABS(TAB_8(3,I)-TAB_8(3,J))*100.).LE.
     .               NINT(DMON*100.).AND.
     .               NINT(ABS(TAB_8(4,I)-TAB_8(4,J))*100.).LE.
     .               NINT(DDAY*100.).AND.
     .               NINT(ABS(TAB_8(5,I)-TAB_8(5,J))*100.).LE.
     .               NINT(DOUR*100.).AND.
     .               NINT(ABS(TAB_8(6,I)-TAB_8(6,J))*100.).LE.
     .               NINT(DMIN*100.)) then
cc                   print 1799, i,ctab7_i,(tab_8(ii,i),ii=1,2),
cc     .              (nint(tab_8(ii,i)),ii=4,6),
cc     .              (nint(-rab_8(ii,i)),ii=1,5),nint(tab_8(10,i))
cc     .              ,tab_8(8,i),tab_8(9,i)
cc 1799             format('===> DUPL. FOUND:'/'   --> I: ',I6,';{ ID: ',A8,
cc     .             '; LAT: ',F9.5,'; LON: ',F10.5,'; RPRT DD HH:mm ',
cc     .             I2.2,I2,':',I2,'; RCPT YYYYMMDDHHMM: ',I4,4I2.2,
cc     .             '; file #',I2,'} ELEV=',F15.2,'PMSL=',F15.2)
cc                  print 1800, j,ctab7_j,(tab_8(ii,j),ii=1,2),
cc     .             (nint(tab_8(ii,j)),ii=4,6),
cc     .             (nint(-rab_8(ii,j)),ii=1,5),nint(tab_8(10,j))
cc     .             ,tab_8(8,j),tab_8(9,j)
cc 1800             format('   --> J: ',I6,';{ ID: ',A8,'; LAT: ',F9.5,
cc     .             '; LON: ',F10.5,'; RPRT',' DD HH:mm ',I2.2,I2,':',I2,
cc     .             '; RCPT YYYYMMDDHHMM: ',I4,4I2.2,
cc     .             '; file #',I2,'} ELEV=',F15.2,'PMSL=',F15.2)

cc                  print *,'lat diff =',ABS(TAB_8(1,I)-TAB_8(1,J))
cc                  print *,'lon diff =',ABS(TAB_8(2,I)-TAB_8(2,J))
               ! when reports "I" and "J" satisfy the duplicate
               ! check, the default is to set report "J"'s
               ! duplicate flag to 1 in order to throw it out
               ! (thus retaining report "I") - however this is
               ! overridden in the following cases {whereby
               ! report "I"'s duplicate flag is set to 1 in order
               ! to throw it out (thus instead retaining report "J")}:
                     JJDUP(j)=1
                  endif
               endif
            if(JJDUP(J) .EQ.1) then
               if(ctab7_i.eq.'missing-') then
                  ! case 1: report "I" has a missing report identifier
                  jdup(i) = 1
                  print *, 'I tossed ==> missing report id'
               else if(ctab7_j.ne.'missing-' .and.
     .                 (tab_8(8,j).lt.bmiss) .and.
     .                 (tab_8(9,j).lt.bmiss) .and.
     .                 (tab_8(10,i).le.3.and.tab_8(10,j).ge.4)) then
                  ! case 2: report "I" is from a less-preferred
                  ! file-type than report "J" {e.g., BUFR format
                  ! restricted data, SYNPBR preferred over TAC format
                  ! restricted data, SYNOPR; SYNOPB preferred over
                  ! SYNOP; SYNPMB preferred over SYNOPM}
                  ! and report "J" has a non-missing report identifier
                  jdup(i) = 1
                  print *, 'I tossed ==> from less-preferred file-type',
     .                     ' than J'
                  print *,'I,J:',nint(tab_8(10,i)),nint(tab_8(10,j))
               else if( (tab_8(8,i).ge.bmiss.or.tab_8(9,i).ge.bmiss)
     .            .and. (tab_8(8,j).lt.bmiss)
     .            .and. (tab_8(10,i).ge.4.and.tab_8(10,j).le.3)) then
                  ! case 3: report "J" is from a less-preferred
                  ! file-type than report "I" (see case 2), But,
                  ! report "J" has a non-missing PSML and ELEV
                  ! report "I" has a missing PSML or ELEV.
                  jdup(i) = 1
                  print *, 'I tossed ==> from missing ELEV or PMSL',
     .                     ' in report I'
                  print *,'I,J:',nint(tab_8(10,i)),nint(tab_8(10,j))
               end if
               ! At this point, if report "I"'s duplicate flag is not
               ! set in one of the above cases, set report "J"'s
               ! duplicate flag to 1 in order to throw it out thus
               ! retaining report "I"
               IF(JDUP(I).EQ.0) then
                  JDUP(J) = 1
                  print *, 'J tossed'
                  print *,'I,J:',nint(tab_8(10,i)),nint(tab_8(10,j))
               END IF

             ENDIF  ! endif for dupe test/comparison
             JJDUP(J)=0
  800       continue
            IF(K+1.GT.NTAB) CYCLE LOOP1
            J = IORD(K+1)
            tab7_j_8 = tab_8(7,j)
            K = K+1
         ENDDO
      ENDDO LOOP1
 
C  WRITE BACK THE DUP-CHECKED FILE(S)
C  ----------------------------------
 
      CALL GETENV('DUMMY_MSGS',DUMMY_MSGS)

      DO I=1,NFILES
         IF(FILI(I)(1:4).NE.'NONE') THEN
cpppppppppp
cc    idate_last = -9999
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
cc       print *, 'New message read in , NSUBS, IDATE = ',nsubs,idate
cpppppppppp

C  If no subsets in msg & dummy msgs not expected loop to next input msg
C  ---------------------------------------------------------------------

               IF(NSUBS.LE.0.AND.DUMMY_MSGS.NE.'YES')  CYCLE

               DUPES = .FALSE.

               IF(NSUBS.GT.0)  THEN
                  DO N=1,NSUBS
                     IF(ISUB+N.GT.NTAB) THEN
                        IDUP = 4
                     ELSE
                        IDUP = JDUP(ISUB+N)
                     ENDIF
                     NDUP(IDUP) = NDUP(IDUP)+1
                     ndup_file(idup,i) = ndup_file(idup,i)+1
                     IF(IDUP.GT.0) DUPES = .TRUE.
                  ENDDO
               ENDIF

cpppppppppp
cc    print *, 'DUPES = ',dupes
cpppppppppp

               IF(DUPES) THEN
                  CALL OPENMB(LUBFJ,SUBSET,IDATE)
cpppppppppp
cc             if(idate.ne.idate_last)
cc   $         print *, 'NEW MESSAGE OPENED'
cc             idate_last = idate
cpppppppppp
                  DO WHILE(IFBGET(LUBFI).EQ.0)
                     ISUB = ISUB+1
                     IF(ISUB.GT.NTAB) THEN
                        IDUP = 4
                     ELSE
                        IDUP = JDUP(ISUB)
                     ENDIF
                     IF(IDUP.EQ.0) THEN
                        CALL COPYSB(LUBFI,LUBFJ,IRET) ! Copy non-dups
                     ELSE
                        CALL COPYSB(LUBFI,00000,IRET) ! Toss dups
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
                  CALL COPYMG(LUBFI,LUBFJ)  ! Copy non-dups
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

C  GENERATE REPORT SUMMARY
C  -----------------------
 
      PRINT 300, ISUB,NDUP(2),MXTB,NTAB
  300 FORMAT(/'BUFR_DUPSYP READ IN A TOTAL OF',I8,' REPORTS'/
     .        '  A TOTAL OF ',I7,' REPORTS WERE SKIPPED DUE TO BEING ',
     .        'OVER THE LIMIT OF ',I7//
     .        'BUFR_DUPSYP CHECKED A TOTAL OF',I8,' REPORTS')
      do i = 1,nfiles
         if(ntab_file(i).gt.0) print 301, ntab_file(i),ctext_file(i)
  301 format('  THESE INCLUDE ',I8,' REPORTS FROM ',A20)
      enddo

      print 302, ndup(0)
  302 format(/'NUMBER OF UNIQUE REPORTS WRITTEN OUT ...........',i7)
      do i = 1,nfiles
         if(ndup_file(0,i).gt.0) print 303, ndup_file(0,i),ctext_file(i)
      enddo
  303 format(10x,'THESE INCLUDE ',i8,' REPORTS FROM ',a20)

      print 304, ndup(1)
  304 format(/'NUMBER OF REPORTS SKIPPED DUE TO:'/
     .        '   FAILING DUPLICATE CHECK (ALL SOURCES) .......',i7)
      do i = 1,nfiles
         if(ndup_file(1,i).gt.0) print 303, ndup_file(1,i),ctext_file(i)
      enddo

C  END OF PROGRAM
C  --------------

      WRITE(60,'("ALL DONE")')
      CALL W3TAGE('BUFR_DUPSYP')
      STOP

C  ERROR EXITS
C  -----------

  901 CONTINUE

      PRINT *, '#####BUFR_DUPSYP - UNABLE TO ALLOCATE ARRAYS'
      CALL W3TAGE('BUFR_DUPSYP')
      CALL ERREXIT(99)

      END

