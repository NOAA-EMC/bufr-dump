C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_DUPTAC
C   PRGMMR: WHITING          ORG: EMC         DATE: 2020-08-20
C
C ABSTRACT: Performs a cross subtype duplicate check of pairs of buoy 
c   data types (originating as text/TAC or BUFR) - msg typ 001 and 
c   drifting buoy subtypes 002 & 102; and fixed buoy subtypes 003 & 103.
C   INFORMATION IS READ SEPARATELY FROM EACH FILE THAT
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
C 2020-08-20  J. Whiting   Original version for implementation
c     New code to check for and remove duplicate reports found in TAC
C     feed data streams relative to the BUFR feed replacements.
C     Currently configured for buoy (moored & drifting) data streams
c     (TAC: mbuoy/nc001003 dbuoy/nc001002, and 
c     BUFR: mbuoyb/nc001103 dbuoyb/nc001102).
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT - RECORDS CONTAINING THE WORKING INPUT
C                FILE NAMES FOR TRADITIONAL ALPHANUMERIC CODES (TAC)
C                BUOY TYPES TO BE CHECKED AGAINST BUFR-FEED VERSIONS
C                FOR DUPLICATE REPORTS, EVENTUALLY BEING COMBINED INTO
C                A SINGLE DUMP FILE (usually called "sfcshp");
C                THE ONLY FILE NAMES CONSIDERED BY THIS PROGRAM ARE
C                *001.002 (drifting buoy) AND *001.003 (fixed buoy);
C                THE OUTPUT FILE NAMES WILL BE THE SAME AS THE INPUT
C                NAMES HERE
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
C                  BVERS
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
      PROGRAM BUFR_DUPTAC
 
      PARAMETER (MXTS=9)    ! # of sort keys (+1 for modified RPID)
      PARAMETER (NFILES=2)  ! Number of input files being considered
 
      REAL(8),ALLOCATABLE :: TAB_8(:,:)
      REAL(8),ALLOCATABLE :: RAB_8(:,:)
      INTEGER,ALLOCATABLE :: IWORK(:)
      INTEGER,ALLOCATABLE :: IORD(:)
      INTEGER,ALLOCATABLE :: JDUP(:)

      CHARACTER*80  TSTR,TSTRH,RSTR,FILE,FILI(NFILES),FILO
      CHARACTER*8   SUBSET,crpidI,crpidJ,crpid,crpid0,cvstr
      CHARACTER*3   DUMMY_MSGS, ctyp

      DIMENSION     NDUP(0:2),IPTR(2,NFILES),nrpts(nfiles)

      REAL(8)       UFBTAB_8,rpid,rpidI,rpidJ,BMISS,GETBMISS

      LOGICAL       DUPES,SFCTAC,SFCBFR,itac(nfiles)

      EQUIVALENCE   (crpid,rpid),(crpidI,rpidI),(crpidJ,rpidJ)

      DATA TSTR  /'CLAT  CLON  MNTH DAYS HOUR MINU RPID '/
      DATA TSTRH /'CLATH CLONH MNTH DAYS HOUR MINU RPID '/
      DATA RSTR  /'RCYR  RCMO  RCDY RCHR RCMI           '/

      DATA DEXY  /0.05/  ! /0/
      DATA DMON  /0/
      DATA DDAY  /0/
      DATA DOUR  /0/
      DATA DMIN  /0/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_DUPTAC',2020,0233,0050,'EMC')


      print * ,'---> Welcome to BUFR_DUPTAC - v08-20-2020'
      call bvers(cvstr)
      print'(13x,"(BUFRLIB version = v",a,")",//)', trim(cvstr)


      CALL DATELEN(10)

ccccc CALL OPENBF(0,'QUIET',2) ! print ALL warning AND info msgs
      CALL OPENBF(0,'QUIET',-1) ! NO printout except ABORT msgs
 
C  ASSIGN DEFAULT VALUE FOR 'MISSING' TO LOCAL BMISS VARIABLE
C  ----------------------------------------------------------

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
 
      fili(1:nfiles)='' ; FILI(1:NFILES)(1:4) = 'NONE'
      ifili=0
      itac = .false.

      SFCTAC = .FALSE.    ! flag for  TAC input
      SFCBFR = .FALSE.    ! flag for BUFR input
 
c -- Read filename inputs (only process 2 streams, mtyp=001, TAC & BUFR)
      do nfile=1,2

        READ(5,'(A80)',END=2) FILE
        FILI(nfile) = FILE

c -- 
        i=index(trim(file),'001')
        READ(FILE(I+4:I+6),'(I3)') MST

! --  TAC: dbuoy: 002, mbuoy :003
! -- BUFR: dbuoyb:102, mbuoyb:103
        ctyp = ''
        IF(MST.EQ.  2.OR.MST.EQ.  3) then 
          SFCTAC = .TRUE.
          ctyp = 'TAC'
          itac(nfile) = .true.
        endif ! mst = 2 or 3 TAC
        IF(MST.EQ.102.OR.MST.EQ.103) then 
          SFCBFR = .TRUE.
          ctyp = 'BFR'
        endif ! mst = 102 or 103 BUFR


        PRINT '(a,$)', ' >> WILL CHECK ' // trim(FILE(I:I+6))

        if(ctyp.ne.'') print'(a)','  ('//adjustl(trim(ctyp))//')'


      enddo ! nfile=1,2

2     CONTINUE


c -- check for proper inputs
      if (nfile.ne.3 .or. .not.sfctac .or. .not.sfcbfr) then 

        PRINT'(//,2x,a)','BUFR_DUPTAC: *** input ERROR(s)...'

        if(nfile-1.ne.2) then 
          PRINT'(3x,a,i1,a)',' * Wrong # of inputs (',nfile-1,'):'
          print'(7x,i2,")",2x,a)',(i,fili(i),i=1,2)
        endif ! nfile-1 ne 2

        IF(.NOT.SFCTAC) PRINT'(3x,a)',' * No TAC data stream to check'

        IF(.NOT.SFCBFR) PRINT'(3x,a)',' * No BUFR data stream to check' 

        print*
        print*,' TAC v BUFR duplicate checks not run; exiting (8)'

        CALL W3TAGE('BUFR_DUPTAC')
        call errexit(8)
      endif ! nfile-1 ne 2, no sfctac nor sfcbfr


         PRINT 200, DEXY,DDAY,DOUR
  200 FORMAT(/'BUFR_DUPTAC PARAMETERS:'/
     .        3X,'TOLERANCE FOR LAT/LON CHECKS (IN DEGREES) .. ',F7.3/
     .        3X,'TOLERANCE FOR DAY CHECK (IN DAYS) .......... ',F7.3/
     .        3X,'TOLERANCE FOR HOUR CHECK (IN HOURS) ........ ',F7.3/)
 

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
               PRINT'("#####BUFR_DUPTAC (UFBTAB) CANNOT PROCESS '//
     .            'COMPRESSED BUFR MESSAGES -- FATAL ERROR")'
               CALL W3TAGE('BUFR_DUPTAC')
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
            IF(IBFMS(TAB_8(2,IPT)).EQ.1) THEN      ! CLON missing, get CLATH/CLONH
              OPEN(LUBFI,FILE=FILI(I),FORM='UNFORMATTED')
              CALL UFBTAB(LUBFI,TAB_8(1,IPT),MXTS,MXTB-IPT+1,NTAB,TSTRH)
            ENDIF ! TAB_8(2,*) CLON missing
            OPEN(LUBFI,FILE=FILI(I),FORM='UNFORMATTED')
            CALL UFBTAB(LUBFI,RAB_8(1,IPT),MXTS,MXTB-IPT+1,NTAB,RSTR)
            IPTR(1,I) = IPT
            IPTR(2,I) = IPT+NTAB-1
            IPT = IPT+NTAB
         ENDIF
      ENDDO ! I=1,NFILES

      NTAB = IPT-1
 
C  SET MISSING MINU TO ZERO; STORE INPUT FILE INDEX
C  ------------------------------------------------
      DO N=1,NTAB
         IF(IBFMS(TAB_8(6,N)).EQ.1) TAB_8(6,N) = 0     ! data missing
         DO I=1,NFILES
            IF(N.GE.IPTR(1,I) .AND. N.LE.IPTR(2,I)) THEN
               TAB_8(8,N) = I
               EXIT
            ENDIF
         ENDDO ! I=1,NFILES

c -- set missing lat &/or lon to minus file index (ie, -n)
         IF(IBFMS(TAB_8(1,N)).EQ.1) print*,"lat missing @ n=",n
         IF(IBFMS(TAB_8(2,N)).EQ.1) print*,"lon missing @ n=",n
         IF(IBFMS(TAB_8(1,N)).EQ.1) TAB_8(1,N)=-99.9    ! lat
         IF(IBFMS(TAB_8(2,N)).EQ.1) TAB_8(2,N)=-999.9   ! lon

c --- ??? llflag for removing rpts w/ missing lat/lon ???  ! db

      ENDDO ! N=1,NTAB

c  set up BUFR IDs in TAC format (ie, tac[1:5] == bufr[1:2,5:7] )
      do n=1,ntab
        rpid = tab_8(7,n)
        crpid0 = crpid
        if(len(trim(crpid)).eq.7) crpid0 = crpid(1:2)//crpid(5:7)//'  '
        crpid = crpid0
        tab_8(9,n) = rpid
      enddo ! n=1,ntab
 
C  GET A SORTED INDEX OF THE REPORTS BY RECEIPT, OBS TIME, AND LON/LAT
C  -------------------------------------------------------------------

!     print*,'db: orders - rpid2 mon day hr min lat lon rcptim'

      CALL ORDERS( 2,IWORK,RAB_8(5,1),IORD,NTAB,MXTS,8,2) ! rcpt. minute
      CALL ORDERS(12,IWORK,RAB_8(4,1),IORD,NTAB,MXTS,8,2) ! rcpt. hour
      CALL ORDERS(12,IWORK,RAB_8(3,1),IORD,NTAB,MXTS,8,2) ! rcpt. day
      CALL ORDERS(12,IWORK,RAB_8(2,1),IORD,NTAB,MXTS,8,2) ! rcpt. month
      CALL ORDERS(12,IWORK,RAB_8(1,1),IORD,NTAB,MXTS,8,2) ! rcpt. year
      CALL ORDERS(12,IWORK,TAB_8(2,1),IORD,NTAB,MXTS,8,2) ! longitude
      CALL ORDERS(12,IWORK,TAB_8(1,1),IORD,NTAB,MXTS,8,2) ! latitude
      CALL ORDERS(12,IWORK,TAB_8(6,1),IORD,NTAB,MXTS,8,2) ! minute
      CALL ORDERS(12,IWORK,TAB_8(5,1),IORD,NTAB,MXTS,8,2) ! hour
      CALL ORDERS(12,IWORK,TAB_8(4,1),IORD,NTAB,MXTS,8,2) ! day
      CALL ORDERS(12,IWORK,TAB_8(3,1),IORD,NTAB,MXTS,8,2) ! month
!     CALL ORDERS(10,IWORK,TAB_8(7,1),IORD,NTAB,MXTS,8,2) ! report id
      CALL ORDERS(10,IWORK,TAB_8(9,1),IORD,NTAB,MXTS,8,2) ! modified report id

C  GO THROUGH THE REPORTS IN ORDER, MARKING DUPLICATES
C  ---------------------------------------------------

      print*,'post orders'

      print *,"    k irec rpt# lat lon  hr min  rpid2 rpid  filendx"

      DO K=1,NTAB-1
         IREC = IORD(K)
         JREC = IORD(K+1)
         rpidI = TAB_8(9,IREC)
         rpidJ = TAB_8(9,JREC)

       if(k.le.10.or.(k.gt.5316.and.k.le.5326.or..true.)) then 
         print'(3(1x,i5)," :",1x,f9.5,1x,f10.5,2(2x,i2),$)', k,irec,
     $     irec-((iptr(1,2)-1)*(int(tab_8(8,irec))-1)),       ! orig file rpt#
     $     (tab_8(i,irec),i=1,2),(int(tab_8(j,irec)),j=5,6)   ! la lo hr min
         rpid = tab_8(9,irec) ; print'(1x,a,$)',"'"//crpid//"'" ! rpid mod
         rpid = tab_8(7,irec) ; print'(1x,a,$)',"'"//crpid//"'" ! rpid
         print'(1x,i1,$)',int(tab_8(8,irec))                    ! file index
       endif ! k le 10


         DUPES =
     . NINT(ABS(TAB_8(1,IREC)-TAB_8(1,JREC))*100).LE.NINT(DEXY*100)   ! latitude
     . .AND.
     . NINT(ABS(TAB_8(2,IREC)-TAB_8(2,JREC))*100).LE.NINT(DEXY*100)   ! longitude
     . .AND.
     . NINT(ABS(TAB_8(3,IREC)-TAB_8(3,JREC))*100.).LE.NINT(DMON*100.) ! month
     . .AND.
     . NINT(ABS(TAB_8(4,IREC)-TAB_8(4,JREC))*100.).LE.NINT(DDAY*100.) ! day
     . .AND.
     . NINT(ABS(TAB_8(5,IREC)-TAB_8(5,JREC))*100.).LE.NINT(DOUR*100.) ! hour
     . .AND.
     . NINT(ABS(TAB_8(6,IREC)-TAB_8(6,JREC))*100.).LE.NINT(DMIN*100.) ! minute
     . .AND.
     . (crpidI.EQ.crpidJ)                                             ! rpid 
     . .AND.
     . NINT(TAB_8(8,IREC)).NE.NINT(TAB_8(8,JREC))                     ! file#

      print '(1x,l1," :",$)', dupes
      print'(8(1x,L1),$)',
     & NINT(ABS(TAB_8(1,IREC)-TAB_8(1,JREC))*100).le.NINT(DEXY*100),  !  lat
     & NINT(ABS(TAB_8(2,IREC)-TAB_8(2,JREC))*100).le.NINT(DEXY*100),  !  lon
     & NINT(ABS(TAB_8(3,IREC)-TAB_8(3,JREC))*100).le.NINT(DMON*100.), !  mon
     & NINT(ABS(TAB_8(4,IREC)-TAB_8(4,JREC))*100).le.NINT(DDAY*100.), !  day
     & NINT(ABS(TAB_8(5,IREC)-TAB_8(5,JREC))*100).le.NINT(DOUR*100.), !  hr
     & NINT(ABS(TAB_8(6,IREC)-TAB_8(6,JREC))*100).le.NINT(DMIN*100.), !  min
     & (crpidI.EQ.crpidJ),
     & NINT(TAB_8(8,IREC)).NE.NINT(TAB_8(8,JREC))

! save file 1 rpts, mark file 2 rpts as dupes
         IF(DUPES) THEN
            IF(TAB_8(8,IREC).EQ.1) JDUP(IREC) = 1
            if(tab_8(8,irec).eq.2) JDUP(JREC) = 1
         ENDIF ! DUPES true

      print'(" ::",1x,i1,$)', jdup(irec)
      if(dupes.and.tab_8(8,irec).eq.2)print'(a,$)'," toss next" ; print*
      if(dupes) print'(3x,a,4i6)','   -dupes found ',
     $                       irec,jdup(irec),jrec,jdup(jrec) ! db

      ENDDO ! K=1,NTAB-1
      print* ! linefeed
 
C  WRITE BACK THE DUP-CHECKED FILE(S)
C  ----------------------------------
 
      CALL GETENV('DUMMY_MSGS',DUMMY_MSGS)

      print*,'post obs list'

      print*
      do i=1,2
        nrpts(i)=iptr(2,i)-iptr(1,i)+1
        print 250, nrpts(i),trim(fili(i))
        if(itac(i)) print'(a)', ' (TAC)'
        if(.not.itac(i)) print'(a)', ' (BUFR)'
      enddo ! 1=1,2
      print*
  250 FORMAT('BUFR_DUPTAC READ IN ',I8,' RPTS FROM ',a,$)

      DO I=1,1
         IF(FILI(I)(1:4).NE.'NONE') THEN
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

      nr=0 ; nrd=0  ! db
            DO WHILE(IREADMG(LUBFI,SUBSET,IDATE).EQ.0)
               NSUBS = NMSUB(LUBFI)

C  If no subsets in msg & dummy msgs not expected loop to next input msg
C  ---------------------------------------------------------------------

               IF(NSUBS.LE.0.AND.DUMMY_MSGS.NE.'YES')  CYCLE

               DUPES = .FALSE.

               IF(NSUBS.GT.0)  THEN
                  DO N=1,NSUBS                   ! # of rpts in msg
                     IF(ISUB+N.GT.NTAB) THEN     ! isub = start ptr
                        IDUP = 2
                     ELSE
                        IDUP = JDUP(ISUB+N)
                     ENDIF
                     NDUP(IDUP) = NDUP(IDUP)+1
                     IF(IDUP.GT.0) DUPES = .TRUE.
                  ENDDO
               ENDIF ! (NSUBS.GT.0)

               IF(DUPES) THEN
                  CALL OPENMB(LUBFJ,SUBSET,IDATE)    ! filo

                  DO WHILE(IFBGET(LUBFI).EQ.0)
                     ISUB = ISUB+1
                     IF(ISUB.GT.NTAB) THEN
                        IDUP = 2
                     ELSE
                        IDUP = JDUP(ISUB)
                     ENDIF
                     IF(IDUP.EQ.0) THEN
                        CALL COPYSB(LUBFI,LUBFJ,IRET) ! Copy non-dups
      nr=nr+1    ! db
                     ELSE
                        CALL COPYSB(LUBFI,00000,IRET) ! Skip dups
      nrd=nrd+1  ! db
                     ENDIF
                  ENDDO ! WHILE(IFBGET(LUBFI).EQ.0)
               ELSE ! (DUPES)

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
      nr=nr+nsubs
               ENDIF ! (DUPES)
            ENDDO ! WHILE(IREADMG(LUBFI,SUBSET,IDATE).EQ.0)

!     print'(a,5(1x,i6))','db: ndup(0-2) nr nrd ',ndup,nr,nrd  ! db


            CALL CLOSBF(LUBFI)
            CALL CLOSBF(LUBFJ)

! write FILO over FILI(I) [I=1]
            OPEN(LUBFI,FILE=FILI(I),FORM='UNFORMATTED')
            OPEN(LUBFJ,FILE=FILO   ,FORM='UNFORMATTED')
            CALL COPYBF(LUBFJ,LUBFI)

         ENDIF ! (FILI(I)(1:4).NE.'NONE')
      ENDDO ! I=1,1
 
C  GENERATE REPORT
C  ---------------
 
      nrpt2 = iptr(2,2)-iptr(2,1)              ! # of reports in file2
      PRINT 300,NDUP(0),NDUP(1),nrpt2
  300 FORMAT(/'NUMBER OF UNIQUE TAC REPORTS WRITTEN OUT........',I7/
     .        'NUMBER OF DUPLICATE TAC REPORTS SKIPPED.........',i7/ 
     .        'NUMBER OF UNCHANGED BUFR REPORTS WRITTEN OUT....',i7/)
!    .        'NUMBER OF TAC REPORTS SKIPPED DUE TO:'/
!    .        '   FAILING DUPLICATE CHECK .....................',I7/)

C  END OF PROGRAM
C  --------------

  999 WRITE(60,'("ALL DONE")')
      CALL W3TAGE('BUFR_DUPTAC')
      STOP

C  ERROR EXITS
C  -----------

  901 CONTINUE

      PRINT *, '#####BUFR_DUPTAC - UNABLE TO ALLOCATE ARRAYS'
      CALL W3TAGE('BUFR_DUPTAC')
      CALL ERREXIT(99)

      END
