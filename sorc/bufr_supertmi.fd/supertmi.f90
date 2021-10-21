!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: BUFR_SUPERTMI
!   PRGMMR: KEYSER           ORG: NP22        DATE: 2011-08-04
!
! ABSTRACT:  Reprocesses TRMM TMI data.  Reads each report from input
!   BUFR data dump file and checks for valid satellite id, valid
!   observation date and valid total precipitation observation.  Reports
!   passing checks are superobed onto a user-specified lat/lon grid
!   according to satellite id and then reprocessed into a BUFR file which
!   will later be read by the SSI analysis.  Also, a unique id is
!   generated here for each report and then encoded into the output
!   reprocessed BUFR file.  A user-specified switch allows reports to be
!   selected by location (lat/lon boundary).
!
! PROGRAM HISTORY LOG:
!
! 2000-10-10 R. Treadon - Original author (based on a similar superobing
!            code WAVE_DCODQUIKSCAT by Dennis Keyser)
! 2011-08-04 D. Keyser - In response to the latest version of BUFRLIB
!            which can handle embedded dictionary messages: increases
!            degree of BUFRLIB printout such that code will print a
!            diagnostic if any embedded dictionary messages when
!            reading in messages; moves call to OPENMG from prior to
!            reading of input BUFR file to after reading of input BUFR
!            file but prior to writing of output BUFR file, this
!            prevents a BUFRLIB abort when the input BUFR file contains
!            embedded BUFR dictionary messages (not sure why!) (Note:
!            We use OPENMG here rather than OPENMB because superobs are
!            generated and encoded into the output BUFR file only after
!            the entire input BUFR file is read - the date in Sec. 1 of
!            all BUFR messages in the output superob file is simply the
!            dump center date/hour, although this violates the NCEP
!            BUFR standard that all subsets in a BUFR message contain
!            the same year, month, day and hour as that in Sec. 1 of
!            the BUFR message, it keeps the output file more compact
!            and causes no harm since only PREPOBS_PREPDATA ends up
!            reading this file)
!
! USAGE:
!   Input files:
!     unit 05  - input data cards in the form of a namelist (see remarks)
!     unit 11  - NCEP BUFR data dump containing footprint resolution
!                ("hi-res") trmm tmi data ("TRMM")
!     unit 20  - BUFR mnemonic table (needed to produce NCEP BUFR file)
!
!   Output files:
!     unit 06  - standard output print
!     unit 51  - text listing of all reports encoded to unit 52
!              - (generated only when input namelist variable iprint=1)
!     unit 52  - NCEP BUFR data dump containing reprocessed (superobed)
!                TRMM TMI data ("SPTRMM") - each superob has a unique
!                report id
!
!   Subprograms called:
!     library
!       w3lib    - w3tagb   w3tage   errexit
!       bufrlib  - datelen  dumpbf   openbf   openmg   minimg  closmg
!                - ufbint   writsb   ufbcnt   closbf
!
!   Exit states:
!     cond =   0 - successful run
!          =  61 - error obtaining center date from first bufr message
!                  in unit 11
!          =  62 - error obtaining dump date from second bufr message
!                  in unit 11
!
! REMARKS: 
!
!     Contents of input BUFR file:
!
!     HDRSTR contains satellite id, observation year, month, day, hour,
!      minute, second,
!      data HDRSTR/'SAID YEAR MNTH DAYS HOUR MINU SECO '/
!
!     TMISTR contains observation location (latitude and longitude), total
!      precipitation rate, convective precipitation rate, cloud/rain water,
!      and cloud/precipitation ice
!      data TMISTR/'CLAT CLON TRRT CRRT RCWA PCIA'/
!
!
!     Contents of output BUFR file:
!
!        adata(1)  -- satellite id ("SAID")
!        adata(2)  -- suprob year ("YEAR")
!        adata(3)  -- superob month ("MNTH")
!        adata(4)  -- superob day ("DAYS")
!        adata(5)  -- superob hour ("HOUR")
!        adata(6)  -- superob minute ("MINU")
!        adata(7)  -- superob second ("SECO")
!        adata(8)  -- superob ("CLAT")
!        adata(9)  -- superob ("CLON")
!        adata(10) -- superob total precipitation rate (mm/hr) ("TRRT")
!        adata(11) -- superob convective precipitation rate (mm/hr) ("CRRT")
!        adata(12) -- superob cloud/rain water (mm) ("RCWA")
!        adata(13) -- superob cloud/precipitation ice (mm) ("PCIA")
!        adata(14) -- number of observations that went into making a
!                      superob ("ACAV")
!        adata(15) -- superob report id ("RPID")
!      
!          Report ID made up as follows:
!            Character 1:    Set to "S" to identify superobs
!            Characters 2-7: Index which incrementally counts reports
!            Character 8:    Indicator for satellite id ("R" - 282)
!
!
!   Variables read in namelist "INPUT":
!
!    IPRINT - if = 0 (default) will not print listing of all
!             processed reports to unit 51; if = 1 will print
!             listing to unit 51
!    DELAT   - latitude  spacing (degrees) of superob grid box
!        {note: normally whole degrees except for choice of 0.5 degrees
!                                                          (default=1.0)
!    DELON   - longitude spacing (degrees) of superob grid box
!        {note: normally whole degrees except for choice of 0.5 degrees
!                                                          (default=1.0)
!    LIMCNT - limiting number of individual reports for which a
!             superob is generated {i.e., if less than 'limcnt'
!             reports are found in the grid (lat/lon) box, then a
!             superob is not generated for this box}         (default=1)
!
!
!      the following 4 switches indicate the latitude/longitude
!                    boundary for accepting data
!
!    LATS    - this is the southern bdry {lat: deg. n (+); deg. s (-)}
!                                                          (default=-90)
!    LATN    - this is the northern bdry {lat: deg. n (+); deg. s (-)}
!                                                          (default=+90)
!    LONW    - this is the western  bdry {lon: 0-360 deg. w}
!                                                          (default=360)
!    LONE    - this is the eastern  bdry {lon: 0-360 deg. w}
!                                                          (default=  0)
!     (NOTE: These are always integers; must be whole degrees)
!
!
!
!          
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM-SP
!
!$$$

program bufr_supertmi

! Declare variables
  logical no_latlon_check
  character*8  subset,subout,stnid
  character*35 hdrstr,tmistr
  character*10 tm2str
  character*1 stnid8(282:282)
  
  integer nobs(282:282,720,360),icdate(5),iddate(5),idate(8)
  integer ndate(8),itobs(282:282),icnt(282:282)

  real(8) adata_8(15),bdata_8(13),rpid_8
  real    adata(14)
  real    blat(360),blon(720),sumtim(282:282,720,360),&
          sumlat(282:282,720,360),sumlon(282:282,720,360),&
          sumtot(282:282,720,360),sumcnv(282:282,720,360),&
          sumclw(282:282,720,360),sumcli(282:282,720,360)

! Declare namelist
  namelist/input/iprint,delat,delon,limcnt,lats,latn,lonw,lone

! Data statements
  data subout/'NC012013'/
  data hdrstr/'SAID YEAR MNTH DAYS HOUR MINU SECO '/
  data tmistr/'CLAT CLON TRRT CRRT RCWA PCIA      '/
  data tm2str/'ACAV RPID '/
  data lunin /11/,lunout/52/,luntab/20/
  data bmiss/10e10/

!  Char. 8 of report id for sat. id:
!              282
  data stnid8/ 'T'/

! Define equivalence
  equivalence  (stnid,rpid_8)


!****************************************************************************
! Begin code here
  call w3tagb('BUFR_SUPERTMI',2011,0216,0084,'NP23')
  write(6,*) ' '
  write(6,*) '=====> Welcome to Program BUFR_SUPERTMI - Version: 08-04-2011'
  write(6,*) ' '


! Initialize namelist parameters.  Then read in and echo to stdout
  iprint = 0
  delat  = 1.0
  delon  = 1.0
  limcnt = 1
  lats   = -90
  latn   =  90
  lonw   = 360
  lone   =   0

  write(6,*)'namelist input below'
  read(5,input,end=1905)
 1905 continue
  write(6,input)

  if(limcnt.le.0)  limcnt = 1

  no_latlon_check = (lats.eq.-90.and.latn.eq.90.and.lonw.eq.360.and.lone.eq.0)

! Setup for superobs 
  latsiz = 180./delat + 0.5
  lonsiz = 360./delon + 0.5
  dgh = delon/2.
  dgv = delat/2.

! blat & blon represent the center lat/lon for the selected grid
! boxes overwhich superobing is done: 
!   range:  -90 to + 90 lat; 0 to 360 west lon
  latbeg = 1
  k = 0
  do lat  = latbeg,latsiz
     k = k + 1
     blat(k) = (delat * ((lat - 1) - latsiz/2)) + dgv
  enddo
  do long = 1,lonsiz
     blon(long) = (delon * (long - 1)) + dgh
  enddo


! Set up output report id.  character 1 of output report id is set 
! to "S" to identify these as superobs
  stnid = 'S???????'


! Initalize all sums as zero
  nobs   = 0
  sumtim = 0.
  sumlat = 0.
  sumlon = 0.
  sumtot = 0.
  sumcnv = 0.
  sumclw = 0.
  sumcli = 0.
  
  icnt   = 0
  icntt  = 0
  itobs  = 0
  itobst = 0
  
  ibadd  = 0
  isaid  = 0
  irain  = 0
  ilalo  = 0

  ird = 0
  igood=0

  call datelen(10)


! Get the center & dump date from input bufr data dump file
  call dumpbf(lunin,icdate,iddate)
  write(6,*) ' '
  write(6,*) 'From Original BUFR Data Dump File in Unit ',lunin,' (TRMM):'
  write(6,*) '     - Center date (ICDATE) = ',icdate
  write(6,*) '     - Dump date   (IDDATE) = ',iddate
  write(6,*) 'Will transfer these to output reprocessed (superobed) ', &
       'TRMM TMI BUFR file in Unit ',lunout,' (SPTRMM)'
  write(6,*) ' '
     
! If center date could not be read from first dummy message, stop execution
  if(icdate(1).le.0)  then
     write(6,*) 'DUMPBF ERROR - CENTER DATE COULD NOT BE READ ', &
          'FROM INPUT TRMM TMI DATA DUMP FILE -- STOP 61'
     call w3tage('BUFR_SUPERTMI')
     call errexit(61)
  end if

! If dump date could not be read from second dummy message, stop execution
  if(iddate(1).le.0)  then
     write(6,*) 'DUMPBF ERROR - DUMP DATE COULD NOT BE READ ', &
          'FROM INPUT TRMM TMI DATA DUMP FILE -- STOP 62'
     call w3tage('BUFR_SUPERTMI')
     call errexit(62)
  end if

  idatbf =icdate(1)*1000000 +icdate(2)*10000 +icdate(3)*100 +icdate(4)
  idatdm =iddate(1)*1000000 +iddate(2)*10000 +iddate(3)*100 +iddate(4)



! Open the input BUFR data dump file  
  call openbf(lunin,'IN',lunin)
  call openbf(0,'QUIET',1)

! Open output BUFR file which will contain reprocessed (superobed) data
  call openbf(lunout,'OUT',luntab)

  write(6,101) lunout,luntab
101 format(/8x,'===> BUFR DATA SET IN UNIT',i3,' SUCCESSFULLY OPENED', &
         ' FOR OUTPUT; BUFR MNEMONIC TABLES A,B,D IN UNIT',i3/13x,'READ ', &
         'IN AND ENCODED INTO BEGINNING MESSAGES OF OUTPUT DATA SET'/)

  write(6,106) delat,delon,limcnt,lats,latn,lonw,lone,no_latlon_check
106 format(/5x,'USER SPEC. SWITCHES: ',/5x, &
         'DELAT=',f5.1,'; DELON=',f5.1,'; LIMCNT=',i3/5x, &
         'LATITUDE BDRY :',i4,' TO',i4,' (S-N)'/5x, &
         'LONGITUDE BDRY:',i4,' TO',i4,' (W-E) DEG. W.'/5x, &
         'NO_LATLON_CHECK:',l4/)


! Transfer center & dump date from input bufr file to output bufr file
  call openmg(lunout,subout,idatbf)
  call minimg(lunout,icdate(5))
  call openmg(lunout,subout,idatdm)
  call minimg(lunout,iddate(5))
  call closmg(lunout)

  idate    = 0
  idate(1:3) = icdate(1:3)
  idate(5:6) = icdate(4:5)

! Read through the messages/subsets in the file
  irecl = 0
  isubl = 0
  isubt = 0

  print '(/"BEGIN READING IN DATA FROM UNIT ",i2)', lunin

  loop1: do while(ireadmg(lunin,subset,mdate).eq.0)
     loop1n1: do while(ireadsb(lunin).eq.0)
        ird = ird+1

!       Extract satellite id and date.
        call ufbint(lunin,bdata_8,7,1,iret,hdrstr)
        isatid = nint(bdata_8(1))
        iyr    = nint(bdata_8(2))
        mon    = nint(bdata_8(3))
        iday   = nint(bdata_8(4))
        ihr    = nint(bdata_8(5))
        imin   = nint(bdata_8(6))
        isec   = nint(bdata_8(7))

!       Check for valid satellite id & tag using character 8 of report id
        if(isatid.ne.282) then
           print '("SATELLITE ID (",i5,") IS NOT RECOGNIZED")', &
                isatid
           isaid = isaid + 1
           cycle loop1n1
        end if
        stnid(8:8) = stnid8(isatid)


!       Check obs date for realism
        if ( iyr<0  .or. &
             mon<1  .or. mon>12  .or. &
             iday<1 .or. iday>31 .or. &
             ihr<0  .or. ihr>24  .or. &
             imin<0 .or. imin>60 .or. &
             isec<0 .or. isec>60 ) then
           print '("BAD DATE:",i4.4,5i2.2," SUBSET:",a8)', &
                iyr,mon,iday,ihr,imin,isec,subset
           ibadd = ibadd + 1
           cycle loop1n1
        end if

!       Extract observation location and data.
        call ufbint(lunin,bdata_8(8),6,1,iret,tmistr)
        slat   = bdata_8(8)
        slon   = bdata_8(9)
        if (slon<0.) slon = 360. + slon
        tot = bdata_8(10)
        cnv = bdata_8(11)
        clw = bdata_8(12)
        cli = bdata_8(13)

!       Check that total precipitation observation is "good."  If not, skip.
        if (tot<0. .or. tot>1000.) then
           irain = irain + 1
           cycle loop1n1
        endif

!       Store latitude in inlat: input: n(+), s(-);  output: n(+), s(-) x 100
        inlat = nint(slat * 100.)

!       Store lon. in iwlon: input: 0-360 e; output: 0-360 w x 100
        iwlon = mod((36000-nint(slon*100.)),36000)

        if(.not. no_latlon_check)  then
!       Check that this report is within the specified lat/lon boundary;
!       If not, go on to next report
           iwlont = iwlon
           lonwt  = lonw
           if(lonw.lt.lone)  then
              if(iwlon.lt.lone*100)  iwlont = iwlon + 36000
              lonwt = lonw + 360
           end if
           if(iwlont.eq.0)  iwlont = 360
           if(inlat.lt.lats*100.or.inlat.ge.latn*100.or.iwlont.gt. &
                lonwt*100.or.iwlont.le.lone*100)  then
              ilalo = ilalo + 1
              cycle loop1n1
           end if
        end if
           


!       Report passed all checks - come here to superob
        igood = igood + 1

!       Store report time in "itime", check for cross over to next day (if
!       so, add 24000 so avg. Time makes sense; will correct later if itime
!       is > 24000) (this makes the assumption that is can only occur when
!       the center dump hour is 0, okay right now since this data dumps
!       only occur at 00, 06, 12 and 18z with +/- 3-hour time window)
        
        itime = nint((1000. * (real(ihr) + real(imin)/60. + &
             real(isec)/3600.)) + 0.00005)
        if(icdate(4).eq.0.and.ihr.lt.12) itime=itime+24000
        

!       Find lat/lon box containing this report (ilm, jlm)
        ilm = 0
        jlm = 0
        latbeg = 1
        kndx = 0
        loop1n2: do ii = latbeg,latsiz
           kndx = kndx + 1
           if(inlat.ge.nint((blat(kndx)+dgv)*100.).or. &
                inlat.lt.nint((blat(kndx)-dgv)*100.))  cycle loop1n2
           ilm = kndx
           loop1n3: do ij = 1,lonsiz
              if(iwlon.ge.nint((blon(ij)+dgh)*100.).or.iwlon.lt. &
                   nint((blon(ij)-dgh)*100.))  cycle loop1n3
              jlm = ij
              go to 5
           enddo loop1n3
        enddo loop1n2

!       If no box found, go on to next report
        write(6,106) slat,slon
105     format(5x,'* * * *   REPORT LAT/LON IS EITHER MISSING OR ', &
             'INVALID, LAT=',f6.2,', LON=',f7.2,' -- GO ON TO NEXT REPORT'/)
        cycle loop1n1

5       continue

!       The following should   n e v e r   happen!!
        if(ilm.eq.0.or.jlm.eq.0)  then
           write(6,*) '&&& EITHER ILM (',ilm,') OR JLM (',jlm,')', &
                ' IS ZERO -- SHOULD NEVER HAPPEN !!!'
           cycle loop1n1
        end if


!       Sum up values within the grid box ilm, jlm
        nobs(isatid,jlm,ilm) = nobs(isatid,jlm,ilm)+1
        itobs(isatid) = itobs(isatid) + 1
        sumtim(isatid,jlm,ilm) = sumtim(isatid,jlm,ilm) + real(itime)
        sumlat(isatid,jlm,ilm) = sumlat(isatid,jlm,ilm) + slat
        sumlon(isatid,jlm,ilm) = sumlon(isatid,jlm,ilm) + slon
        sumtot(isatid,jlm,ilm) = sumtot(isatid,jlm,ilm) + tot
        sumcnv(isatid,jlm,ilm) = sumcnv(isatid,jlm,ilm) + cnv
        sumclw(isatid,jlm,ilm) = sumclw(isatid,jlm,ilm) + clw
        sumcli(isatid,jlm,ilm) = sumcli(isatid,jlm,ilm) + cli
           
     enddo loop1n1

  enddo loop1

  print '(/"DONE READING IN DATA FROM UNIT ",i2)', lunin

  call openmg(lunout,subout,idatbf)

! Loop thru the satellite id's
  loop2: do k = 282,282   ! currently only one possible sat. id - 282

! Check to see if any reports are present from this satellite id
     if(itobs(k).eq.0)  cycle loop2

     itobs(k) = 0

! Loop thru boxes: construct means & write superob to adata array
     latbeg = 1
     kndx = 0
     loop2n1: do ki = latbeg,latsiz
        kndx = kndx + 1
        loop2n2: do kj = 1,lonsiz

!       If less than 'limcnt' individual reports went into making superob
!       in this box, skip processing the superob for this box
           if(nobs(k,kj,kndx).lt.limcnt)  cycle loop2n2
           itobs(k) = itobs(k) + nobs(k,kj,kndx)
           itobst = itobst + nobs(k,kj,kndx)
           xmul = real(nobs(k,kj,kndx))

!       Calc. means within each grid box
           avgtim = sumtim(k,kj,kndx)/xmul
           avglat = sumlat(k,kj,kndx)/xmul
           avglon = sumlon(k,kj,kndx)/xmul

           avgtot = sumtot(k,kj,kndx)/xmul
           avgcnv = sumcnv(k,kj,kndx)/xmul
           avgclw = sumclw(k,kj,kndx)/xmul
           avgcli = sumcli(k,kj,kndx)/xmul


!       Store mean time in "itime" (check for mean time into the next day)
           itime = nint(avgtim + 0.00005)
           if(itime.ge.24000)  itime = itime - 24000.

!       Modify (if needed) center date year, month and day to properly
!       define the year, month and day of the superob
           ndate = idate
           jtime=nint(1000. * (real(idate(5)) + &
            real(idate(6))/60. + real(idate(7))/3600.))
           if(abs(jtime-itime).gt.6000)  then
              daychg = sign(1.0,real(jtime-itime))
              call w3movdat((/daychg,0.,0.,0.,0./),idate,ndate)
           end if

!       Build an intermediate array (adata) containing the data
           adata = bmiss
           adata(1)   = k
           adata(2:4) = ndate(1:3)
           adata(5)   = int(itime/1000.)
           xmin       = (itime - (nint(adata(5)) * 1000)) * 0.06
           adata(6)   = int(xmin)
           adata(7)   = (nint(xmin*100.) - nint(adata(6))*100)*0.6
           adata(8)   = avglat
           adata(9)   = avglon
           adata(10)  = avgtot
           adata(11)  = avgcnv
           adata(12)  = avgclw
           adata(13)  = avgcli
           adata(14)  = nobs(k,kj,kndx)
           icnt(k)    = min(999999,icnt(k)+1)
           icntt      = icntt + 1
           write(stnid(2:7),'(i6.6)')  icnt(k)

!       Transfer superob data to real*8 output array
           adata_8(15)   = rpid_8
           adata_8(1:14) = adata(1:14)

           if(iprint.eq.1)  write(51,8601)  (nint(adata_8(ii)),ii=1,7), &
            (adata_8(ii),ii=8,13),nint(adata_8(14)),stnid
8601       format(1x,i4,3x,i4,5i3.2,2x,2f7.2,4(2x,f7.2),2x,i5,4x,a8)

!       Encode the reprocessed (superobed) data into a subset (report)
           call ufbint(lunout,adata_8,15,1,iret,hdrstr//tmistr//tm2str)

!       Write the encoded subset (report) into the output bufr message
           call writsb(lunout)
           call ufbcnt(lunout,irec,isub)
           if(irec.gt.irecl)  then
              isubt = isubt + isubl
              write(6,1254) irec-1,isubl,isubt
1254          format(/' --- THIS REPORT OPENS NEW BUFR MSG: LAST MSG WAS NO.',&
                i10,' (DATA) WITH',i5,' RPTS (TOTAL NO. RPTS WRITTEN=',i7,')'/)
           end if
           isubl = isub
           irecl = irec

        enddo loop2n2
     enddo loop2n1
  enddo loop2


! Close input and output NCEP TRMM TMI BUFR data sets
  call closbf(lunin)
  write(6,102) lunin
102 format(/5x,'===> BUFR DATA SET IN UNIT',i3,' SUCCESSFULLY CLOSED'/)

  call ufbcnt(lunout,irec,isub)
  isubt = isubt + isub
  write(6,1253) irec,isub,isubt
1253 format(/' --- WROTE BUFR DATA MSG NO.',i10,' WITH',i5,' REPORTS ', &
          '(TOTAL NO. REPORTS WRITTEN =',i7,')'/)
  call closbf(lunout)
  write(6,102) lunout


! Print statistics to stdout
  write(6,699) ird,isaid,irain,ilalo,igood
 699 format(//5X,'*** PROCESSING ENDED NORMALLY ***'// &
     ' >>>  TOTAL NUMBER OF INPUT TRMM TMI REPORTS READ ',25('.'),i8,'  <<<'// &
     12x,'- Number skipped due to unrecognized satellite id ',13('.'),i7// &
     12x,'- Number skipped due to bad precipitation value   ',13('.'),i7// &
     12x,'- Number skipped due to invalid lat/lon location ',14('.'),i7// &
     12x,'- NUMBER OF INPUT TRMM TMI REPORTS PASSING ALL CHECKS ',9('.'),i8/)

  write(6,114) itobs,itobst
 114 format(/30x,'+++ NO. OF ORIGINAL REPORTS THAT WERE USED TO GENERATE ', &
     'SUPEROBS +++'/ &
     /45x,'FROM SATELLITE ID 282 ...... ',i10, &
     /45x,'TOTAL FROM ALL SATELLITES .. ',i10/)
!dak  write(6,115)
 115 format(5x,'NOBS:')
!dak  write(6,116) NOBS
 116 format(1x,30i4)

  write(6,110) icnt,icntt,(1.-float(icntt)/float(itobst))*100.
 110 format(/27x,'+++  NUMBER OF REPROCESSED (SUPEROBED) TRMM TMI REPORTS ', &
      'WRITTEN TO OUTPUT FILE +++'// &
      40x,'>>>  FROM SATELLITE ID 282 ...... ',i7,'  <<<'// &
      ' >>>  TOTAL NUMBER OF REPROCESSED (SUPEROBED) TRMM TMI REPORTS ', &
      'WRITTEN ..',i7,'  <<<'// &
      'percentage compression is ',f6.2,'%'// &
      44x,'*****  PROGRAM SUCCESSFULLY COMPLETED  *****'/)


! End code here
  call w3tage('BUFR_SUPERTMI')
  stop                                                              
  
end program bufr_supertmi
