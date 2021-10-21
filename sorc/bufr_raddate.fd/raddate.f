C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM:  RADDATE     ADD REAL HOUR INCREMENT TO REAL DATE
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2013-01-13
C
C ABSTRACT: RADDATE ACCEPTS A REAL DATE AND HOUR INCREMENT FROM
C   STANDARD INPUT AND WRITES A REAL INCREMENTED DATE TO STANDARD
C   OUTPUT. USEFUL FOR COMPUTING ENDPOINTS OF A TIME WINDOW GIVEN
C   A CENTER POINT AND RADIUS.
C
C PROGRAM HISTORY LOG:
C 1996-09-06  J. WOOLLEN  ORIGINAL VERSION FOR IMPLEMENTATION
C 1998-11-01  J. WOOLLEN  CONVERT TO F90
C 1999-06-03  D. KEYSER   MODIFIED TO PORT TO IBM SP AND RUN IN 4 OR
C                         8 BYTE STORAGE
C 2012-11-20  J. WOOLLEN  INITIAL PORT TO WCOSS -- NO LOGIC CHANGES
C 2013-01-13  J. WHITING  FINAL PORT TO WCOSS -- UPDATED DOC BLOCKS
C                           (NO LOGIC CHANGES)
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - (STDIN) REAL DATE AND INCREMENT
C
C   OUTPUT FILES:
C     UNIT 06  - (STDOUT) INCREMENTED DATE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - NONE
C     LIBRARY:   - NONE                
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C REMARKS: BECAUSE THIS PROGRAM IS DESIGNED TO RETURN ITS OUTPUT TO
C          A SCRIPT VARIABLE NO CALLS TO W3TAGE OR W3TAGB SHOULD BE
C          INCLUDED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  WCOSS
C
C$$$
C-----------------------------------------------------------------------
      PROGRAM RADDATE
 
      DIMENSION   MON(12)
      REAL(8)     ADATE,BDATE
 
      DATA MON/31,28,31,30,31,30,31,31,30,31,30,31/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      READ(5,*) ADATE,DHOUR

C  ONE WAY OR ANOTHER PARSE A TEN DIGIT DATE INTEGER
C  -------------------------------------------------
 
      KDATE = NINT(ADATE)
      IDATE = I4DY(KDATE)
      IY = MOD(IDATE/1000000,10000)
      IM = MOD(IDATE/10000  ,100  )
      ID = MOD(IDATE/100    ,100  )
      HR = MOD(ADATE        ,100._8 ) + DHOUR
      IF(MOD(IY    ,4).EQ.0) MON(2) = 29
      IF(MOD(IY/100,4).NE.0) MON(2) = 28
 

1     IF(HR.LT.0) THEN
         HR = HR+24
         ID = ID-1
         IF(ID.EQ.0) THEN
            IM = IM-1
            IF(IM.EQ.0) THEN
               IM = 12
               IY = IY-1
            ENDIF
            ID = MON(IM)
         ENDIF
         GOTO 1
      ELSEIF(HR.GE.24) THEN
         HR = HR-24
         ID = ID+1
         IF(ID.GT.MON(IM)) THEN
            ID = 1
            IM = IM+1
            IF(IM.GT.12) THEN
               IM = 1
               IY = IY+1
            ENDIF
         ENDIF
         GOTO 1
      ENDIF
 
      BDATE = IY*1000000 + IM*10000 + ID*100
      BDATE = BDATE + HR
      PRINT'(F13.2)',BDATE
 
      STOP  
      END




