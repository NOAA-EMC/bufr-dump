C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_EDTBFR
C   PRGMMR: DONG             ORG: NP22        DATE: 2021-09-02
C
C ABSTRACT: APPLIES REAL-TIME INTERACTIVE QUALITY CONTROL FLAGS,
C   GENERATED FROM EITHER A "REJECT" LIST MAINTAINED BY NCEP/NCO OR
C   FROM DECISIONS MADE BY THE THE NCEP/NCO SENIOR DUTY METEOROLOGIST
C   (SDM), TO ALL TYPES OF SURFACE LAND (BUFR MESSAGE TYPE 000),
C   SURFACE MARINE (BUFR MESSAGE TYPE 001), UPPER-AIR (BUFR MESSAGE
C   TYPE 002), AIRCRAFT (BUFR MESSAGE TYPE 004), SATELLITE-DERIVED WIND
C   (BUFR MESSAGE TYPE 005) OR MESONET (BUFR MESSAGE TYPE 255) REPORT
C   DATA AS IT IS BEING DUMPED FROM THE /dcom BUFR DATABASE IN THE
C   VARIOUS NCEP NETWORKS.  THE DATA TIME WINDOW AND THE BUFR INPUT
C   FILE NAMES TO CHECK ARE READ FROM STANDARD INPUT.  ALL BUFR FILE
C   CONNECTIONS (EXCEPT STANDARD INPUT WHICH IS PRE-CONNECTED) ARE MADE
C   THROUGH THE FORTRAN OPEN STATEMENT.
C
C PROGRAM HISTORY LOG:
C 1997-02-01  J. WOOLLEN -- ORIGINAL AUTHOR
C 1997-02-27  J. WOOLLEN -- FIXED COPY TO PROCESS THE DATE MESSAGES
C       CORRECTLY
C 1998-10-19  J. WOOLLEN -- UPDATED SATWIND DATA PROCESSING
C 1998-12-02  J. WOOLLEN -- Y2K/F90 COMPLIANT
C 1999-06-03  D. KEYSER  -- PURGED PRESS. Q.M. NOW ENCODED AS 14 AND
C       REJECT LIST PRESS. Q.M. NOW ENCODED AS 12 (LIKE ALL OTHERS) NOW
C       THAT QMPR IS STORED IN 4 BITS; MODIFIED TO PORT TO IBM SP AND
C       RUN IN 4 OR 8 BYTE STORAGE
C 1999-11-16  J. WOOLLEN -- FIXED EDITING LONGITUDE TO BE -180<=x<=180
C 2000-12-07  B. FACEY   -- ADDED CAPABILITY TO EDT TYPE SHP
C 2004-02-02  D. KEYSER  -- ADDED FLEXIBILITY IN READING AND CHECKING
C       INPUT FILE NAMES; REPLACED CALLS TO "BORT" WITH CALLS TO
C       "ERREXIT"; STREAMLINED CODE; IMPROVED DOCBLOCKS AND COMMENTS;
C       ADDED MORE DESCRIPTIVE STANDARD OUTPUT PRINT; ADDED STATUS FILE
C       IN UNIT 60 THAT IS WRITTEN TO ONLY WHEN THIS PROGRAM COMPLETES
C       SUCCESSFULLY (TRANSF. TO DUMPJB SCRIPT); NOW CHECKS TO SEE IF
C       EACH DATE ENTRY IN FLAG FILE IS BETWEEN EXACT TIME WINDOW
C       SPECIFICATION (OUT TO FRACTION OF HOUR AT EACH END) RATHER THAN
C       BETWEEN NINT OF TIME WINDOW AT EACH END; DATE TOLERANCE FOR
C       SATELLITE-DERIVED WINDS {ABS(OBS. DAT - ENTRY DATE)} INCREASED
C       TO 2.01 HOURS IN ORDER TO MARK ALL WINDS IN THE DUMP (STILL
C       0.01 HOURS FOR ALL OTHER TYPES); CORRECTED ERROR IN CALCULATING
C       DATE TOLERANCE WHEN .GE. 1.00 HOURS; ONLY ATTEMPTS TO ENCODE
C       NEW Q.M.`S (STRING PQMST) IF THERE ARE .GT. 0 UPPER-AIR LEVELS
C       READ IN (PREVENTS BUFRLIB WARNING MESSAGE FROM BEING PRINTED)
C 2004-05-17  D. KEYSER  -- NO LONGER ABORTS IF PRESSURE LEVEL INFO IN
C       AN ENTRY IS ENTERED INCORRECTLY, IN FACT NOW CHECKS FOR VALID
C       "NUMBERS" IN THE DATE, LAT, LON, AS WELL AS THE PRESSURE(S) (IN
C       CASES WHEN THE VALID NUMBER OF WILD CARD "-" CHARACTERS ARE NOT
C       FOUND) AND IF ANY VALUES ARE NON-NUMERIC THE CODE WILL SKIP THE
C       ENTRY, SEND A MESSAGE TO THE PRODUCTION JOBLOG FILE TO ALERT
C       THAT THERE IS A CORRUPT ENTRY IN THE SDMEDIT FLAG FILE, AND
C       THEN CONTINUE PROCESSING THE NEXT RECORD (ENTRY) IN THE FLAG
C       FILE; IN THE EVENT THE PROGRAM TERMINATES ABNORMALLY, A
C       MESSAGE IS NOW POSTED TO THE PRODUCTION JOBLOG FILE
C 2004-08-25  D. KEYSER  -- PROCESSING GENERALIZED TO ALLOW FOR WILD
C       CARD ENTRIES IN REPORT ID LOCATION IN SDMEDIT FLAG FILE ENTRIES
C       (IF AN "*" IS FOUND IN FLAG FILE ID ENTRY, ALL REPORTS
C       BEGINNING WITH CHARACTERS MATCHING THOSE PRIOR TO "*" IN FLAG
C       FILE ARE CONSIDERED TO HAVE A MATCHING REPORT ID); ADDED READ
C       OF BUFR MESSAGE TYPE (IN THE FORM "NCtttsss") FROM COLUMNS 68-75
C       OF EACH SDMEDIT FLAG FILE ENTRY - IF FOUND (I.E, NOT "--------"
C       OR "        "), THEN AN ADDITIONAL REQUIREMENT FOR A MATCH IS
C       THAT THE REPORT BE FROM THIS BUFR MESSAGE TYPE
C 2005-03-24  D. KEYSER  -- INCREASE THE SIZE FOR MXTB FROM 50000 TO
C       300000 SO DUMPS OVER VERY WIDE TIME WINDOWS (E.G., 24-HOURS)
C       CONTAINING MORE REPORTS WILL NOT HIT THE LIMIT
C 2006-04-18  D. KEYSER  -- MODIFIED TO PROPERLY HANDLE E-AMDAR AND
C       CANADIAN AMDAR AIRCRAFT (HI-ACCURACY LAT/LON, STN. ID FROM
C       "ACRN"), TAMDAR (from MADIS) AIRCRAFT (HI-ACCURACY LAT/LON, STN.
C       ID FROM "ACID", MOISTURE AVAILABLE); UPDATED PRINT TO INCLUDE
C       "NEW" SUBTYPES; LAT/LON TOLERANCE CHECK SET TO 0.01 FOR TYPES
C       WHICH ENCODE HIGH-ACCURACY LAT/LON (LAT/LON TOLERANCE REMAINS
C       0.25 FOR TYPES ENCODING LOW-ACCURACY LAT/LON); WILL
C       AUTOMATICALLY ASSUME A TYPE ENCDOES HIGH-ACCUARACY LAT/LON IF
C       LOW-ACCURACY LAT/LON IS MISSING
C 2007-03-23  D. KEYSER   INTRODUCED ALLOCATABLE ARRAYS TO AVOID ARRAY
C     OVERFLOW PROBLEMS, DETERMINES SIZE OF ARRAYS BY CALLING UFBTAB
C     WITH NEGATIVE UNIT NUMBER TO SIMPLY COUNT SUBSETS
C 2008-06-10  D. KEYSER   REPLACED CALL TO BUFRLIB ROUTINE PARSEQ WITH
C     CALL TO BUFRLIB ROUTINE PARSTR {PARSEQ HAS BEEN REPLACED WITH
C     PARSTR IN LATEST (28 MAY 2008) VERSION OF THE BUFRLIB, PARSEQ HAS
C     BEEN MARKED AS OBSOLETE}
C 2011-09-30  D. KEYSER   IMPROVED COMMENTS; STREAMLINED CODE; ACCOUNTS
C     FOR SHIP REPORTS NOW (OR SOON) BEING SPLIT INTO TWO TANKS,
C     b001/xx001 (NOW ONLY RESTRICTED) AND NEW b001/xx013
C     (UNRESTRICTED), ALSO ACCOUNTS FOR TAMDAR (from MADIS) PENAIR IN
C     b004/xx012 AND TAMDAR (from MADIS) CHAUTAUQUA IN b004/xx013; AS A
C     RESULT OF LATEST BUFRLIB WHICH CAN HANDLE EMBEDDED DICTIONARY
C     MESSAGES, INCREASES AMOUNT OF BUFRLIB PRINTOUT DURING (ONLY) THE
C     POINT WHEN READING IN MESSAGES IN ORDER TO DETECT ANY EMBEDDED
C     DICTIONARY MESSAGES; ADDED MORE OPTIONS FOR WILD CARD ENTRIES IN
C     REPORT ID LOCATION IN SDMEDIT FLAG FILE ENTRIES: "*" IN CHARACTER
C     1 FOLLOWED BY 7 BLANK CHARACTERS - ALL REPORTS ARE CONSIDERED TO
C     HAVE A MATCHING REPORT ID (SUBJECT TO CONDITIONS), "*" IN
C     CHARACTER 1 FOLLOWED BY 1-7 VALID CHARACTERS - ALL REPORTS ENDING
C     WITH THE CHARACTERS MATCHING THOSE AFTER "*" IN FLAG FILE ARE
C     CONSIDERED TO HAVE A MATCHING REPORT ID, ONE OR MORE "?"`S IN ANY
C     CHARACTER - ALL REPORTS MATCHING NON-"?" CHARACTERS IN FLAG FILE
C     AND WITH ANY VALID CHARACTER IN "?" POSITION(S) ARE CONSIDERED TO
C     HAVE A MATCHING REPORT ID (THIS ALSO APPLIES FOR REPORTS IN THE
C     FLAG FILE BEGINNING WITH, OR ENDING WITH, "*"); ADDED OPTION TO
C     ENTER ONLY THE HOUR IN TIME LOCATION IN SDMEDIT FLAG FILE ENTRIES,
C     ALL REPORTS WHICH MATCH THE HOUR ARE CONSIDERED TO HAVE A MATCH IN
C     TIME REGARDLESS OF THEIR YEAR, MONTH OR DAY (ALLOWS REPORTS FOR
C     ONLY ONE CYCLE EACH DAY, E.G. 00 OR 12Z, TO BE PUT ON THE REJECT
C     LIST); ADDED OPTION TO ENTER ONLY THE YEAR, MONTH AND DAY IN TIME
C     LOCATION IN SDMEDIT FLAG FILE ENTRIES, ALL REPORTS WHICH MATCH
C     THE YEAR, MONTH AND DAY ARE CONSIDERED TO HAVE A MATCH IN TIME
C     REGARDLESS OF THEIR HOUR (ALLOWS REPORTS FOR MANY DIFFERENT
C     CYCLES ON THE SAME DAY TO BE PUT ON THE REJECT LIST); ADDED READ
C     OF WMO BULLETIN HEADER AND ORIGINATOR (IN THE FORM "TTAAii CCCC")
C     FROM COLUMNS 78-88 OF EACH SDMEDIT FLAG FILE ENTRY - IF FOUND
C     (I.E, NOT "-----------" OR "           "), THEN AN ADDITIONAL
C     REQUIREMENT FOR A MATCH IS THAT THE REPORT HAVE THIS WMO BULLETIN
C     HEADER AND ORIGINATOR; ADDED READ OF LAT/LON BOUNDARY (IN THE
C     FORM "sssnnneeewww") FROM COLUMNS 91-102 OF EACH SDMEDIT FLAG
C     FILE ENTRY - IF FOUND (I.E, NOT "------------" OR "            "),
C     THEN AN ADDITIONAL REQUIREMENT FOR A MATCH IS THAT THE REPORT
C     HAVE A LATITUDE AND LONGITUDE INSIDE THIS BOUNDARY; ADDED READ OF
C     UPPER-AIR BUFR INSTRUMENT TYPE FOR UPA TYPES ONLY (IN THE FORM
C     "ttt") FROM COLUMNS 105-107 OF EACH SDMEDIT FLAG FILE ENTRY - IF
C     FOUND (I.E, NOT "---" OR "   "), THEN AN ADDITIONAL REQUIREMENT
C     FOR A MATCH IS THAT THE UPA REPORT HAVE THIS UPPER-AIR INSTRUMENT
C     TYPE; ADDED READ OF NETWORK(S) - EITHER THOSE (UP TO FOUR) TO BE
C     CONSIDERED FOR MATCHING, OR A SINGLE NETWORK TO NOT BE CONSIDERED
C     FOR MATCHING - FROM COLUMNS 110-128 OF EACH SDMEDIT FLAG FILE
C     ENTRY - IF FOUND (I.E, NOT "-------------------" OR
C     "                   "), THEN AN ADDITIONAL REQUIREMENT FOR A
C     MATCH IS THAT THE REPORT EITHER BE FROM ONE OF UP TO FOUR
C     NETWORKS LISTED IN COLUMNS 110-128 (WHERE THE REPORT`S NETWORK IS
C     DEFINED BY THE ENVIRONMENT VARIABLE "NET" SET IN THE PARENT
C     SCRIPT) OR, IF COLUMN 110 IS SET TO "!", THAT THE REPORT NOT BE
C     FROM THE SINGLE NETWORK LISTED IN COLUMNS 111-114; THE OPTION TO
C     SPECIFY A RANGE OF PRESSURES OVER WHICH TO APPLY QUALITY MARKERS
C     (COLUMNS 55-65 OF EACH SDMEDIT FLAG FILE ENTRY) IS EXPANDED TO
C     INCLUDE AIRCRAFT REPORTS (REPORT TYPE "ACF" IN COLUMNS 37-39)
C     {BEFORE IT ONLY APPLIED TO UPPER-AIR REPORTS (REPORT TYPE "UPA"
C     IN COLUMNS 37-39)}, FOR THOSE AIRCRAFT TYPES WHICH DO NOT REPORT
C     PRESSURE, THE STANDARD ATMOSPHERE PRESSURE IS CALCULATED FROM THE
C     REPORTED HEIGHT (PRESSURE- ALTITUDE) AND IS USED TO COMPARE
C     AGAINST THE PRESSURE RANGE IN COLUMNS 55-65
C 2011-10-17  D. A. KEYSER -- RAP (RAPID REFRESH) NETWORK REPLACES
C     RUC2A (RAPID UPDATE CYCLE)
C 2012-11-20  J. WOOLLEN  INITIAL PORT TO WCOSS -- ADAPTED IBM/AIX
C     GETENV SUBPROGRAM CALL TO INTEL/LINUX SYNTAX; EXPANDED EDIT
C     DESCRIPTORS TO ACCOMODATE RECOMMENDED FIELD WIDTHS (FORMAT 200)
C 2013-01-22  J. WHITING  FINAL PORT TO WCOSS -- UPDATED DOC BLOCKS;
C     READY FOR IMPLEMENTATION.
C 2013-12-12  D. A. KEYSER -- RECOGNIZES NEW LEVEL 2 DECODER VAD WINDS
C     IN MESSAGE TYPE 002, SUBTYPE 017
C 2014-03-05  D. A. KEYSER -- INCREASED MAXIMUM NUMBER OF TIME- AND
C     REPORT TYPE-RELEVANT ENTRIES ALLOWED IN THE SDMEDIT FLAG FILE
C     FROM 1000 TO 2000 (PARAMETER "MEDT")
C 2015-01-28  D. A. KEYSER -- RECOGNIZES NEW NPP/VIIRS IR(LW) POES
C     WINDS IN MESSAGE TYPE 005, SUBTYPE 090
C 2015-03-06  D. A. KEYSER -- INCREASED THE MAXIMUM NUMBER OF ID
C     MATCHES THAT CAN BE LISTED IN STDOUT WHEN A WILDCARD ID ENTRY IS
C     SPECIFIED FROM 3000 TO 3500 (DUE TO > 3000 MEXICAN MDCRS REPORTS
C     CURRENTLY FLAGGED VIA AN ENTRY WITH A WILDCARD ID); THE PRINT
C     STATEMENT STOPS LISTING ID`S IF THIS MAXIMUM IS HIT AND PRINTS
C     A WARNING MESSAGE (BEFORE, NO WARNING AND ARRAY OVERFLOW OCCURRED
C     FOR THE MEXICAN MDCRS LISTING).  THE WILDCARD ID-MATCH LISTING
C     NOW INCLUDES A PRINT OF THE COMPLETE SDMEDIT ENTRY CARD (TO
C     IDENTIFY THE WILDCARD ID-MATCH LISTING FOR CASES WHERE THERE IS
C     MORE THAN ONE SUCH ENTRY IN THE SDMEDIT FILE).
C 2016-01-14  D. A. Keyser --  Recognizes new Korean AMDAR aircraft
C     (raw BUFR) in message type 004, subtype 011 and new "Catch-all"
C     AMDAR aircraft (raw BUFR) in message type 004, subtype 103.
C 2016-08-09  S. Melchior/D. Keyser
C         - Recognizes new TAMDAR aircraft (raw BUFR) from Panasonic
C           (as well as historical data from AirDAT) in message type
C           004, subtype 010.
C         - Updated nomenclature for several existing aircraft types.
C         - Corrected comments re: which aircraft types contain
C           moisture and what type of moisture observation is reported.
C         - Included E-AMDAR (BUFR type 004, subtype 006), TAMDAR (from
C           Panasonic, or before that AirDAT, 004, 010), PenAir TAMDAR
C           (from MADIS, 004, 012), Chautauqua TAMDAR (from MADIS, 004,
C           013) and "Catch-all" AMDAR (004, 103) to types of aircraft
C           reports which can encode a moisture QM from an sdmedit flag
C           text file entry.
C         - Corrected comments re: which aircraft types use which
C           height mnemonic when a pressure must be calculated from
C           reported "height", and which aircraft types actually report
C           pressure. (Needed when obs within a specified pressure
C           layer are considered based on the sdmedit flag text file
C           entry.)
C         - Corrected oversight in 2016-01-14 update whereby code was
C           not obtaining the reported height (in the form of mnemonic
C           FLVLST) for Korean and "Catch-all" AMDAR aircraft reports.
C           This is needed when a pressure must be calculated from
C           reported "height" when obs within a specified pressure
C           layer are considered based on the sdmedit flag text file
C           entry.
C         - Corrected oversight in 2016-01-14 update whereby code was
C           not obtaining the id (in this case ACRN) from Korean and
C           "Catch-all" AMDAR aircraft reports.
C         - Corrected oversight in 2011-09-30 update whereby code was
C           not obtaining the id (in this case ACID) from PenAir and
C           Chautauqua TAMDAR (from MADIS) aircraft reports. (These are
C           no longer available so this fix solely for historical
C           reruns.)
C         - Check for cases where operator descriptors transform
C           mnemonics CLAT/CLON from low- to high-accuracy (use BUFRLIB
C           routine NEMSPECS to check scale for CLAT in first subset in
C           each file read in).  In this case, use lat/lon matching
C           tolerances designed for high-accuracy lat/lon (just as
C           though reports used CLATH/CLONH instead of operator
C           descriptors on CLAT/CLON.
C           BENEFIT: The V7 BUFR MDCRS in NC004004 (coming in fall 2016)
C                    use operator descriptors to increase precision of
C                    CLAT/CLON. Need this change to use proper lat/lon
C                    tolerances in the matching process.
C         - Replaced explicit assignment of BMISS with GETBMISS
C           function.
C 2016-08-15  D. A. Keyser --  Added ability to apply QC flags to
C     mesonet reports originally in the b255 tanks (that eventually go
C     into the "msonet" dump). These are entered in the sdmedit flag
C     text file with report type "MSO".
C 2016-11-10  D. A. Keyser --  Corrected a bug in code. If an sdmedit
C     flag file entry has a report id of "*       " and its lat/lon
C     range and upper-air instrument type fields are set to all dashes
C     "-"  but its WMO bulletin header field is specified, all reports
C     read in with a matching "TYP" that had a missing WMO bulletin
C     header had ended up matching this entry and their quality markers
C     were updated based on those specified in this flag file entry
C     (currently only mesonet and TAMDAR aircraft reports have a missing
C     WMO bulletin header). These no longer match.
C 2017-11-27  D. A. Keyser --  Recognizes new GOES-16 derived motion
C     wind types in message type 005 (IR/long-wave in subtype 030, WV
C     imager/deep-layer in subtype 031, visible in subtype 032, WV
C     imager/cloud-top in subtype 034 and IR/short-wave in subtype 039).
C 2018-02-21  D. A. KEYSER --  Modified to handle BUFR-feed upper-air
C     reports in tanks b002/xx101-xx105:
C        - Output message length increased from 10K (default) TO 200K
C          via call to BUFRLIB subroutine MAXOUT.
C        - Maximum number of data values in an uncompressed BUFR subset
C          (MAXSS) increased from 80K (default) to 300K via use of
C          BUFRLIB function ISETPRM (note: must link to dynamic
C          allocation version of BUFRLIB).
C        - Subroutine APPLY:
C           - Increased arrays holding level data from 255 to 9000 (to
C             account for high vertical resolution in many reports now
C             in these tanks).
C           - Reads mnemonic "VSIGX" from these tanks rather than "VSIG"
C             which is only in TAC-feed tanks, and then translates its
C             value into "VSIG" value associated with "SURF", "MAND",
C             "SIGT" and "SIGW" levels (for editing Q.M. of levels by
C             their vertical significance qualifier).
C           - Increased amount of temporary diagnostic print.
C
C 2021-09-02  D. STOKES -- COMMENTED SYSTEM CALLS OF POSTMSG TO JLOGFILE
C 2022-08-15  I. GENKOVA -- INCREASED MEDT SIZE TO ACCOMODATE SDMEDIT
C
C USAGE
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT - FIRST RECORD CONTAINS TIME-WINDOWING
C                SPECIFICATIONS (THE YYYYMMDDHH<.HH> DATE OF THE
C                EARLIEST TIME TO DUMP AND THE YYYYMMDDHH<.HH> DATE OF
C                THE LATEST TIME TO DUMP), SECOND THROUGH N`TH RECORDS
C                CONTAIN THE WORKING INPUT FILE NAMES FOR ALL DUMP
C                TYPES BEING COMBINED INTO A SINGLE DUMP FILE
C     UNIT 20  - SDMEDIT FLAG TEXT FILE (SEE REMARKS)
C     UNIT 21  - BUFR FILE(S) PRIOR TO QUALITY CONTROL
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 21  - BUFR FILE(S) AFTER QUALITY CONTROL
C     UNIT 50  - WORKSPACE (SCRATCH) FILE(S)
C     UNIT 60  - STATUS FILE WHOSE PRESENCE INDICATES THIS PROGRAM
C                COMPLETED SUCCESSFULLY
C
C   WORK FILES:
C     UNIT 91  - SCRATCH FILE CONTAINING CONTENTS OF FLAG FILE ENTRY
C                FOR CASES OF A CORRUPT ENTRY WHERE MESSAGE IS POSTED
C                TO THE PRODUCTION JOBLOG FILE VIA POSTMSG
C
C   SUBPROGRAMS CALLED:
C     UNIQUE     - APPLY    PRSRNG   IQMF
C     SYSTEM     - SYSTEM   GETENV
C     LIBRARY:
C       W3NCO    - W3TAGB   W3TAGE   ERREXIT  W3DIFDAT
C       BUFRLIB  - DATELEN  OPENBF   COPYMG   UFBTAB   OPENMB
C                  COPYBF   COPYSB   CLOSMG   CLOSBF   IREADMG
C                  IREADSB  UFBCPY   WRITSB   UFBINT   MESGBC
C                  PARSTR   NMSUB    IBFMS    CAPIT    NEMSPECS
C                  GETBMISS ISETPRM  IGETPRM  MAXOUT
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C            > 0 - ABNORMAL RUN
C
C REMARKS:
C     Contents of each 128-character "record" (line entry) in SDMEDIT
C     flag text file
C     (Note: Anything in character 129 or after in the SDMEDIT flag
C            text file is ignored by this program.)
C        Column        Description
C        ------        -----------
C        01-08         Reported call sign, WMO id, tail number {MDCRS
C                      aircraft, E-AMDAR aircraft, Canadian AMDAR
C                      aircraft, TAMDAR (Panasonic or AirDAT) aircraft,
C                      Korean AMDAR aircraft, "Catch-all" AMDAR (raw
C                      BUFR) aircraft}, flight number (all other
C                      aircraft), or "SWsssnnn" (satellite-derived
C                      winds, where "sss" is BUFR message subtype and
C                      "nnn" is BUFR code table value for satellite
C                      number) - Left justified, blank right fill
C                      Wild card value can also be entered as follows:
C                        An asterisk ('*') in column 1 followed by
C                         blanks in columns 2-7:
C                           - All id's will be considered, but only if
C                             one or more of the following three fields
C                             are filled (i.e. they do not contain
C                             either all blanks or all dashes):
C                               Characters 78-88:
C                                 WMO bulletin header and originator
C                               Characters 91-102:
C                                 Lat/lon boundary for considering rpts
C                               Characters 105-107:
C                                 Upper-air instrument type
C                             Otherwise, NO ids will be considered
C                              (this prevents an accidental typo in an
C                               entry from causing every report in a
C                               dump file to potentially contain
C                               held, purged or rejected observations)
C                        Valid characters in columns 1-X (where X < 8)
C                         followed by an asterisk ('*') in column X+1:
C                           - All id's with the same leading characters
C                             as in columns 1-X will be considered
C                             (e.g., if characters 1-8 are "FAA42*  ',
C                              then reports with id FAA42, FAA421,
C                              FAA42J86, and FAA4217 would be
C                              considered)
C                        An asterisk ('*') in column 1 followed by
C                         valid characters in columns 2-X (where X < 9)
C                           - All id's with the same trailing
C                             characters as those in 2-X will be
C                             considered
C                             (e.g., if characters 1-8 are "*67A    ',
C                              then reports with id 67A, AZ467A, K67A,
C                              42567A, and GH34567A would be
C                              considered)
C                        In any form of the report id above, a question
C                         mark ("?") in any one character is always
C                         matched
C                         (e.g., if characters 1-8 are "72?56   ",
C                              then reports with id 72456, 72556 and
C                              72666 will be considered; if characters
C                              1-8 are "F?A42*  ', then reports with id
C                              FAA42, FBA421, FCA42J86, and FDA4217
C                              would be considered; if characters 1-8
C                              are "AF?765?K", then reports with id
C                              AFZ765XK, AF17657K, AFP7655K and
C                              AF9765RK would be considered)
C        09-10         Spares (blanks)
C        11-15         Reported latitude (x 100 degrees, right
C                      justified, blank left fill - if "-" precedes
C                      latitude it is in Southern Hemisphere)
C                         "-----" - Latitude not entered (all latitudes
C                                   are considered)
C        16-17         Spares (blanks)
C        18-22         Reported longitude (x 100 degrees West, 0-36000)
C                      (right justified, blank left fill)
C                         "-----" - Longitude not entered (all
C                                   longitudes are considered)
C        23-24         Spares (blanks)
C        25-34         Report date in format YYYYMMDDHH (UTC)
C                         "----------" - Report date not entered (all
C                                        dates are considered)
C                         "--------HH" - Only report hour entered (all
C                                        YYYYMMDD's with this hour are
C                                        considered)
C                         "YYYYMMDD--" - Only report year, month and
C                                        day entered (all hours with
C                                        this YYYYMMDD are considered)
C                      {Note: For valid YYYYMMDDHH, all reports with
C                             HH are considered to be a match with the
C                             flag file entry (from which the time
C                             tolerance is calculated); reported
C                             minutes (if present) is NOT considered
C                             here}
C        35-36         Spares (blanks)
C        37-39         Report type
C                         "SFC" - Surface land (BUFR message type 000)
C                         "SHP" - Surface marine (BUFR message type 001)
C                         "UPA" - Upper-air (BUFR message type 002)
C                         "ACF" - Aircraft (BUFR message type 004)
C                         "SAT" - Sat.-derived wind (BUFR msg type 005)
C                         "MSO" - Mesonets (BUFR message type 255)
C        40-41         Spares (blanks)
C          42          Quality marker on pressure (see *)
C                         For SFC and MSO - station & mean sea-level
C                         For SHP - mean sea-level
C                         For UPA - station and level pressure (note:
C                                   if geopotential or height obs is
C                                   non-missing and geopot./height q.m.
C                                   in column 43 is not entered, q.m.
C                                   on geopot./height is set to that of
C                                   pressure here)
C                         For ACF and SAT - not used
C          43          Quality marker on geopotential or height (see *)
C                         For SFC, SHP, ACF, SAT and MSO - not used
C                         For UPA - if geopotential or height obs is
C                                   non-missing and this is not
C                                   entered, q.m. on geopot./height is
C                                   set to that of pressure (see value
C                                   in column 42)
C          44          Quality marker on air temperature (see *)
C                         For SAT - not used
C          45          Quality marker on moisture (see *)
C                         For SAT, ACF {AIREP (subtype 001), PIREP
C                           (002), AFWA-MDCRS (007), CANADIAN AMDAR
C                           (009), KOREAN AMDAR (011)} - not used
C                         For SFC, SHP, UPA - dewpoint temperature
C                         For ACF {E-AMDAR (subtype 006)} - mixing ratio
C                         For ACF {MDCRS (subtype 004)} - mixing ratio
C                                 or relative humidity
C                         For ACF {TAMDAR (Panasonic or AirDAT, subtype
C                           010; MADIS, 008, 012, 013)} - rel. humidity
C                         For ACF {AMDAR (subtype 003), RECCOs (005)}
C                           and MSO - dewpoint temp. or r. humidity
C                         For ACF {Catch-all AMDAR (subtype 103)} -
C                           mixing ratio or dewpoint temperature
C          46          Quality marker on wind (see *)
C        47-48         Spares (blanks)
C        49-52         For SFC, SHP, ACF, SAT, MSO - not used
C                      For UPA - Vertical significance qualifier
C                         "SURF" - Surface level
C                         "MAND" - Mandatory level
C                         "SIGT" - Significant temperature level
C                         "SIGW" - Significant wind level
C                         "----" - Vertical significance qualifier not
C                                  entered (all types of UPA levels are
C                                  considered subject to the pressure
C                                  levels in columns 55-65)
C        53-54         Spares (blanks)
C        55-65         For SFC, SHP, SAT, MSO - not used
C                      For UPA, ACF - Pressure(s) (in whole mb) over
C                        which to apply quality markers
C                          If only one pressure value is specified, it
C                            should begin in column 55 with all
C                            unfilled columns to the right remaining
C                            blank (quality markers will be applied to
C                            ONLY this one pressure level)
C                          If two pressure values are specified, the
C                            first level should begin in column 55, a
C                            single blank should separate the two
C                            values, and all unfilled columns to the
C                            right of the second value should remain
C                            blank - the order of the values does not
C                            matter {quality markers will be applied to
C                            all pressure levels bounded by (and
C                            including) these levels}
C                          If "-----------" - Pressure level(s) not
C                            entered (all pressure levels are
C                            considered, subject to the vertical
C                            significance qualifier in columns 49-52
C                            for UPA)
C                          Note: The standard atmosphere pressure is
C                                calculated and used here for all
C                                levels which report height but not
C                                pressure {e.g., winds-by-height
C                                levels for UPA; types AMDAR fmt, RECCO,
C                                AIREP/PIREP, E-AMDAR, Canadian AMDAR &
C                                TAMDAR (Panasonic and MADIS) for ACF}
C        66-67         Spares (blanks)
C        68-75         BUFR message type
C                         "NCtttsss" - where ttt is BUFR report type
C                                      and sss is BUFR report subtype
C                                      (e.g., "NC004003" is the BUFR
C                                      message type for automated
C                                      AMDAR format aircraft reports)
C                         "----------" - BUFR message type not entered
C                             or         (all BUFR message types are
C                         "          "   considered)
C        76-77         Spares (blanks)
C        78-88         WMO bulletin header and originator
C                         "TTAAii CCCC" - where TTAAii is the 6
C                                         character bulletin header (4
C                                         letters and 2 numbers) and
C                                         CCCC is the bulletin
C                                         originator (4 characters)
C                                         {e.g., "IUCS59 RJTD" is a
C                                         WMO bulletin header and
C                                         originator for Japanese
C                                         IR(LW) satellite winds}
C                         "-----------" - WMO bulletin header and
C                             or          originator not entered (all
C                         "           "   WMO bulletin headers and
C                                         originators are considered)
C                      Note 1: For reports in the dump file with
C                              replicated bulletin headers and
C                              originators (e.g., reports constructed
C                              via the merging of "parts" such as sfc
C                              land METAR, all sfc marine, RAOBs,
C                              DROPs, PIBALs) only the bulletin header
C                              and originator from the first
C                              replication is compared to the value in
C                              columns 78-88
C                      Note 2: If either the reported WMO bulletin
C                              header or originator in a report in the
C                              dump file is missing, then the value for
C                              TTAAii CCCC in columns 78-88 is ignored
C                              (TTAAii CCCC is not tested)
C        89-90         Spares (blanks)
C        91-102        Latitude/longitude boundary for considering
C                      reports
C                         "sssnnneeewww" - Lat/lon boundary where:
C                                            sss is southern latitude
C                                                   limit in degrees
C                                                   (N+, S-)
C                                            nnn is northern latitude
C                                                   limit in degrees
C                                                   (N+, S-)
C                                            eee is eastern longitude
C                                                   limit in degrees
C                                                   (0.-360. West)
C                                            www is western longitude
C                                                   limit in degrees
C                                                   (0.-360. West)
C                                            (e.g., " 10 50 70129" or
C                                                   "010050070129" or
C                                                   "+10+50 70129" all
C                                            consider all reports in
C                                            boundary 10N-50N lat and
C                                            70W-129W lon
C                                                 - or -
C                                                   "-09030160200" or
C                                                   " -9+30160200" or
C                                                   "-09 30160200" all
C                                            consider all reports in
C                                            boundary 9S-30N lat and
C                                            160W-200W lon)
C                      Note: This is applicable only if a specific
C                            latitude and longitude are NOT specified
C                            in columns 11-15 and 18-22, respectively
C                            (if they are, then the value in columns
C                            91-102 is ignored)
C        103-104       Spares (blanks)
C        105-107       For SFC, SHP, ACF, SAT, MSO - not used
C                      For UPA -
C                         "ttt" - Upper-air instrument type (see BUFR
C                                 Code Table 0-02-011 for definition of
C                                 ttt)
C                         "---" - Upper-air instrument type not
C                           or    entered (all upper-air instrument
C                         "   "   types considered)
C                      Note 1: The value for ttt is ignored for SFC,
C                              SHP, ACF, SAT, MSO (ttt is not tested)
C                      Note 2: If the reported upper-air instrument
C                              type in a report in the dump file is
C                              missing, then the value for ttt is
C                              ignored for UPA (ttt is not tested)
C        108-109       Spares (blanks)
C        110-128
C            If 110 is "!":
C              111-128   Network (only 1) for NOT considering reports
C                         (based on first four characters in exported
C                         value for "NET" in parent script, left
C                         justified) - ALL networks except this one
C                         will be considered for this entry
C                         "------------------" - No networks entered
C                                  or             (all networks are
C                         "                  "    considered)
C                               otherwise
C                         "NETX              " - where:
C                                            NETX is network for NOT
C                                                   considering reports
C                         where current choices for NETX are (must be
C                          upper-case!):
C                           "HOUR" - hourly network
C                           "CDAS" - CDAS network
C                           "CFS " - CFS network
C                           "DUMP" - dump monitor network
C                           "GDAS" - GDAS network
C                           "GFS " - GFS network
C                           "GODA" - GODAS network
C                           "NAM " - NAM/NDAS network
C                           "RTMA" - RTMA network
C                           "URMA" - URMA network
C                           "RAP " - Rapid Refresh network
C                           "RUCS" - Surface RUC network
C                          The network should be placed in columns
C                            111-114 (left justified if less than 4
C                            characters) with all unfilled columns to
C                            the right (i.e., 115-128) remaining blank
C                          The maximum number of networks that can be
C                            entered here is one
C                        NOTE: If NET is not set or exported in the
C                              parent script, then all networks are
C                              considered regardless of the network
C                              listed in columns 111-114
C            Otherwise if 110 is NOT "!":
C              110-128   Networks (up to 4) for considering reports
C                         (based on first four characters in exported
C                         value for "NET" in parent script, left
C                         justified) - any networks not in this list
C                         will NOT be considered for this entry
C                         "-------------------" - No networks entered
C                                  or             (all networks are
C                         "                   "    considered)
C                               otherwise
C                         "NET1 NET2 NET3 NET4" - Up to 4 networks
C                                                 where:
C                                            NET1 is first network for
C                                                   considering reports
C                                            NET2 is second network for
C                                                   considering reports
C                                            NET3 is third network for
C                                                   considering reports
C                                            NET4 is fourth network for
C                                                   considering reports
C                         where current choices for NET1, NET2, NET3,
C                          NET4 are (must be upper-case!):
C                           "HOUR" - hourly network
C                           "CDAS" - CDAS network
C                           "CFS " - CFS network
C                           "DUMP" - dump monitor network
C                           "GDAS" - GDAS network
C                           "GFS " - GFS network
C                           "GODA" - GODAS network
C                           "NAM " - NAM/NDAS network
C                           "RAP " - Rapid Refresh network
C                           "RUCS" - Surface RUC network
C                          If only one network is specified, it should
C                            be placed in columns 110-113 (left
C                            justified if less than 4 characters) with
C                            all unfilled columns to the right (i.e.,
C                            114-128) remaining blank (only this one
C                            network will be considered for this entry)
C                          If two networks are specified, they should
C                            be placed in columns 110-118 (each left
C                            justified if less than 4 characters, with
C                            a blank character in-between) with all
C                            unfilled columns to the right (i.e.,
C                            119-128) remaining blank (only these two
C                            networks will be considered for this
C                            entry)
C                          If three networks are specified, they should
C                            be placed in columns 110-123 (each left
C                            justified if less than 4 characters, with
C                            a blank character in-between) with all
C                            unfilled columns to the right (i.e.,
C                            124-128) remaining blank (only these three
C                            networks will be considered for this
C                            entry)
C                          If four networks are specified, they should
C                            be placed in columns 110-128 (each left
C                            justified if less than 4 characters, with
C                            a blank character in-between) (only these
C                            four networks will be considered for this
C                            entry)
C                          The maximum number of networks that can be
C                            entered here is four
C                        NOTE: If NET is not set or exported in the
C                              parent script, then all networks are
C                              considered regardless of the networks
C                              listed in columns 110-128
C
C        * - "H"   - Hold (keep) flag
C            "P"   - Purge flag
C            "R"   - Reject flag
C            "-"   - Quality marker not entered
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  WCOSS (Linux)
C
C$$$
      PROGRAM BUFR_EDTBFR

      PARAMETER (MEDT=2000)        ! Allows up to 2000 time- and report
                                   ! type-relevant entries in the
                                   ! SDMEDIT flag file
      PARAMETER (MXTS=10)
      PARAMETER (ISTNID_MATCH=3500) ! Allows up to 3500 stn id matches
                                    ! to be listed in stdout when a
                                    ! wildcard id entry is specified

      REAL(8),ALLOCATABLE :: TAB_8(:,:)
      REAL(8),ALLOCATABLE :: CLONH_8(:)
      REAL(8),ALLOCATABLE :: CLATH_8(:)
      INTEGER,ALLOCATABLE :: IMATCH_STNID(:,:)
      LOGICAL,ALLOCATABLE :: EDIT(:),EDIT2(:,:)

      CHARACTER*80  BFRFIL,ASTR,DSTR,BSTR,TSTR,SSTR
      CHARACTER*128 CARD,CARDS(0:6,MEDT)
      CHARACTER*40  CTYPE(0:255,0:4),CTYPE_sat(10:90,5:5),CTYPE1,
     .              CTYPE_mso(1:161,255:255)
      CHARACTER*11  BUHDOR
      CHARACTER*8   STNID,SUBSET,MSGTYP,STNID_TEST,CARDS8,CARDS8_TRUN,
     .              STNID_TRUN,CBUHD,CBORG
      CHARACTER*8   STNID_MATCH(MEDT,ISTNID_MATCH)
      CHARACTER*4   NET
      CHARACTER*3   CTYP(0:6),CTYP1,CITP

      DIMENSION    KTOT(MEDT)
      DIMENSION    IREAD(0:6,MEDT,0:2400)
      DIMENSION    ELAT(MEDT),ELON(MEDT)
      DIMENSION    DEXX(MEDT),DEXXH(MEDT),DEYY(MEDT),DEYYH(MEDT)
      DIMENSION    NEDT(0:6),KREP(0:6),LREP(0:6),DTIM(MEDT)
      DIMENSION    LYEAR(MEDT),LMNTH(MEDT),LDAYS(MEDT),LHOUR(MEDT)
      DIMENSION    JDAT(8),IDAT(8),RINC(5),IFIRST(0:6,MEDT)

      LOGICAL      SKIPIT(MEDT),LPRINT,LMATCH_LATLON

      REAL(8)      BMISS, GETBMISS
      REAL(8)      ADATE,BDATE,EDATE,UFBTAB_8,RSTNID_8,BUHD_8,BORG_8
      REAL(8)      ADATE_YYYYMMDD,BDATE_YYYYMMDD,HOUR_8,MINU_8

      EQUIVALENCE  (STNID,RSTNID_8),(CBUHD,BUHD_8),(CBORG,BORG_8)

      COMMON /UEDIT/  IVSG(MEDT),PMIN(MEDT),PMAX(MEDT)
      COMMON /COUNTS/ IPQM(0:6,0:14),IWQM(0:6,0:14),ITQM(0:6,0:14),
     .                IGQM(0:6,0:14),IMQM(0:6,0:14)
      COMMON /BUFRLIB_MISSING/BMISS

      DATA ASTR  /'ACRN CLON CLAT YEAR MNTH DAYS HOUR BUHD BORG RATP'/
      DATA DSTR  /'ACID CLON CLAT YEAR MNTH DAYS HOUR BUHD BORG RATP'/
      DATA BSTR  /'RPID CLON CLAT YEAR MNTH DAYS HOUR BUHD BORG RATP'/
      DATA SSTR  /'SAID CLON CLAT YEAR MNTH DAYS HOUR BUHD BORG RATP'/

      DATA PI180  /.0174532/
      DATA LUEDT  /20/
      DATA LUBFI  /21/
      DATA LUBFJ  /50/
      DATA DEYT   /.25/
      DATA DEYTH  /.01/

      DATA CTYPE
     ,          /'SYNOPTIC-LAND, RESTRICTED (WMO RES. 40) ', ! 000.000
     .           'SYNOPTIC - FIXED LAND                   ', ! 000.001
     .           'SYNOPTIC - MOBIL LAND                   ', ! 000.002
     .         4*'                                        ',
     .           'AVIATION - METAR                        ', ! 000.007
     .           'SURFACE PROFILER (NOAA and COOP. AGENCY)', ! 000.008
     .         2*'                                        ',
     .           'SHEF DATA                               ', ! 000.011
     .           'AVIATION-SUPPLEMENTARY CLIMATOLOGICAL DA', ! 000.012
     .       243*'                                        ',
     .         1*'                                        ',
     .           'SURFACE MARINE SHIP, RESTRICTED         ', ! 001.001
     .           'SURFACE MARINE BUOY(FM 18)(MOORED & DFT)', ! 001.002
     .           'SURFACE MARINE BUOY (FM 13) (MOORED)    ', ! 001.003
     .           'SURFACE MARINE C-MAN PLATFORM           ', ! 001.004
     .           'SURFACE TIDE GAUGE STATION (CREX FORMAT)', ! 001.005
     .           'MEAN SEA-LEVEL PRESSURE BOGUS           ', ! 001.006
     .           'SURFACE MARINE COAST GUARD STATION      ', ! 001.007
     .           'SURFACE TIDE GAUGE STATION (CMAN FORMAT)', ! 001.008
     .           'USGS RIVER/STREAM DATA                  ', ! 001.009
     .         3*'                                        ',
     .           'SURFACE MARINE SHIP, UNRESTRICTED       ', ! 001.013
     .       242*'                                        ',
     .         1*'                                        ',
     .           'RAWINSONDE - FIXED LAND                 ', ! 002.001
     .           'RAWINSONDE - MOBIL LAND                 ', ! 002.002
     .           'RAWINSONDE - SHIP                       ', ! 002.003
     .           'DROPWINSONDE                            ', ! 002.004
     .           'PIBAL                                   ', ! 002.005
     .           'OZONESONDE (LOW-RES)(FROM MET. OF. BUFR)', ! 002.006
     .           'NOAA PROFILER NETWORK (NPN) WINDS       ', ! 002.007
     .           'NeXRaD VAD WINDS FROM RADAR CODED MESSAG', ! 002.008
     .           'PROFILER WINDS ORIG. FROM IN PIBAL BULL.', ! 002.009
     .           'PROFLR SPECTRAL MOMENTS (NOAA & COOP AG)', ! 002.010
     .           'COOPERATIVE AGENCY PROFILER (CAP) WINDS ', ! 002.011
     .           'RASS TEMPERATURES (NOAA & COOP. AGENCY) ', ! 002.012
     .           'JAPANESE METEOROLOGICAL AG. PROFLR WINDS', ! 002.013
     .           'HONG KONG PROFILER WINDS                ', ! 002.014
     .           'OZONESONDE (HIGH-RES) (FROM ASCII)      ', ! 002.015
     .           'EUROPEAN PROFILER WINDS                 ', ! 002.016
     .           'NeXRaD VAD WINDS FROM LEVEL 2 DECODER   ', ! 002.017
     .        83*'                                        ',
     .           'RAWINSONDE - FIXED LAND (FROM BUFR)     ', ! 002.101
     .           'RAWINSONDE - MOBIL LAND (FROM BUFR)     ', ! 002.102
     .           'RAWINSONDE - SHIP (FROM BUFR)           ', ! 002.103
     .           'DROPWINSONDE (FROM BUFR)                ', ! 002.104
     .           'PIBAL (FROM BUFR)                       ', ! 002.105
     .       150*'                                        ',
     .         1*'                                        ',
     .           'MANUAL AIREP FORMAT AIRCRAFT            ', ! 004.001
     .           'MANUAL PIREP FORMAT AIRCRAFT            ', ! 004.002
     .           'AUTOMATED AMDAR FORMAT AIRCRAFT         ', ! 004.003
     .           'AUTOMATED MDCRS AIRCRAFT (from ARINC)   ', ! 004.004
     .           'FLIGHT LEVEL RECONNAISSANCE AIRCRAFT    ', ! 004.005
     .           'AUTOMATED E-AMDAR AIRCRAFT (raw BUFR)   ', ! 004.006
     .           'AUTO MDCRS AIRCRAFT (ARINC) (from AFWA) ', ! 004.007
     .           'AUTO TAMDAR ACFT - Mesaba (from MADIS)  ', ! 004.008
     .           'AUTOMATED CANADIAN AMDAR ACFT (raw BUFR)', ! 004.009
     .           'AUTO TAMDAR ACFT (from Panasonic,AirDAT)', ! 004.010
     .           'AUTO KOREAN AMDAR AIRCRAFT (raw BUFR)   ', ! 004.011
     .           'AUTO TAMDAR ACFT - PenAIR (from MADIS)  ', ! 004.012
     .           'AUTO TAMDAR ACFT-Chautauqua (from MADIS)', ! 004.013
     .        89*'                                        ',
     .           'AUTO CATCH-ALL AMDAR AIRCRAFT (raw BUFR)', ! 004.103
     .       152*'                                        '/

      DATA CTYPE_sat
     .          /'GOES/NESDIS IR(LW) DERIVED CLOUD MOTION ', ! 005.010
     .           'GOES/NESDIS WV IMAGER DERIVED CLD MOTION', ! 005.011
     .           'GOES/NESDIS VIS DERIVED CLOUD MOTION    ', ! 005.012
     .           'GOES/NESDIS PICT TRIP DERIVED CLD MOTION', ! 005.013
     .           'GOES/NESDIS WV SOUNDR DERIVED CLD MOTION', ! 005.014
     .           'GOES/NESDIS IR(LW) DERIVED CLOUD MOTION ', ! 005.015
     .           'GOES/NESDIS WV IMAGER DERIVED CLD MOTION', ! 005.016
     .           'GOES/NESDIS VIS DERIVED CLOUD MOTION    ', ! 005.017
     .           'GOES/NESDIS WV SOUNDR DERIVED CLD MOTION', ! 005.018
     .           'GOES/NESDIS IR(SW) DERIVED CLOUD MOTION ', ! 005.019
     .         1*'                                        ',
     .           'INSAT/INDIA IR(LW) DERIVED CLOUD MOTION ', ! 005.021
     .           'INSAT/INDIA VIS DERIVED CLOUD MOTION    ', ! 005.022
     .           'INSAT/INDIA WV IMAGER DERIVED CLD MOTION', ! 005.023
     .        17*'                                        ',
     .           'GMS/MTSAT/JMA IR(LW) DERIVED CLD MOTION ', ! 005.041
     .           'GMS/MTSAT/JMA VIS DERIVED CLOUD MOTION  ', ! 005.042
     .           'GMS/MTSAT/JMA WV IMGR DERIVED CLD MOTION', ! 005.043
     .           'GMS/MTSAT/JMA IR(LW) DERIVED CLD MOTION ', ! 005.044
     .           'GMS/MTSAT/JMA VIS DERIVED CLOUD MOTION  ', ! 005.045
     .           'GMS/MTSAT/JMA WV IMGR DERIVED CLD MOTION', ! 005.046
     .         3*'                                        ',
     .           'GMS/MTSAT/NESDIS IR(LW) DERIVED C MOTION', ! 005.050
     .           'GMS/MTSAT/NESDIS WV IMGR DERIVD C MOTION', ! 005.051
     .         9*'                                        ',
     .           'METEOSAT/EUMETSAT IR(LW) DERIVD C MOTION', ! 005.061
     .           'METEOSAT/EUMETSAT VIS DERIVED CLD MOTION', ! 005.062
     .           'METEOSAT/EUMETSAT WV IMG DERIVD C MOTION', ! 005.063
     .           'METEOSAT/EUMETSAT IR(LW) DERIVD C MOTION', ! 005.064
     .           'METEOSAT/EUMETSAT VIS DERIVED CLD MOTION', ! 005.065
     .           'METEOSAT/EUMETSAT WV IMG DERIVD C MOTION', ! 005.066
     .         3*'                                        ',
     .           'AQUA/TERRA/MODIS IR(LW) DERIVED C MOTION', ! 005.070
     .           'AQUA/TERRA/MODIS WV IMG DERIVED C MOTION', ! 005.071
     .         8*'                                        ',
     .           'NOAA/METOP/AVHRR IR(LW) DERIVED C MOTION', ! 005.080
     .         9*'                                        ',
     .           'NPP/VIIRS IR(LW) DERIVED CLOUD MOTION   '/ ! 005.090

      DATA CTYPE_mso
     .          /'MESONET/MADIS: Denver Urban Drainage    ', ! 255.001
     .           'MESONET/MADIS: NFIC Remote Auto. Wx Stns', ! 255.002
     .           'MESONET/MADIS: MesoWest (many subprov.) ', ! 255.003
     .           'MESONET/MADIS: Auto Pos Rpting Sys WxNet', ! 255.004
     .           'MESONET/MADIS: Kansas Dept. of Transpo. ', ! 255.005
     .           'MESONET/MADIS: Florida msonet(FAWN, USF)', ! 255.006
     .           'MESONET/MADIS: Iowa Dept. of Transpo.   ', ! 255.007
     .           'MESONET/MADIS: Minnesota Dept of Transpo', ! 255.008
     .           'MESONET/MADIS: "Anything Weather"       ', ! 255.009
     .           'MESONET/MADIS: Nat. Ocean Service-PORTS ', ! 255.010
     .           'MESONET/MADIS: Army Aberdeen Prving Grds', ! 255.011
     .           'MESONET/MADIS: "Weather for You"        ', ! 255.012
     .           'MESONET/MADIS: NWS Cooperative Observers', ! 255.013
     .           'MESONET/MADIS: NWS Hydromet Auto Dta Sys', ! 255.014
     .           'MESONET/MADIS: AWS Convergence Tech.    ', ! 255.015
     .           'MESONET/MADIS: Iowa Environmental       ', ! 255.016
     .           'MESONET/MADIS: Oklahoma Mesonet         ', ! 255.017
     .           'MESONET/MADIS: Colorado Dept. of Transpo', ! 255.018
     .           'MESONET/MADIS: West Texas Mesonet       ', ! 255.019
     .           'MESONET/MADIS: Wisconsin Dept of Transpo', ! 255.020
     .           'MESONET/MADIS: LSU * JSU (Universities) ', ! 255.021
     .           'MESONET/MADIS: Colorado East I-470      ', ! 255.022
     .           'MESONET/MADIS: DC Net (Wash, DC & NYC)  ', ! 255.023
     .           'MESONET/MADIS: Indiana Dept. of Transpo.', ! 255.024
     .           'MESONET/MADIS: Florida Dept. of Transpo.', ! 255.025
     .           'MESONET/MADIS: Alaska Dept. of Transpo. ', ! 255.026
     .           'MESONET/MADIS: Georgia Dept. of Transpo.', ! 255.027
     .           'MESONET/MADIS: Virginia Dept. of Transpo', ! 255.028
     .           'MESONET/MADIS: MO Commercial Agr. Wx Net', ! 255.029
     .           'MESONET/MADIS: Other than above         ', ! 255.030
     .           'URBANET/MADIS: UrbaNet                  ', ! 255.031
     .           'URBANET/MADIS: USouthAL                 ', ! 255.032
     .        68*'                                        ',
     .           'COOP/MADIS: NOAA Auto. NEPP, HCN-modrnzd', ! 255.101
     .           'COOP/SHEF: NWS Cooperative Observer     ', ! 255.102
     .         8*'                                        ',
     .           'MESONET/MADIS: Climate Reference Network', ! 255.111
     .        19*'                                        ',
     .           'HYDRO/MADIS: Denver Urban Drainage      ', ! 255.131
     .        28*'                                        ',
     .           'HYDRO/MADIS: Other than above           ', ! 255.160
     .           'SNOW/MADIS: Snow data frm many providers'/ ! 255.161


      DATA CTYP /'SFC','SHP','UPA','---','ACF','SAT','MSO'/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_EDTBFR',2022,0227,1100,'NP22')

      print *
      print * ,'---> Welcome to BUFR_EDTBFR - Version 09-02-2021'
      print *

C  Override current BUFRLIB maximum number of data values in an
C   uncompressed BUFR subset (80000) (due to hi-vert res raobs)
C -------------------------------------------------------------
      IRET=ISETPRM('MAXSS',300000 )  ! must use DA version of BUFRLIB
      IF(IRET.EQ.0)  THEN
        IMAXSS=IGETPRM('MAXSS')
        PRINT'(/" MAXIMUM NUMBER OF DATA VALUES IN AN UNCOMPRESSED",
     $    " BUFR SUBSET (MAXSS) SET TO ",I0)', IMAXSS
      ELSE
        PRINT'(/25("*"),"ABORT",25("*")/"ATTEMPT TO SET MAXSS FAILED ",
     $    " -- STOP 94"/25("*"),"ABORT",25("*")/)'
C       CALL CW3TAGE('BUFR_EDTBFR')
        call errexit(94)
      ENDIF

C  ASSIGN DEFAULT VALUE FOR 'MISSING' TO LOCAL BMISS VARIABLE
C  ----------------------------------------------------------

      CALL SETBMISS(10E8_8)
      BMISS = GETBMISS()     ! assign default value for "missing"
      print'(1X)'
      print'(" BUFRLIB value for missing is: ",G0)', bmiss
      print'(1X)'

      NET = '    '
      CALL GETENV('NET',NET)

      CALL CAPIT(NET)

      IF(NET.NE.'    ')  PRINT'("    -- NETWORK = ''",A4,"''"/)', NET

C     HARDWIRED PARAMETERS:
C     DEYT  = 0.25 FOR TYPES REPORTING LOW-ACCURACY LAT (0.01)
C           = 0.01 FOR TYPES REPORTING HIGH-ACCURACY LAT (>/= 0.001)
C               TOLERANCE FOR LAT CHECKS IN DEGREES (UNLESS LAT NOT
C               SPECIFIED FOR A REPORT ENTRY IN SDMEDIT FILE,
C               THEN = 10E8)
C     DEXT  = 0.25 * COS(LAT * 0.0174532) FOR TYPES REPORTING LOW-
C             ACCURACY LON (0.01)
C           = 0.01 * COS(LAT * 0.0174532) FOR TYPES REPORTING HIGH-
C             ACCURACY LON (>/= 0.001)
C               TOLERANCE FOR LON CHECKS IN DEGREES (WHERE LAT IS
C               REPORT LAT ENTRY IN SDMEDIT FILE) (UNLESS LON NOT
C               SPECIFIED FOR A REPORT ENTRY IN SDMEDIT FILE,
C               THEN = 10E8) (NOTE: IF LON IS SPECIFIED BUT LAT IS NOT
C               FOR AN ENTRY IN SDMEDIT FLAG FILE, THEN DEXT = 0.25)
C     DTMT  = 0.01 (FOR ALL TYPES EXCEPT SATELLITE-DERIVED WINDS)
C             2.01 (FOR SATELLITE-DERIVED WINDS)
C               TOLERANCE FOR TIME CHECK IN HOURS (UNLESS DATE/TIME NOT
C               SPECIFIED, THEN = 10E8)
C  ------------------------------------------------------------------

      NREP = 0
      KREP = 0
      MREP = 0
      LREP = 0
      IPQM = 0
      IWQM = 0
      ITQM = 0
      IGQM = 0
      IMQM = 0

      IFIL_CHK  = 0
      IFIL_READ = 0

      IVSG = 0
      PMIN = 0
      PMAX = BMISS

      CALL DATELEN(10)

ccccc CALL OPENBF(0,'QUIET',2) ! Uncomment for extra print from bufrlib

C  READ IN THE DATE RANGE (TIME WINDOW)
C  ------------------------------------

      READ(5,*,END=900,ERR=900) ADATE,BDATE

      PRINT 200, DEYT,DEYTH,DEYT,PI180,DEYTH,PI180,ADATE,BDATE
200   FORMAT(/2X,'ALL REPORTS IN DUMP FILE(S) ARE READ AND COMPARED TO',
     . ' REPORT ENTRIES IN SDMEDIT FLAG FILE.'/3X,'IF REPORT TYPE, ',
     . 'STATION ID, BUFR MESSAGE TYPE (in the form NCtttsss, where ttt',
     . ' is the BUFR report type and sss is the BUFR'/3X,'report ',
     . 'subtype), WMO BULLETIN HEADER AND ORIGINATOR {in the form ',
     . 'TTAAii CCCC, where TTAAii is the 6 character bulletin header'/
     . 3X,'(4 letters and 2 numbers) and CCCC is the bulletin ',
     . 'originator (4 characters)}, INSTRUMENT TYPE (FOR ',
     . 'RADIOSONDES/DROPSONDES'/3X,'ONLY), AND NETWORK (based on ',
     . 'environment variable "NET" set in the parent script) MATCH AN ',
     . 'ENTRY IN SDMEDIT FLAG FILE (SEE **),'/3X,'THEN THE REPORT WILL',
     . ' BE CONSIDERED FOR Q.C. EDITING IF ALL OF THE FOLLOWING ',
     . 'CONDITIONS ARE MET:'/7X,'1) REPORT LAT IS WITHIN',F5.2,
     . ' DEGREES OF LAT IN FLAG FILE ENTRY FOR TYPES REPORTING LOW-',
     . 'ACCURACY LAT -- or --'/10X,'REPORT LAT IS WITHIN',F5.2,
     . ' DEGREES OF LAT IN FLAG FILE ENTRY FOR TYPES REPORTING HIGH-',
     . 'ACCURACY LAT'/10X,'(IN EITHER CASE, IF LAT NOT SPECIFIED IN ',
     . 'ENTRY REPORT, LAT IS NOT CHECKED UNLESS A LAT-LON BOUNDARY IS ',
     . 'SPECIFIED IN WHICH'/10X,'CASE THE LAT MUST BE WITHIN THE ',
     . 'BOUNDARY RANGE)'/7X,'2) REPORT LON IS WITHIN',F5.2,' * COS(LAT',
     . ' * ',F10.7,') DEGREES OF LON IN FLAG FILE ENTRY FOR TYPES ',
     . 'REPORTING LOW-ACCURACY LON'/10X,'-- or --'/10X,'REPORT LON IS ',
     . 'WITHIN',F5.2,' * COS(LAT * ',F10.7,') DEGREES OF LON IN FLAG ',
     . 'FILE ENTRY FOR TYPES REPORTING HIGH-ACCURACY LON'/10X,'(WHERE,',
     . ' IN EITHER CASE, "LAT" IS REPORT LAT) (IN EITHER CASE, IF LON ',
     . 'NOT SPECIFIED IN ENTRY, REPORT LON IS NOT CHECKED'/10X,'UNLESS',
     . ' A LAT-LON BOUNDARY IS SPECIFIED IN WHICH CASE THE LON MUST BE',
     . ' WITHIN THE BOUNDARY RANGE; IF LAT NOT SPECIFIED IN'/10X,
     . 'ENTRY BUT LON IS, THEN TOLERANCE IS SAME AS FOR LAT)'/7X,'3) ',
     . 'REPORT DATE (YYYYMMDDHH.hh) IS WITHIN EITHER 2.01 (FOR ',
     . 'SATELLITE-DERIVED WINDS) OR 0.01 (ALL OTHER TYPES)'/10X,'HOURS',
     . ' OF DATE IN FLAG FILE ENTRY (IF ENTIRE DATE NOT SPECIFIED IN ',
     . 'ENTRY, REPORT DATE IS NOT CHECKED; IF ONLY REPORT HOUR'/10X,
     . 'IS SPECIFIED IN ENTRY, REPORT YYYYMMDD IS NOT CHECKED BUT ',
     . 'HH.hh IS CHECKED; IF ONLY REPORT YEAR, MONTH AND DAY IS'/10X,
     . 'SPECIFIED IN ENTRY, REPORT HH.hh IS NOT CHECKED BUT REPORT ',
     . 'YYYYMMDD IS CHECKED)'//3X,'** - IF ID IN FLAG FILE ENTRY ',
     . 'CONTAINS ONLY A WILD CARD CHARACTER "*", THEN ALL ID''S WILL ',
     . 'MATCH IT {SUBJECT TO CONDITIONS'/8X,'(SEE %%)}. IF ID IN FLAG ',
     . 'FILE ENTRY ENDS WITH A WILD CARD CHARACTER "*", THEN THE ONLY ',
     . 'REQUIREMENT FOR AN ID MATCH IS THAT'/8X,'THE BEGINNING ',
     . 'CHARACTERS OF THE REPORT ID MATCH THE FLAG FILE ID CHARACTERS ',
     . 'UP TO THE "*".  IF ID IN FLAG FILE ENTRY BEGINS'/8X,'WITH A ',
     . 'WILD CARD CHARACTER "*" THEN THE ONLY REQUIREMENT FOR AN ID ',
     . 'MATCH IS THAT THE ENDING CHARACTERS OF THE REPORT ID'/8X,
     . 'MATCH THE FLAG FILE ID CHARACTERS AFTER THE "*".  IF ANY ',
     . 'CHARACTER IN ID IN FLAG FILE ENTRY CONTAINS A WILD CARD ',
     . 'CHARACTER'/8X,'"?", THEN THAT CHARACTER MATCHES ANY CHARACTER ',
     . 'IN THAT SAME POSITION OF THE REPORT ID (OF COURSE ALL OTHER ',
     . 'NON-WILD CARD'/8X,'CHARACTERS MUST MATCH). IF BUFR MESSAGE ',
     . 'TYPE IS "--------" OR "        " (ALL BLANKS) IN THE FLAG FILE',
     . ' ENTRY, THEN IT IS'/8X,'ASSUMED TO MATCH THE REPORT''S BUFR ',
     . 'MESSAGE TYPE, WHATEVER THAT MIGHT BE.  IF WMO BULLETIN HEADER ',
     . 'AND ORIGINATOR IS'/8X,'"-----------" OR "           " (ALL ',
     . 'BLANKS) IN THE FLAG FILE ENTRY, THEN IT IS ASSUMED TO MATCH ',
     . 'THE REPORT''S WMO BULLETIN'/8X,'HEADER AND ORIGINATOR, ',
     . 'WHATEVER THEY MIGHT BE.  IF THE REPORT''S WMO BULLETIN HEADER ',
     . 'AND/OR ORIGINATOR IS MISSING, THEN IT'/8X,'IS ASSUMED TO MATCH',
     . ' THE FLAG FILE ENTRY REGARDLESS OF WHAT THE FLAG FILE ENTRY IS',
     . ' FOR WMO BULLETIN HEADER AND ORIGINATOR.'/8X,'IF UPPER-AIR ',
     . 'INSTRUMENT TYPE IS "---" OR "   " (ALL BLANKS) IN THE FLAG ',
     . 'FILE ENTRY, THEN IT IS ASSUMED TO MATCH THE'/8X,'REPORT''S ',
     . 'UPPER-AIR INSTRUMENT TYPE, WHATEVER THAT MIGHT BE.  IF THE ',
     . 'REPORT''S UPPER-AIR INSTRUMENT TYPE IS MISSING, THEN IT'/8X,
     . 'IS ASSUMED TO MATCH THE FLAG FILE ENTRY REGARDLESS OF WHAT THE',
     . ' FLAG FILE ENTRY IS FOR UPPER-AIR INSTRUMENT TYPE.  IF'/8X,
     . 'NETWORKS LISTED (UP TO FOUR) ARE "-------------------" or ',
     . '"                   " "(ALL BLANKS) IN THE FLAG FILE ENTRY, ',
     . 'THEN'/8X,'IT IS ASSUMED TO MATCH THE REPORT''S NETWORK, ',
     . 'WHATEVER THAT MIGHT BE.  IF THE PARENT SCRIPT DOES NOT SET ',
     . 'ENVIRONMENT'/8X,'VARIABLE "NET", THEN IT IS ASSUMED TO MATCH ',
     . 'THE FLAG FILE ENTRY REGARDLESS OF WHAT THE FLAG FILE ENTRY IS ',
     . 'FOR NETWORKS(S).'//3X,'%% - IF ID IN FLAG FILE ENTRY CONTAINS ',
     . 'ONLY A WILD CARD CHARACTER "*", THEN ALL ID''S WILL MATCH IT ',
     . 'ONLY IF ONE OR MORE OF THE'/8X,'FOLLOWING THREE FIELDS ARE ',
     . 'FILLED (I.E. THEY DO NOT CONTAIN EITHER ALL BLANKS OR ALL ',
     . 'DASHES):'/11X,'- CHARACTERS  78- 88: WMO BULLETIN HEADER AND ',
     . 'ORIGINATOR'/11X,'- CHARACTERS  91-102: LAT/LON BOUNDARY FOR ',
     . 'CONSIDERING REPORTS'/11X,'- CHARACTERS 105-107: UPPER-AIR ',
     . 'INSTRUMENT TYPE'///'TIME WINDOW FOR CHECKING REPORTS AGAINST ',
     . 'SDMEDIT FLAG FILE ENTRIES IS: ',F13.2,'  to  ',F13.2,'  UTC')

      LOOP1: DO IERR_COUNT=0,5  ! will repeat attempt to read cards
                                !  up to 6 times in the event of a read
                                !  error

C  READ RECORDS FROM SDMEDIT FLAG FILE, ONLY STORE THOSE W/I TIME
C  WINDOW, PARSED ACCORDING TO REPORT TYPE
C  --------------------------------------------------------------

         REWIND LUEDT
         NREC = 0
         NEDT = 0
         LOOP1n1: DO  ! read through all cards
            READ(LUEDT,'(A128)',END=4,ERR=3) CARD
            NREC = NREC+1
            IF(CARD(1:7).EQ.'STATION') CYCLE LOOP1n1 !Skip poss. hdr rec
            IF(CARD(25:34).EQ.'----------') THEN
C  .. All YYYYMMDDHHs are checked for this record
               EDATE = ADATE+0.001
            ELSE IF(CARD(25:32).EQ.'--------') THEN
C  .. All YYYYMMDDs are chckd for this record but only for a specific HH
               READ(CARD(33:34),'(I2)',ERR=801) IHOUR
               ADATE_YYYYMMDD = INT(ADATE/100)
               BDATE_YYYYMMDD = INT(BDATE/100)
               JHOUR = MOD(INT(BDATE),100)
               EDATE = NINT((ADATE_YYYYMMDD*100)+IHOUR)
               IF(NINT(BDATE_YYYYMMDD).GT.NINT(ADATE_YYYYMMDD)) THEN
                  IF(IHOUR.GE.0.AND.IHOUR.LE.JHOUR)  EDATE =
     .             NINT((BDATE_YYYYMMDD*100)+IHOUR)
               ENDIF
            ELSE IF(CARD(33:34).EQ.'--') THEN
C  .. All HHs are chckd for this record but only for a specific YYYYMMDD
               READ(CARD(25:32),'(I8)',ERR=801) IYYYYMMDD
               EDATE = 9999999999.
               IF(INT(ADATE/100).EQ.IYYYYMMDD .OR.
     .            INT(BDATE/100).EQ.IYYYYMMDD)  EDATE = ADATE+0.001
            ELSE
C  .. Only a specific YYYYMMDDHH is checked for this record
               READ(CARD(25:34),'(F10.0)',ERR=801) EDATE
            ENDIF
            IF(EDATE.GE.ADATE .AND. EDATE.LE.BDATE) THEN
C     .. This card is w/i time window - store it according to rpt type
               LOOP1n2: DO ITYP=0,6
                  IF(CARD(37:39).EQ.CTYP(ITYP)) THEN
                     IF(NEDT(ITYP)+1.LE.MEDT) THEN
                        NEDT(ITYP) = NEDT(ITYP) + 1
                        CARDS(ITYP,NEDT(ITYP)) = CARD !Store this record
                     ELSE
                        IF(NEDT(ITYP)+1.EQ.MEDT+1) PRINT'(/"#####THE ",
     .                   "NUMBER OF RECORDS IN THE SDMEDIT FLAG FILE ",
     .                   "IN UNIT",I3," THAT ARE WITHIN THE TIME ",
     .                   "WINDOW FOR REPORT"/5X,"TYPE ",A," EXCEEDS ",
     .                   "THE LIMIT OF",I6," - ALL REMAINING RECORDS ",
     .                   "FOR THIS REPORT TYPE ARE IGNORED"/)',
     .                   LUEDT,CTYP(ITYP),MEDT
                     ENDIF
                     CYCLE LOOP1n1 ! move on to next sdmedit flag file
                                   !  card
                  ENDIF
               ENDDO LOOP1n2 !  DO ITYP=0,6 ! storing cards w/i time
                                            !  window according to
                                            !  report type
            ENDIF

            CYCLE LOOP1n1 ! move on to next sdmedit flag file card

  801       CONTINUE

C  COME HERE IF THE "YYYYMMDDHH" DATE IN THE FLAG FILE ENTRY IS FOUND
C   TO NOT BE VALID -- THIS ENTRY WILL BE SKIPPED OVER WHEN EDITING IS
C   PERFORMED
C  -------------------------------------------------------------------

            PRINT'(/"#####SDMEDIT FILE CONTAINS CORRUPTED RECORD '//
     .       '(OVERALL NUMBER",I5,"): ",/A/5X,"- IGNORE AND SKIP TO '//
     .       'NEXT RECORD"/)', NREC,CARD
            REWIND(91)
            WRITE(91,'(A)') CARD
            REWIND(91)
C           CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg'//
C    .       ' "$jlogfile" "***WARNING: CORRUPT RECORD IN SDMEDIT '//
C    .       'FILE: `cat ./fort.91`"')

         ENDDO LOOP1n1 ! read through cards

3        CONTINUE

C  Error reading a card, start over (unless hit limit of 5 re-tries)
C  -----------------------------------------------------------------

         IF(IERR_COUNT.EQ.5) THEN
            PRINT'(/"#####ERRORS READING THE SDMEDIT FLAG FILE IN UNIT",
     .       I3," - SDMEDIT Q.C. FLAGS NOT APPLIED TO DUMP FILE(S)"/)',
     .      LUEDT
            GOTO 700
         ENDIF
         PRINT'(/"ERRORS READING THE SDMEDIT FLAG FILE IN UNIT",I3," -",
     .    " REWIND FILE AND TRY READING AGAIN"/)', LUEDT
      ENDDO LOOP1 ! DO IERR_COUNT=0,5 ! repeated attempts to read cards

4     CONTINUE ! All cards have been read, those w/i time window stored
               ! according to report type

      IF(NREC.EQ.0) THEN

C  Non-existent or empty flag file - all done
C  ------------------------------------------

         PRINT'(/"#####THE SDMEDIT FLAG FILE IN UNIT",I3," EITHER ",
     .    "DOESN''T EXIST OR IS EMPTY - SDMEDIT Q.C. FLAGS NOT APPLIED"/
     .    5X,"TO DUMP FILE(S)"/)', LUEDT
         GOTO 700
      ELSEIF(MAX(NEDT(0),NEDT(1),NEDT(2),NEDT(4),NEDT(5),NEDT(6)).EQ.0)
     .  THEN

C  Flag file doesn`t contain any time-relevant rec. entries - all done
C  -------------------------------------------------------------------

         PRINT'(/"#####SDMEDIT FLAG FILE IN UNIT",I3," DOESN''T ",
     .    "CONTAIN ANY TIME-RELEVANT RECORD ENTRIES -SDMEDIT Q.C. ",
     .    "FLAGS"/5X,"NOT APPL. TO DUMP FILE(S)"/)', LUEDT
         GOTO 700
      ELSE
         PRINT'(/"READ IN",I5," RECORDS FROM SDMEDIT FLAG FILE IN UNIT",
     .   I3)', NREC,LUEDT
         DO ITYP=0,6
            IF(ITYP.EQ.3) CYCLE
            PRINT'(7X,"For report type ",A,", a total of ",I5,
     .       " records are within requested time window and read into ",
     .       "memory")', CTYP(ITYP),NEDT(ITYP)
         ENDDO
      ENDIF

C  ALL TIME-RELEVANT RECORDS FROM SDMEDIT FLAG FILE READ INTO MEMORY
C  (PARSED BY REPORT TYPE) - LOOP OVER THE LIST OF BUFR DUMP FILES TO
C  CONSIDER FOR Q.C. MARKING
C  ------------------------------------------------------------------

      IFIRST = 0
      LOOP2: DO ! loop through the BUFR dump files (at this point they
                !  are split according to tank)
         IREAD = 0
         READ(5,'(A80)',END=100,ERR=901) BFRFIL  ! Read in filename
         IFIL_READ = IFIL_READ + 1
         LOOP2n1: DO I=4,10 ! Find BUFR msg type/subtype in file name
                            !  message type is associated with report
                            !  type index in cards
            IF(BFRFIL(I:I).EQ.'.') THEN
               READ(BFRFIL(I-3:I+3),'(I3,1X,I3)',ERR=902) ITYP_true,JTYP
               EXIT LOOP2n1
            ENDIF
         ENDDO LOOP2n1
         CTYP1  = '---'
         CTYPE1 = ' '
         ITYP = ITYP_true
         IF(ITYP_true.EQ.255) ITYP = 6
         IF(ITYP.GE.000 .AND. ITYP.LT.007) THEN
            CTYP1 = CTYP(ITYP)
            IF(ITYP_true.LT.005) THEN
               IF(JTYP.GE.000 .AND. JTYP.LE.255) CTYPE1 =
     .                                            CTYPE(JTYP,ITYP_true)
            ELSE IF(ITYP_true.EQ.005) THEN
              IF(JTYP.GE.010.AND.JTYP.LT.091) CTYPE1 =
     .                                         CTYPE_sat(JTYP,ITYP_true)
            ELSE IF(ITYP_true.EQ.255) THEN
              IF(JTYP.GE.001.AND.JTYP.LT.161) CTYPE1 =
     .                                         CTYPE_mso(JTYP,ITYP_true)
            ENDIF
         ENDIF
         PRINT'(//"===> NEXT BUFR FILE TO CHECK HAS TYPE=",I4.3,
     .    ", SUBTYPE=",I4.3,", SDMEDIT TYPE: ",A,5X,A)',ITYP_true,JTYP,
     .    CTYP1,CTYPE1
         IF(CTYP1.EQ.'---') THEN
            PRINT'(16X,"- SKIP THIS FILE SINCE IT IS NOT A TYPE ",
     .       "CHECKED BY SDMEDIT (BUFR MESSAGE TYPE .NE. 000,"/18X,
     .       "001, 002, 004, 005 OR 255)"/)'
            CYCLE LOOP2 ! move on to next BUFR dump file
         ENDIF
         IF(NEDT(ITYP).EQ.0) THEN
            PRINT'(/"#####THE SDMEDIT FLAG FILE IN UNIT",I3," DOESN''T",
     .       " CONTAIN ANY TIME-RELEVANT RECORD ENTRIES FOR THIS REPORT"
     .       /5X,"TYPE - SDMEDIT Q.C. FLAGS NOT APPLIED TO THIS DUMP ",
     .       "FILE")', LUEDT
            CYCLE LOOP2 ! move on to next BUFR dump file
         ENDIF

C  CONVERT IDS, LOCATIONS, TYPES AND TIMES FROM TIME- AND REPORT TYPE-
C  RELEVANT RECORDS IN SDMEDIT FLAG FILE INTO ARRAYS FOR LATER CROSS
C  CHECKING OF REPORTS IN THIS BUFR DUMP FILE (FIRST SET DEFAULTS)
C  -------------------------------------------------------------------

         ELAT  = 0
         DEYY  = BMISS
         DEYYH = BMISS
         ELON  = 0
         DEXX  = BMISS
         DEXXH = BMISS
         LYEAR = -99
         LMNTH = -99
         LDAYS = -99
         LHOUR = -99
         DTIM  = BMISS
         SKIPIT = .FALSE.

         LOOP2n2: DO M=1,NEDT(ITYP) ! loop through cards w/i time window
                                    !  for this report type
            IF(CARDS(ITYP,M)(1:2).EQ.'* ') THEN

C  IF CHARACTERS 1-8 IN THIS CARD ARE "*       ", THEN ALL REPORT ID'S
C   MATCH, MAKE SURE THAT ONE OR MORE OF THE FOLLOWING ARE FILLED (I.E.
C   THEY DO NOT CONTAIN EITHER ALL BLANKS OR ALL DASHES): WMO BULLETIN
C   HEADER AND ORIGINATOR (CHARACTERS 78-88), LAT/LON BOUNDARY FOR
C   CONSIDERING REPORTS (CHARACTERS 91-102), AND UPPER-AIR INSTRUMENT
C   TYPE (CHARACTERS 105-107) -- OTHERWISE, THIS ENTRY WILL BE SKIPPED
C   OVER WHEN EDITING IS PERFORMED (PREVENTS AN ACCIDENTAL TYPO IN AN
C   SDMEDIT FLAG FILE ENTRY FROM CAUSING EVERY REPORT IN A DUMP FILE TO
C   BE EDITED ACCORDING TO THE QUALITY MARKER INFORMATION IN THIS
C   ENTRY!)

               IF((CARDS(ITYP,M)(78:88).EQ.'-----------'          .OR.
     .             CARDS(ITYP,M)(78:88).EQ.'           ')         .AND.
     .            (CARDS(ITYP,M)(91:102).EQ.'------------'        .OR.
     .             CARDS(ITYP,M)(91:102).EQ.'            ')       .AND.
     .            (CARDS(ITYP,M)(105:107).EQ.'---'                .OR.
     .             CARDS(ITYP,M)(105:107).EQ.'   ')) THEN
                  PRINT'(/131("v"))'
                  PRINT'("#####SDMEDIT FILE CONTAINS RECORD (NUMBER",I5,
     .             ") WHICH IS NOT CONSIDERED HERE (full wild-card id ",
     .             "with WMO bulletin header and"/5X,"originator, lat/",
     .             "lon boundary, and upper-air instrument type fields",
     .             " all not filled - likely a typo which could cause ",
     .             "every"/5X,"report in a dump file to be edited ",
     .             "according to the Q.M. info in this entry!) (SEE ",
     .             "DOCBLOCK FOR DOCUMENTATION ON FORMAT)"/"RPT. ID   ",
     .             "N-LAT  W-LON  YYYYMMDDHH  TYP  PZTQW  LTYP  ",
     .             "PRESSURE(S)  MSG TYPE  WMO-BULLHDR  LAT-LON-BDRY  ",
     .             "ITP  NCEP-ANL-NETWORK(S)"/8("-"),2X,2(5("-"),2X),
     .             10("-"),"  ---  -----  ----  -----------  ",
     .             "--------  -----------  ------------  ---  ",
     .             "-------------------")', M
                  PRINT'(A128)',CARDS(ITYP,M)
                  PRINT'(/131("^")//)'
                  REWIND(91)
                  WRITE(91,'(A)') CARDS(ITYP,M)
                  REWIND(91)
C                 CALL SYSTEM('[ -n "$jlogfile" ] && '//
C    .             '$DATA/postmsg "$jlogfile" "***WARNING: TOO '//
C    .             'MANY WILDCARDS IN RECORD IN SDMEDIT FILE: '//
C    .             '`cat ./fort.91`"')
                  SKIPIT(M) = .TRUE.
                  CYCLE LOOP2n2 ! move on to card w/i time windo for
                                !  this report type
               ENDIF
            ENDIF

            IF(CTYP1.EQ.'UPA' .OR. CTYP1.EQ.'ACF') THEN
C  .. parse vertical significance qualifier and pressure ranges for
C     types UPA and ACF only
               CALL PRSRNG(CARDS(ITYP,M),M,IER)
               IF(IER.GT.0)  GOTO 800 ! problem with parsing
            END IF
            DTMT = 0.01
            IF(CTYP1.EQ.'SAT') DTMT = 2.01 ! wider time tol. for satwnds
            IF(CARDS(ITYP,M)(11:15).NE.'-----') THEN
               READ(CARDS(ITYP,M)(11:15),'(F5.2)',ERR=800) ELAT(M)
               DEYY(M)  = DEYT
               DEYYH(M) = DEYTH
            ENDIF
            IF(CARDS(ITYP,M)(18:22).NE.'-----') THEN
               READ(CARDS(ITYP,M)(18:22),'(F5.2)',ERR=800) ELON(M)
               DEXX(M) = DEYT*COS(ELAT(M)*PI180)
               DEXXH(M) = DEYTH*COS(ELAT(M)*PI180)
               ELON(M) = MOD(360.-ELON(M),360.)
               IF(ELON(M).GT.180.) ELON(M) = ELON(M)-360.
            ENDIF
            IF(CARDS(ITYP,M)(25:32).NE.'--------') THEN
C  .. case where full YYYYMMDDHH specified in SDMEDIT flag file entry
               READ(CARDS(ITYP,M)(25:28),'(I4)',ERR=800) LYEAR(M)
               READ(CARDS(ITYP,M)(29:30),'(I2)',ERR=800) LMNTH(M)
               READ(CARDS(ITYP,M)(31:32),'(I2)',ERR=800) LDAYS(M)
               IF(CARDS(ITYP,M)(33:34).NE.'--')
     .          READ(CARDS(ITYP,M)(33:34),'(I2)',ERR=800) LHOUR(M)
               DTIM(M) = DTMT
            ELSE IF(CARDS(ITYP,M)(33:34).NE.'--') THEN
C  .. case where only HH specified in SDMEDIT flag file entry
               READ(CARDS(ITYP,M)(33:34),'(I2)',ERR=800) LHOUR(M)
               DTIM(M) = DTMT
            ELSE IF(CARDS(ITYP,M)(25:32).NE.'--------') THEN
C  .. case where only YYYYMMDD specified in SDMEDIT flag file entry
               READ(CARDS(ITYP,M)(25:28),'(I4)',ERR=800) LYEAR(M)
               READ(CARDS(ITYP,M)(29:30),'(I2)',ERR=800) LMNTH(M)
               READ(CARDS(ITYP,M)(31:32),'(I2)',ERR=800) LDAYS(M)
               DTIM(M) = DTMT
            ENDIF
            CYCLE LOOP2n2 ! move on to card w/i time windo for this
                          !  report type

  800       CONTINUE

C  COME HERE IF ANY OF THE FOLLOWING VALUES IN THE FLAG FILE ENTRY ARE
C   FOUND TO NOT BE VALID: PRESSURE(S), LATITUDE, LONGITUDE, YEAR,
C   MONTH, DAY, HOUR -- THIS ENTRY WILL BE SKIPPED OVER WHEN EDITING
C   IS PERFORMED
C   (NOTE: IF THIS IS THE FIRST OCCURENCE OF THIS PARTICULAR ENTRY FOR
C          A PARTICULAR TYPE, THEN POST A MESSAGE TO THE PRODUCTION
C          JOBLOG FILE)
C  -------------------------------------------------------------------

            PRINT'(/"#####SDMEDIT FILE CONTAINS CORRUPTED RECORD '//
     .       '(NUMBER",I5,"): ",/A/5X,"- IGNORE AND SKIP TO NEXT '//
     .       'RECORD"/)', M,CARDS(ITYP,M)
            IF(IFIRST(ITYP,M).EQ.0) THEN
               REWIND(91)
               WRITE(91,'(A)') CARDS(ITYP,M)
               REWIND(91)
C              CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg'//
C    .          ' "$jlogfile" "***WARNING: CORRUPT RECORD IN SDMEDIT '//
C    .          'FILE: `cat ./fort.91`"')
               IFIRST(ITYP,M) = 1
            ENDIF
            SKIPIT(M) = .TRUE.

         ENDDO LOOP2n2 ! DO M=1,NEDT(ITYP) ! loop through cards w/i time
                                           !  window for this rpt type

         IFIL_CHK = IFIL_CHK + 1

         OPEN(LUBFI,FILE=BFRFIL,FORM='UNFORMATTED')

         CALL MESGBC(LUBFI,MSGT,ICOMP)
         IF(ICOMP.EQ.1) THEN
            PRINT'(5X,"INPUT AND OUTPUT BUFR FILE MESSAGES   ",
     .       "C O M P R E S S E D")'
            PRINT'("-----BUFR_EDTBFR (UFBTAB) SHOULD BE ABLE TO '//
     .       'PROCESS COMPRESSED BUFR MESSAGES -- CONTINUE")'
C           CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg'//
C    .       ' "$jlogfile" "***WARNING: COMPRESSED BUFR MESSAGES, '//
C    .      'UFBTAB IN PROGRAM BUFR_SDMEDIT SHOULD STILL WORK HOWEVER"')
         ELSE  IF(ICOMP.EQ.0) THEN
            PRINT'(5X,"INPUT AND OUTPUT BUFR FILE MESSAGES   ",
     .       "U N C O M P R E S S E D")'
         ELSE IF(ICOMP.EQ.-1) THEN
            PRINT'(5X,"ERROR READING INPUT BUFR FILE - MESSAGE ",
     .       "COMPRESSION UNKNOWN")'
         ELSE  IF(ICOMP.EQ.-3) THEN
            PRINT'(5X,"INPUT BUFR FILE DOES NOT EXIST")'
         ELSE  IF(ICOMP.EQ.-2) THEN
            PRINT'(5X,"INPUT BUFR FILE HAS NO DATA MESSAGES")'
            CYCLE LOOP2 ! move on to next BUFR dump file
         ENDIF

         PRINT'(16X,"- BEGIN CHECKING REPORTS IN THIS FILE"/)'

         TSTR = BSTR
         IF(ITYP.EQ.004) THEN
            IF(JTYP.EQ.004 .OR. JTYP.EQ.006 .OR. JTYP.EQ.009 .OR.
     .         JTYP.EQ.010 .OR. JTYP.EQ.011 .OR. JTYP.EQ.103) THEN
               TSTR = ASTR
            ELSEIF(JTYP.EQ.008 .OR. JTYP.EQ.012 .OR. JTYP.EQ.013) THEN
               TSTR = DSTR
            ENDIF
         ELSEIF(ITYP.EQ.005) THEN
            TSTR = SSTR
         ENDIF

C  COUNT THE NUMBER OF SUBSETS IN THE FILE TO ALLOCATE SPACE
C  ---------------------------------------------------------

         OPEN(LUBFI,FILE=BFRFIL,FORM='UNFORMATTED')
         CALL OPENBF(0,'QUIET',1) ! will generate diagnostic print if
                                  ! an embedded BUFR table is read
         CALL UFBTAB(-LUBFI,UFBTAB_8,1,1,MXTB,' ')
         CALL OPENBF(0,'QUIET',0) ! return to default wrt degree of prnt

         ALLOCATE(TAB_8(MXTS,MXTB),STAT=I);        IF(I.NE.0) GOTO 904
         ALLOCATE(CLONH_8(MXTB),STAT=I);           IF(I.NE.0) GOTO 904
         ALLOCATE(CLATH_8(MXTB),STAT=I);           IF(I.NE.0) GOTO 904
         ALLOCATE(IMATCH_STNID(MXTB,MEDT),STAT=I); IF(I.NE.0) GOTO 904
         ALLOCATE(EDIT(MXTB),STAT=I);              IF(I.NE.0) GOTO 904
         ALLOCATE(EDIT2(MXTB,MEDT),STAT=I);        IF(I.NE.0) GOTO 904

C  MAKE A TABLE OF ID, LAT, LON AND TIME (TO HOUR), WMO BULLETIN HEADER
C   AND ORIGINATOR, AND UPPER-AIR INSTRUMENT TYPE FROM THE BUFR REPORT
C   DATA {WHERE ID IS STN. ID (FLIGHT/TAIL NUMBER FOR AIRCRAFT) FOR ALL
C   TYPES EXCEPT SATELLITE-DERIVED WINDS WHERE IT IS "SWsssnnn", WHERE
C   "sss" IS BUFR MESSAGE SUBTYPE (I.E., PRODUCT TYPE) AND "nnn" IS
C   DECODED VALUE FOR SATELLITE NUMBER ("SAID")}
C  --------------------------------------------------------------------

         OPEN(LUBFI,FILE=BFRFIL,FORM='UNFORMATTED')
         CALL UFBTAB(LUBFI,TAB_8,MXTS,MXTB,NTAB,TSTR)

C  CHECK FOR HIGH-ACCURACY LAT/LON IF LOW-ACCURACY LAT/LON NOT FOUND
C  -----------------------------------------------------------------

         IF(IBFMS(TAB_8(2,1)).NE.0) THEN
          print *, '     -----> This type encodes high-accuracy lat/lon'
            OPEN(LUBFI,FILE=BFRFIL,FORM='UNFORMATTED')
            CALL UFBTAB(LUBFI,CLONH_8,1,MXTB,NTAB,'CLONH')
            OPEN(LUBFI,FILE=BFRFIL,FORM='UNFORMATTED')
            CALL UFBTAB(LUBFI,CLATH_8,1,MXTB,NTAB,'CLATH')
            TAB_8(2,:) = CLONH_8(:)
            TAB_8(3,:) = CLATH_8(:)
            DEXX = DEXXH
            DEYY = DEYYH
         ELSE
C  ... check for operator descriptors transforming CLAT/CLON from low-
C      to high-accuracy (check scale for CLAT in first subset in file)
            nscl = 2
            iflg_nemspecs = 0
            call openbf(lubfi,'IN ',lubfi)
            if(ireadmg(lubfi,subset,idate).eq.0) then
               if(ireadsb(lubfi).eq.0) then
                  call nemspecs(lubfi,'CLAT',1,nscl,nref,nbts,iret)
                  if(iret.eq.0) then
                     iflg_nemspecs = 1
cpppppppppp
                     print *, '     ---> For CLAT, NSCL = ',NSCL
                     print *
cpppppppppp
                  endif
               endif
            endif
            call closbf(lubfi)
            if(iflg_nemspecs.eq.0) then
               print *
               print *,'##WARNING: NSCL for CLAT could not be returned',
     .                 ' by BUFRLIB routine NEMSPECS, NSCL assumed to ',
     .                 'be 2 (low-accuracy lat/lon).'
               print *
            endif
            if(nscl.gt.2) then
          print *, '     -----> This type encodes high-accuracy lat/lon'
               DEXX = DEXXH
               DEYY = DEYYH
            else
          print *, '     -----> This type encodes low-accuracy lat/lon'
            endif
         ENDIF

         IF(CTYP1.EQ.'SAT') THEN
            DO N=1,NTAB
               TAB=TAB_8(1,N)
               WRITE(STNID,'("SW",2I3.3)') JTYP,NINT(TAB)
               TAB_8(1,N) = RSTNID_8
            ENDDO
         ENDIF

C  LOOP THROUGH THE SAVED (TIME- AND REPORT TYPE-RELEVANT) SDMEDIT FLAG
C  FILE ENTRIES, COMPARING EACH ID, LAT, LON, TIME, MESSAGE TYPE, WMO
C  BULLETIN  HEADER AND ORIGINATOR, LAT-LON BOUNDARY AND UPPER-AIR
C  INSTRUMENT TYPE TO THOSE IN THE BUFR REPORT TABLE - MARK ALL MATCHES
C  {Note: Since both REPORT and FLAG FILE ENTRY dates are present only
C         out to the hour (when flag file entry date is specified), all
C         reports which have date agreeing with flag file date are
C         considered to be a match with the flag file entry (from which
C         the time tolerance is calculated); reported minutes (if
C         present) is NOT considered here}
C  --------------------------------------------------------------------

         WRITE(MSGTYP,'("NC",2I3.3)') ITYP_true,JTYP
         IMATCH_STNID = 0
         EDIT  = .FALSE.
         EDIT2 = .FALSE.
         DO M=1,NEDT(ITYP)  ! Loop thru the saved sdmedit flag file
                            !  entries

C  Do not edit any potential matches with a corrupted flag file entry
C  ------------------------------------------------------------------

            IF(SKIPIT(M)) CYCLE ! move on to next sdmedit flag file
                                ! entry

            JDAT    = 0
            IF(LYEAR(M).GT.0 .AND. LHOUR(M).GE.0) THEN
C  .. case where full YYYYMMDDHH specified in SDMEDIT flag file entry
               JDAT(1) = LYEAR(M)   ! Flag file entry year
               JDAT(2) = LMNTH(M)   ! Flag file entry month
               JDAT(3) = LDAYS(M)   ! Flag file entry day
               JDAT(5) = LHOUR(M)   ! Flag file entry hour
            ELSE IF(LYEAR(M).LT.0. .AND. LHOUR(M).GE.0) THEN
C  .. case where only HH specified in SDMEDIT flag file entry
               JDAT(5) = LHOUR(M)   ! Flag file entry hour
            ELSE IF(LYEAR(M).GT.0. .AND. LHOUR(M).LT.0) THEN
C  .. case where only YYYYMMDD specified in SDMEDIT flag file entry
               JDAT(1) = LYEAR(M)   ! Flag file entry year
               JDAT(2) = LMNTH(M)   ! Flag file entry month
               JDAT(3) = LDAYS(M)   ! Flag file entry day
            ENDIF
            DO N=1,NTAB     ! Loop thru the BUFR report table values
               TAB2    = TAB_8(2,N) ! Report longitude
               TAB3    = TAB_8(3,N) ! Report latitude
               IDAT    = 0
               RINC = 0

C  FIRST TEST FOR A MATCH: TIME
C  ----------------------------

               IF(LYEAR(M).GT.0 .AND. LHOUR(M).GE.0) THEN
C  .. case where full YYYYMMDDHH specified in SDMEDIT flag file entry
                  IDAT(1) = TAB_8(4,N) ! Report year
                  IDAT(2) = TAB_8(5,N) ! Report month
                  IDAT(3) = TAB_8(6,N) ! Report day
                  IDAT(5) = TAB_8(7,N) ! Report hour
                  IT = 2
                  CALL W3DIFDAT(JDAT,IDAT,IT,RINC)
               ELSE IF(LYEAR(M).LT.0. .AND. LHOUR(M).GE.0) THEN
C  .. case where only HH specified in SDMEDIT flag file entry
                  IDAT(5) = TAB_8(7,N) ! Report hour
                  IT = 2
                  CALL W3DIFDAT(JDAT,IDAT,IT,RINC)
               ELSE IF(LYEAR(M).GT.0. .AND. LHOUR(M).LT.0) THEN
C  .. case where only YYYYMMDD specified in SDMEDIT flag file entry
                  IDAT(1) = TAB_8(4,N) ! Report year
                  IDAT(2) = TAB_8(5,N) ! Report month
                  IDAT(3) = TAB_8(6,N) ! Report day
                  IT = 2
                  CALL W3DIFDAT(JDAT,IDAT,IT,RINC)
               ENDIF

               IF(ABS(RINC(2)).GT.DTIM(M)) CYCLE ! no match in time,
                                                 !  move on to next rpt

C  SECOND FIRST TEST FOR A MATCH: REPORT ID
C  ----------------------------------------

               RSTNID_8 = TAB_8(1,N)
               STNID_TEST = STNID
               IF(STNID_TEST.EQ.CARDS(ITYP,M)(1:8)) THEN
ccccc print *, 'exact match - match type 1'
                  IMATCH_STNID(N,M) = 1
               ELSE
C  .. check for wild-card matches in id
                  IF(CARDS(ITYP,M)(1:2).EQ.'* ') THEN
ccccc print *, 'card has only "*" in char. 1 - match type 2'
                     IMATCH_STNID(N,M) = 2
                  ELSE
                     DO I=1,8
                        IF(CARDS(ITYP,M)(I:I).EQ.'?') THEN
                           STNID_TEST(I:I) = '?'
ccccc print *, 'card has id with "?" mark in char. ',i,' case 1 - set ',
ccccc. stnid char. ',i,' to "?"'
                        ENDIF
                     ENDDO
                     II = 8
                     DO I=2,8
                        IF(CARDS(ITYP,M)(I:I+1).EQ.'* ') THEN
ccccc print *, 'card has id ending with "*" in char. ',i
                           II = I - 1
                           EXIT
                        ENDIF
                     ENDDO
                     IF(STNID_TEST(1:II).EQ.CARDS(ITYP,M)(1:II)) THEN
ccccc print *, 'partial wildcard - match type 3'
                        IMATCH_STNID(N,M) = 3
                     ELSE
                        IF(CARDS(ITYP,M)(1:1).EQ.'*') THEN
                           CARDS8 = CARDS(ITYP,M)(1:8)
                           CALL STRSUC(CARDS8,CARDS8_TRUN,LENS1)
ccccc print *, 'card has "*" in char. 1 followed by ',LENS1-1,' char. ',
ccccc. id - test for match'
                           CALL STRSUC(STNID,STNID_TRUN,LENS2)
                           STNID_TEST = STNID
                           J = LENS2-(LENS1-1)
                           DO I = 2,LENS1
                              J = J + 1
                              IF(CARDS(ITYP,M)(I:I).EQ.'?') THEN
                                 STNID_TEST(j:j) = '?'
ccccc print *, 'card has id with "?" mark in char. ',i,. ' case 2 - ',
ccccc. set stnid char. ',j,' to "?"'
                              ENDIF
                           ENDDO
ccccc print *, 'LENS1 = ',lens1
ccccc print *, 'LENS2 = ',lens2
ccccc print *, 'LENS2-(LENS1-1) = ',LENS2-(LENS1-1)
ccccc print *, 'LENS2-(LENS1-1)+1 = ',LENS2-(LENS1-1)+1
ccccc print *, 'STNID_TEST(LENS2-(LENS1-1)+1:LENS2): ',
ccccc. STNID_TEST(LENS2-(LENS1-1)+1:LENS2)
ccccc print *, 'CARDS(ITYP,M)(2:LENS1): ',CARDS(ITYP,M)(2:LENS1)
                           IF(STNID_TEST(LENS2-(LENS1-1)+1:LENS2).EQ.
     .                      CARDS(ITYP,M)(2:LENS1)) THEN
ccccc print *, 'partial wildcard - match type 4'
                              IMATCH_STNID(N,M) = 4
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
cvvvvvprint
ccc    print *, '1-STNID_TEST, CARDS(ITYP,M)(1:8), IMATCH_STNID(N,M): ',
ccc  .             STNID_TEST, CARDS(ITYP,M)(1:8), IMATCH_STNID(N,M)
c^^^^^print
               IF(IMATCH_STNID(N,M).LE.0) CYCLE ! no match in report id,
                                                !  move on to next rpt

C  THIRD FIRST TEST FOR A MATCH: LATITUDE/LONGITUDE
C  ------------------------------------------------

               LMATCH_LATLON = .FALSE.
               IF(MIN(DEYY(M),DEXX(M)).GE.BMISS) THEN

C  If lat/lon not specified in SDMEDIT flag file entry, check to see if
C   lat/lon boundary is specified in SDMEDIT flag file entry
C  --------------------------------------------------------------------

                  IF(CARDS(ITYP,M)(91:102).NE.'------------' .AND.
     .               CARDS(ITYP,M)(91:102).NE.'            ') THEN
C  .. if there is a lat/lon boundary specified in the SDMEDIT flag file
C     entry, parse it out of the flag file entry
                     READ(CARDS(ITYP,M)( 91: 93),'(F3.0)',ERR=802)ALAT_S
                     READ(CARDS(ITYP,M)( 94: 96),'(F3.0)',ERR=802)ALAT_N
                     READ(CARDS(ITYP,M)( 97: 99),'(F3.0)',ERR=802)ALON_E
                     READ(CARDS(ITYP,M)(100:102),'(F3.0)',ERR=802)ALON_W
                     ALON_E = MOD(360.-ALON_E,360.)
                     IF(ALON_E.GT.180.) ALON_E = ALON_E-360.
                     ALON_W = MOD(360.-ALON_W,360.)
                     IF(ALON_W.GT.180.) ALON_W = ALON_W-360.
                     IF(TAB3.GE.ALAT_S.AND.TAB3.LE.ALAT_N) THEN
                        IF(ALON_E.GT.ALON_W) THEN
C      .. in this case, the lon boundary specified in the SDMEDIT flag
C         file entry does not cross 180 degrees
                           IF(TAB2.GE.ALON_W.AND.TAB2.LE.ALON_E) THEN
C          .. the report is inside the lat/lon boundary specified in
C             the SDMEDIT flag file entry
                              LMATCH_LATLON = .TRUE.
                           ENDIF
                        ELSE
C      .. in this case, the lon boundary specified in the SDMEDIT flag
C         file entry crosses 180 degrees
                           IF(TAB2.LE.ALON_E.OR.TAB2.GE.ALON_W) THEN
C          .. the report is inside the lat/lon boundary specified in
C             the SDMEDIT flag file entry
                              LMATCH_LATLON = .TRUE.
                           ENDIF
                        ENDIF
                     ENDIF
                  ELSE
C  .. if there is no lat/lon boundary specified in the SDMEDIT flag
C     file entry, then all report's are considered to have a lat/lon
C     match
                     LMATCH_LATLON = .TRUE.
                  ENDIF
               ELSE

C  If lat/lon is specified in SDMEDIT flag file entry, check to see if
C   the report's lat/lon matches that in the SDMEDIT flag file entry
C  -------------------------------------------------------------------

                  LMATCH_LATLON = (ABS(TAB2-ELON(M)).LE.DEXX(M) .AND.
     .                             ABS(TAB3-ELAT(M)).LE.DEYY(M))
               ENDIF
               GO TO 803
  802          CONTINUE

C  COME HERE IF THE LAT/LON BOUNDARY IN THE SDMEDIT FLAG FILE ENTRY IS
C   FOUND TO NOT BE VALID -- THE LAT/LON BOUNDARY WILL BE TREATED AS
C   MISSING (i.e., all report's are considered to have a lat/lon match
C   since the lat/lon is also not specified in SDMEDIT flag file entry)
C  --------------------------------------------------------------------

               LMATCH_LATLON = .TRUE.
               PRINT'(/"#####SDMEDIT FILE CONTAINS A RECORD WITH A '//
     .         'CORRUPTED LAT/LON BOUNDARY IN COLUMNS 91-102: ",/A/5X,
     .         "- THE LAT/LON RANGE IS TREATED AS MISSING"/)',
     .         CARDS(ITYP,M)
               REWIND(91)
               WRITE(91,'(A)') CARDS(ITYP,M)
               REWIND(91)
C              CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg'//
C    .          ' "$jlogfile" "***WARNING: CORRUPT RECORD IN SDMEDIT '//
C    .          'FILE: `cat ./fort.91`"')
  803          CONTINUE

               IF(.NOT.LMATCH_LATLON) CYCLE ! no match in lat/lon,
                                                 !  move on to next rpt

C  REMAINING TESTS FOR A MATCH: MESSAGE TYPE, WMO BULLETIN HEADER,
C   U-AIR INSTRUMENT TYPE, NETWORK(S)
C  ------------------------------------------------------------------

               CARD = CARDS(ITYP,M)

C  Obtain the WMO bulletin header & originator information for this rpt
C  --------------------------------------------------------------------

               BUHD_8 = TAB_8(8,N)
               BORG_8 = TAB_8(9,N)
C  .. if either is missing, will not compare to sdmedit flag file entry
C     (i.e., will set CARD(78:88) to '-----------') --> UNLESS the
C     sdmedit flag file entry has a report id of "*       " (full
C     wildcard) -and- has a lat/lon boundary for considering reports
C     consisting of all blanks or dashes -and- has an upper-air
C     instrument type consisting of all blanks or dashes or is not an
C     upper-air report) (in this case will set CARD(78:88) to
C     'missing !!!', not allowing a match to ever occur)
               IF(IBFMS(BUHD_8).NE.0.OR.IBFMS(BORG_8).NE.0) THEN
                  CARD(78:88) = '-----------'
                  IF(IMATCH_STNID(N,M).EQ.2) THEN
                     IF((CARDS(ITYP,M)(91:102).EQ.'------------'   .OR.
     .                   CARDS(ITYP,M)(91:102).EQ.'            ')  .AND.
     .                 ((CARDS(ITYP,M)(105:107).EQ.'---'           .OR.
     .                   CARDS(ITYP,M)(105:107).EQ.'   ').OR.
     .                      CARDS(ITYP,M)(37:39).NE.'UPA')) THEN
                        CARD(78:88) = 'missing !!!'
                     ENDIF
                  ENDIF
               ENDIF
               BUHDOR = CBUHD(1:6)//' '//CBORG(1:4)

C  Obtain the u-air instrument type for this rpt (only for UPA types)
C  ------------------------------------------------------------------

               ITP = 999
               IF(CARDS(ITYP,M)(37:39).EQ.'UPA')
     .          ITP = MIN(TAB_8(10,N),999._8)
               WRITE(CITP,'(I3)') ITP
               IF(ITP.EQ.999) CARD(105:107) = '   '
               IF(CARDS(ITYP,M)(105:105).EQ.'0')
     .          CARD(105:105) = ' '

C  Determine the network(s) (up to 4) which apply for this report
C  or the single network which does not apply for this report
C  --------------------------------------------------------------

               IMATCH_NET = 0
               IF(CARDS(ITYP,M)(110:128).NE.'-------------------' .AND.
     .            CARDS(ITYP,M)(110:128).NE.'                   ') THEN
                  IF(NET.NE.'    ') THEN
                     IF(CARDS(ITYP,M)(110:110).EQ.'!') THEN
                        IF(CARDS(ITYP,M)(111:114).NE.NET) IMATCH_NET = 2
                     ELSE
                        DO INET = 1,4
                           ISTART = 105 + (INET * 5)
                           IF(CARDS(ITYP,M)(ISTART:ISTART+3).EQ.NET)THEN
                              IMATCH_NET = 1
                              EXIT
                           ENDIF
                        ENDDO
                     ENDIF
                  ELSE
                     IMATCH_NET = 3
                  ENDIF
               ELSE
                  IMATCH_NET = 4
               ENDIF

C  Set EDIT2 to TRUE for this report in BUFR dump file (index N) and
C   this sdmedit flag file entry (index M) if there indeed is a match
C   (will later signal that this report must be edited according to
C   the quality marker information in the sdmedit flag file entry)
C  ------------------------------------------------------------------

               EDIT2(N,M) = ((MSGTYP.EQ.CARDS(ITYP,M)(68:75)        .OR.
     .                        CARDS(ITYP,M)(68:75).EQ.'--------'    .OR.
     .                        CARDS(ITYP,M)(68:75).EQ.'        ')  .AND.
     .                       (BUHDOR.EQ.CARD(78:88)                 .OR.
     .                        CARD(78:88).EQ.'-----------'          .OR.
     .                        CARD(78:88).EQ.'           ')        .AND.
     .                       (CITP.EQ.CARD(105:107)                 .OR.
     .                        CARD(105:107).EQ.'---'                .OR.
     .                        CARD(105:107).EQ.'   ')              .AND.
     .                       (IMATCH_NET.GT.0))


C  Set EDIT to TRUE for this report in BUFR dump file (index N) if
C   either: 1) EDIT2 is TRUE here, or 2) EDIT has previously been set
C   to TRUE for this report in BUFR dump file in association with
C   another sdmedit flag file entry (will later signal that this report
C   must be edited according to the quality marker information in one
C   or more sdmedit flag file entries)
C  --------------------------------------------------------------------

               EDIT(N) = (EDIT2(N,M) .OR. EDIT(N))
cvvvvvprint
ccc      print *, 'LMATCH_LATLON: ',LMATCH_LATLON
ccc      print *, 'N,M,EDIT(N),EDIT2(N,M): ',N,M,EDIT(N),EDIT2(N,M)
c^^^^^print
            ENDDO ! DO N=1,NTAB ! Loop thru the BUFR report table values
         ENDDO ! DO M=1,NEDT(ITYP)  ! Loop thru the saved sdmedit flag
                                    !  file entries

C  READ AND COPY THE ORIGINAL FILE, APPLYING Q.M. MARKS TO MARKED RPTS
C  -------------------------------------------------------------------

         OPEN(LUBFI,FILE=  BFRFIL,FORM='UNFORMATTED')
         OPEN(LUBFJ,FILE='.EBTMP',FORM='UNFORMATTED')
         CALL OPENBF(LUBFI,'IN ',LUBFI)
         CALL OPENBF(LUBFJ,'OUT',LUBFI)
         call maxout(200000)
         ISUB = 0

         STNID_MATCH = '99999999'
         KTOT        = 0
         DO WHILE(IREADMG(LUBFI,SUBSET,IDATE).EQ.0) ! Loop through BUFR
                                                    !  dump file msgs
            NSUBS = NMSUB(LUBFI) ! NSUBS is the # of subsets in message
            IF(NSUBS.GT.0) THEN
               CALL OPENMB(LUBFJ,SUBSET,IDATE)
               DO N=ISUB+1,ISUB+NSUBS ! Loop thru all subsets in message
                  IF(EDIT(N)) THEN

C  EDIT(N) = TRUE means this subset (report) matches 1 or more sdmedit
C   flag file entries and thus must be edited according to the quality
C   marker information in those matching entries - now we need to find
C   out which flag file entries it matches
C  ---------------------------------------------------------------------

                     IF(IREADSB(LUBFI).NE.0) GOTO 903 ! read subset in
                     CALL UFBCPY(LUBFI,LUBFJ) ! Copy unpacked subset to
                                              !  output BUFR file in
                                              !  preparation for editing
                     DO M=1,NEDT(ITYP) ! Loop thru saved sdmedit file
                                       !  entries - look for a match

C  Do not edit any potential matches with a corrupted flag file entry
C  ------------------------------------------------------------------

                        IF(SKIPIT(M)) CYCLE ! move on to next sdmedit
                                            !  flag file entry

                        IF(EDIT2(N,M)) THEN

C  EDIT2(N,M) = TRUE means this subset (report) matches THIS particular
C   sdmedit flag file entry - call APPLY to edit it according to the
C   quality marker information in this entry
C  --------------------------------------------------------------------

                           CALL UFBINT(LUBFI,HOUR_8,1,1,IRET,'HOUR')
                           CALL UFBINT(LUBFI,MINU_8,1,1,IRET,'MINU')
                           IF(HOUR_8.GE.24.) HOUR_8 = 0.
                           IF(MINU_8.GE.59.) MINU_8 = 0.
                           IHHMM = HOUR_8*100 + MINU_8
                           IF(LYEAR(M).EQ.-99) IHHMM = 0
                           LPRINT = IREAD(ITYP,M,IHHMM).EQ.0
cvvvvvprint
ccc      print *, 'CALL APPLY'
c^^^^^print
                           CALL APPLY(LUBFJ,CARDS(ITYP,M),M,ITYP,JTYP,
     .                                LPRINT)
                           IREAD(ITYP,M,IHHMM) = 1
                           IF(IMATCH_STNID(N,M).GT.1) THEN
                              KTOT(M) = KTOT(M) + 1
                              RSTNID_8 = TAB_8(1,N)
                              IF(KTOT(M).LT.ISTNID_MATCH+1)
     .                         STNID_MATCH(M,KTOT(M)) = STNID
                           ENDIF
                        ENDIF
                     ENDDO ! DO M=1,NEDT(ITYP) ! Loop thru saved
                                               !  sdmedit file entries
                     CALL WRITSB(LUBFJ) ! write subset (likely now
                                        !  edited) to output BUFR file
                     MREP = MREP + 1
                     LREP(ITYP) = LREP(ITYP) + 1
                  ELSE

C  EDIT(N) = FALSE means this subset (report) does NOT match ANY
C   sdmedit flag file entries - it will not be edited so simply copy it
C   as is to output BUFR file
C  --------------------------------------------------------------------

                     CALL COPYSB(LUBFI,LUBFJ,IRET)
                     MREP = MREP + 1
                     LREP(ITYP) = LREP(ITYP) + 1
                  ENDIF
               ENDDO ! DO N=ISUB+1,ISUB+NSUBS ! Loop thru all subsets
                                              !  in BUFR dump file mse
            ELSE
               CALL CLOSMG(LUBFJ)       ! No subsets in message, close
               CALL COPYMG(LUBFI,LUBFJ) !  previous output message and
                                        !  just copy this one to output
                                        !  BUFR file
            ENDIF
            ISUB = ISUB+NSUBS
            NREP = NREP + NSUBS
            KREP(ITYP) = KREP(ITYP) + NSUBS
         ENDDO ! Loop through BUFR dump file messages

         DO  M = 1,NEDT(ITYP)
            IF(KTOT(M).GT.0) THEN
               PRINT 400, CARDS(ITYP,M)(1:8)
  400 FORMAT(/'--> Wildcard id: ',A8,' for below entry includes the ',
     . 'following id''s (there may be dupl. id''s for more than one ',
     . 'time or location):')
               PRINT'(A128)',CARDS(ITYP,M)
               PRINT 401, (STNID_MATCH(M,K),
     .          K=1,MIN(KTOT(M),ISTNID_MATCH))
               IF(KTOT(M).GT.ISTNID_MATCH)
     .          PRINT'("#### Cannot list all reports here as there ",
     .          "are more id matches (",I0,") than the print limit (",
     .          I0,")"/)', KTOT(M), ISTNID_MATCH
  401 FORMAT(3X,14A9)
            ENDIF
         ENDDO

         CALL CLOSBF(LUBFI)
         CALL CLOSBF(LUBFJ)
         OPEN(LUBFI,FILE=  BFRFIL,FORM='UNFORMATTED')
         OPEN(LUBFJ,FILE='.EBTMP',FORM='UNFORMATTED')
         CALL COPYBF(LUBFJ,LUBFI)  ! Copy entire updated BUFR file back
                                   !  to original input unit number

         DEALLOCATE(TAB_8,STAT=I);        IF(I.NE.0) GOTO 905
         DEALLOCATE(CLONH_8,STAT=I);      IF(I.NE.0) GOTO 905
         DEALLOCATE(CLATH_8,STAT=I);      IF(I.NE.0) GOTO 905
         DEALLOCATE(IMATCH_STNID,STAT=I); IF(I.NE.0) GOTO 905
         DEALLOCATE(EDIT,STAT=I);         IF(I.NE.0) GOTO 905
         DEALLOCATE(EDIT2,STAT=I);        IF(I.NE.0) GOTO 905
      ENDDO LOOP2 ! Loop through the BUFR dump files
      CALL SYSTEM('[ -f ./fort.91 ] && rm ./fort.91')

C  EXITS
C  -----

100   CONTINUE
      IF(IFIL_READ.EQ.0) THEN
         PRINT'(/"#####THERE ARE NO DUMP FILES IN UNIT 05, ALL DUMPS ",
     .    "IN THIS GROUP ARE EMPTY"/)'
         GOTO 700
      ENDIF
      IF(IFIL_CHK.EQ.0) GOTO 700

      PRINT 101, NREP
      IF(KREP(0).GT.0) THEN
         PRINT 102, KREP(0)
         PRINT 103, IPQM(0,0),IPQM(0,12),IPQM(0,14)
         PRINT 104, IWQM(0,0),IWQM(0,12),IWQM(0,14)
         PRINT 105, ITQM(0,0),ITQM(0,12),ITQM(0,14)
         PRINT 106, IMQM(0,0),IMQM(0,12),IMQM(0,14)
         PRINT 107, LREP(0)
      ENDIF
      IF(KREP(1).GT.0) THEN
         PRINT 108, KREP(1)
         PRINT 103, IPQM(1,0),IPQM(1,12),IPQM(1,14)
         PRINT 104, IWQM(1,0),IWQM(1,12),IWQM(1,14)
         PRINT 105, ITQM(1,0),ITQM(1,12),ITQM(1,14)
         PRINT 106, IMQM(1,0),IMQM(1,12),IMQM(1,14)
         PRINT 109, LREP(1)
      ENDIF
      IF(KREP(2).GT.0) THEN
         PRINT 110, KREP(2)
         PRINT 103, IPQM(2,0),IPQM(2,12),IPQM(2,14)
         PRINT 104, IWQM(2,0),IWQM(2,12),IWQM(2,14)
         PRINT 105, ITQM(2,0),ITQM(2,12),ITQM(2,14)
         PRINT 106, IMQM(2,0),IMQM(2,12),IMQM(2,14)
         PRINT 111, IGQM(2,0),IGQM(2,12),IGQM(2,14)
         PRINT 112, LREP(2)
      ENDIF
      IF(KREP(4).GT.0) THEN
         PRINT 113, KREP(4)
         PRINT 104, IWQM(4,0),IWQM(4,12),IWQM(4,14)
         PRINT 105, ITQM(4,0),ITQM(4,12),ITQM(4,14)
         PRINT 106, IMQM(4,0),IMQM(4,12),IMQM(4,14)
         PRINT 114, LREP(4)
      ENDIF
      IF(KREP(5).GT.0) THEN
         PRINT 115, KREP(5)
         PRINT 104, IWQM(5,0),IWQM(5,12),IWQM(5,14)
         PRINT 116, LREP(5)
      ENDIF
      IF(KREP(6).GT.0) THEN
         PRINT 118, KREP(6)
         PRINT 103, IPQM(6,0),IPQM(6,12),IPQM(6,14)
         PRINT 104, IWQM(6,0),IWQM(6,12),IWQM(6,14)
         PRINT 105, ITQM(6,0),ITQM(6,12),ITQM(6,14)
         PRINT 106, IMQM(6,0),IMQM(6,12),IMQM(6,14)
         PRINT 119, LREP(6)
      ENDIF
      PRINT 117, MREP
101   FORMAT(/114('=')/'SUMMARY:'/3X,'TOTAL NUMBER OF REPORTS OF ALL ',
     . 'TYPES READ FROM ALL DUMPS CHECKED IN THIS GROUP ',25('.'),I7/)
102   FORMAT(3X,111('+')//3X,'Total number of surface land reports ',
     . 'read from all dumps checked in this group ',25('.'),I7/3X,
     . 'Number of reports with a q.m. change due to matching a SDMEDIT',
     . ' flag file q.m. change entry:'/)
103   FORMAT(18X,'Pressure changed to  0 (keep) ...',I7/
     .       18X,'Pressure changed to 12 (reject) .',I7/
     .       18X,'Pressure changed to 14 (purge) ..',I7/18X,43('-'))
104   FORMAT(18X,'Wind     changed to  0 (keep) ...',I7/
     .       18X,'Wind     changed to 12 (reject) .',I7/
     .       18X,'Wind     changed to 14 (purge) ..',I7/18X,43('-'))
105   FORMAT(18X,'Air temp changed to  0 (keep) ...',I7/
     .       18X,'Air temp changed to 12 (reject) .',I7/
     .       18X,'Air temp changed to 14 (purge) ..',I7/18X,43('-'))
106   FORMAT(18X,'Moisture changed to  0 (keep) ...',I7/
     .       18X,'Moisture changed to 12 (reject) .',I7/
     .       18X,'Moisture changed to 14 (purge) ..',I7/18X,43('-'))
107   FORMAT(/3X,'Total number of surface land reports written back to',
     . ' all dumps checked in this group ',19('.'),I7/)
108   FORMAT(3X,111('+')//3X,'Total number of surface marine reports ',
     . 'read from all dumps checked in this group ',23('.'),I7/3X,
     . 'Number of reports with a q.m. change due to matching a SDMEDIT',
     . ' flag file q.m. change entry:'/)
109   FORMAT(/3X,'Total number of surface marine reports written back ',
     . 'to all dumps checked in this group ',17('.'),I7/)
110   FORMAT(3X,111('+')//3X,'Total number of upper-air reports read ',
     . 'from all dumps checked in this group ',28('.'),I7/3X,'Number ',
     . 'of reports with a q.m. change due to matching a SDMEDIT flag ',
     . 'file q.m. change entry:'/)
111   FORMAT(18X,'Geo/hght changed to  0 (keep) ...',I7/
     .       18X,'Geo/hght changed to 12 (reject) .',I7/
     .       18X,'Geo/hght changed to 14 (purge) ..',I7/18X,43('-'))
112   FORMAT(/3X,'Total number of upper-air reports written back to ',
     . 'all dumps checked in this group ',22('.'),I7/)
113   FORMAT(3X,111('+')//3X,'Total number of aircraft reports read ',
     . 'from all dumps checked in this group ',29('.'),I7/3X,'Number ',
     . 'of reports with a q.m. change due to matching a SDMEDIT flag ',
     . 'file q.m. change entry:'/)
114   FORMAT(/3X,'Total number of aircraft reports written back to all',
     . ' dumps checked in this group ',23('.'),I7/)
115   FORMAT(3X,111('+')//3X,'Total number of satellite wind reports ',
     . 'read from all dumps checked in this group ',23('.'),I7/3X,
     . 'Number of reports with a q.m. change due to matching a SDMEDIT',
     . ' flag file q.m. change entry:'/)
116   FORMAT(/3X,'Total number of satellite wind reports written back ',
     . 'to all dumps checked in this group ',17('.'),I7/)
117   FORMAT(3X,111('+')//3X,'TOTAL NUMBER OF REPORTS OF ALL TYPES ',
     . 'WRITTEN BACK TO ALL DUMPS CHECKED IN THIS GROUP ',19('.'),I7//
     . 114('=')/)
118   FORMAT(3X,111('+')//3X,'Total number of mesonet reports read ',
     . 'from all dumps checked in this group ',30('.'),I7/3X,
     . 'Number of reports with a q.m. change due to matching a SDMEDIT',
     . ' flag file q.m. change entry:'/)
119   FORMAT(/3X,'Total number of mesonet reports written back to all ',
     . 'dumps checked in this group ',24('.'),I7/)

700   CONTINUE
      write(60,'("ALL DONE")')
      CALL W3TAGE('BUFR_EDTBFR')
      STOP

900   CONTINUE
      PRINT *, '#####E-O-F OR ERROR READING TIME WINDOW FROM STANDARD ',
     . 'INPUT - STOP 99'
C     CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" '//
C    . '"**ERROR IN PROGRAM BUFR_SDMEDIT - ABNORMAL EXIT"')
      CALL W3TAGE('BUFR_EDTBFR')
      CALL ERREXIT(99)
901   CONTINUE
      PRINT *, '#####ERROR READING WORKING INPUT DUMP FILE NAME FROM ',
     . 'STANDARD INPUT - STOP 99'
C     CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" '//
C    . '"**ERROR IN PROGRAM BUFR_SDMEDIT - ABNORMAL EXIT"')
      CALL W3TAGE('BUFR_EDTBFR')
      CALL ERREXIT(99)
902   CONTINUE
      PRINT'("#####ERROR READING BUFR MESSAGE TYPE/SUBTYPE (INTERNAL)'//
     . ' FROM INPUT DUMP FILE NAME - STOP 99")'
C     CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" '//
C    . '"**ERROR IN PROGRAM BUFR_SDMEDIT - ABNORMAL EXIT"')
      CALL W3TAGE('BUFR_EDTBFR')
      CALL ERREXIT(99)
903   CONTINUE
      PRINT *, '#####POSITION ERROR READING A SUBSET FROM A BUFR ',
     . 'MESSAGE - STOP 99'
C     CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" '//
C    . '"**ERROR IN PROGRAM BUFR_SDMEDIT - ABNORMAL EXIT"')
      CALL W3TAGE('BUFR_EDTBFR')
      CALL ERREXIT(99)
904   CONTINUE
      PRINT *, '#####UNABLE TO ALLOCATE ARRAYS - STOP 99'
C     CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" '//
C    . '"**UNABLE TO ALLOCATE ARRAYS - ABNORMAL EXIT"')
      CALL W3TAGE('BUFR_EDTBFR')
      CALL ERREXIT(99)
905   CONTINUE
      PRINT *, '#####UNABLE TO DEALLOCATE ARRAYS - STOP 99'
C     CALL SYSTEM('[ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" '//
C    . '"**UNABLE TO DEALLOCATE ARRAYS - ABNORMAL EXIT"')
      CALL W3TAGE('BUFR_EDTBFR')
      CALL ERREXIT(99)

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    APPLY
C   PRGMMR: KEYSER           ORG: NP22       DATE: 2018-02-21
C
C ABSTRACT: ENCODES SDMEDIT Q.C. FLAGS (KEEP, REJECT OR PURGE) INTO
C   BUFR REPORTS.  THE FOLLOWING DATA TYPES CAN ENCODE Q.C. FLAGS
C   (WHICH REPLACE MISSING VALUES): PRESSURE (SEA-LEVEL AND STATION
C   DEPENDING UPON REPORT TYPE), WIND, AIR TEMPERATURE, GEOPOTENTIAL
C   (OR HEIGHT) AND MOISTURE (DEWPOINT, DEWPOINT DEPRESSION, MIXING
C   RATIO OR RELATIVE HUMIDITY DEPENDING UPON REPORT TYPE).  FOR UPPER-
C   AIR REPORT TYPES, DETERMINES WHICH PRESSURE LEVELS ARE TO GET Q.C.
C   FLAGS.
C
C PROGRAM HISTORY LOG:
C 1997-02-01  J. WOOLLEN -- ORIGINAL AUTHOR
C 2004-02-02  D. KEYSER  -- STREAMLINED CODE; ADDED DOCBLOCK AND
C       COMMENTS; ADDED MORE DESCRIPTIVE STANDARD OUTPUT PRINT
C 2006-04-18  D. KEYSER  -- MODIFIED TO PROPERLY HANDLE TAMDAR AIRCRAFT
C       (MOISTURE AVAILABLE)
C 2014-03-05  D. A. KEYSER -- INCREASED MAXIMUM NUMBER OF TIME- AND
C     REPORT TYPE-RELEVANT ENTRIES ALLOWED IN THE SDMEDIT FLAG FILE
C     FROM 1000 TO 2000 (PARAMETER "MEDT")
C 2016-08-09  S. Melchior/D. Keyser
C         - Corrected comments re: which aircraft types contain
C           moisture and what type of moisture observation is reported.
C         - Included E-AMDAR (BUFR type 004, subtype 006), TAMDAR (from
C           Panasonic, or before that AirDAT, 004, 010), PenAir TAMDAR
C           (from MADIS, 004, 012), Chautauqua TAMDAR (from MADIS, 004,
C           013) and "Catch-all" AMDAR (004, 103) to types of aircraft
C           reports which can encode a moisture QM from an sdmedit flag
C           text file entry.
C         - Corrected comments re: which aircraft types use which
C           height mnemonic when a pressure must be calculated from
C           reported "height", and which aircraft types actually report
C           pressure. (Needed when obs within a specified pressure
C           layer are considered based on the sdmedit flag text file
C           entry.)
C         - Corrected oversight in 2016-01-14 update whereby code was
C           not obtaining the reported height (in the form of mnemonic
C           FLVLST) for Korean and "Catch-all" AMDAR aircraft reports.
C           This is needed when a pressure must be calculated from
C           reported "height" when obs within a specified pressure
C           layer are considered based on the sdmedit flag text file
C           entry.
C         - Replace assignment of BMISS and instead pass in R*8 value
C           set in main program (using GETBMISS) via COMMON.
C 2016-08-15  D. A. Keyser --  Added ability to apply QC flags to
C     mesonet reports originally in the b255 tanks (that eventually go
C     into the "msonet" dump). These are entered in the sdmedit flag
C     text file with report type "MSO".
C 2018-02-21  D. A. KEYSER --  Modified to handle BUFR-feed upper-air
C     reports in tanks b002/xx101-xx105:
C        - Increased arrays holding level data from 255 to 9000 (to
C          account for high vertical resolution in many reports now in
C          these tanks).
C        - Reads mnemonic "VSIGX" from these tanks rather than "VSIG"
C          which is only in TAC-feed tanks, and then translates its
C          value into "VSIG" value associated with "SURF", "MAND",
C          "SIGT" and "SIGW" levels (for editing Q.M. of levels by their
C          vertical significance qualifier).
C        - Increased amount of temporary diagnostic print.
C
C USAGE:    CALL APPLY (LUBFJ, CARD, M, ITYP, JTYP, LPRINT)
C   INPUT ARGUMENT LIST:
C     LUBFJ    - INTEGER, UNIT NUMBER OF OUTPUT BUFR FILE
C     CARD     - CHARACTER*128, STRING CONTAINING ONE ENTIRE LINE
C                (RECORD) AS READ IN FROM THE SDMEDIT FLAG FILE
C     M        - INTEGER, INDEX IN MEMORY POINTING TO THE RECORD NUMBER
C     ITYP     - INTEGER,
C                  IF 000 THROUGH 005: BUFR MESSAGE TYPE (000 - SURFACE
C                    LAND, 001 - SURFACE MARINE, 002 - UPPER-AIR, 004 -
C                    AIRCRAFT, 005 - SATELLITE-DERIVED WIND)
C                  IF 006: MESONET (WHICH IS BUFR MESSAGE TYPE 255)
C     JTYP     - INTEGER, BUFR MESSAGE SUBTYPE (SEE BUFR TABLES)
C     LPRINT   - LOGICAL, IF TRUE THIS IS THE FIRST TIME THIS
C                PARTICULAR RECORD FROM THE SDMEDIT FLAG FILE HAS
C                BEEN PASSED INTO THIS SUBROUTINE (STANDARD OUTPUT
C                DIAGNOSTIC PRINT WILL BE GENERATED), IF FALSE THIS
C                RECORD HAS ALREADY BEEN PASSED IN AND DIAGNOSTIC
C                PRINT HAS ALREADY BEEN GENERATED IN A PREVIOUS CALL
C                SO NONE IS GENERATED HERE
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT LUBFJ - BUFR FILE AFTER QUALITY CONTROL
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  WCOSS (Linux)
C
C$$$
      SUBROUTINE APPLY(LUBFJ,CARD,M,ITYP,JTYP,LPRINT)

      PARAMETER (MEDT=2000) ! Allows up to 2000 time- and report type-
                            ! relevant entries in the SDMEDIT flag file

      CHARACTER*128 CARD
      CHARACTER*80  PQMST, PQMSTX

      DIMENSION    PQMS(10,9000),ZQMS(7),JFIRST(5),KFIRST(0:255)
      INTEGER      IBIT(32)

      LOGICAL      EDIT_UPA,EDIT_ACF,ALL,LPRINT

      REAL(8)      PQMS_8(10,9000),ZQMS_8(7),UFBINT_8,HOUR_8,MINU_8,
     .             HGHT_8,R8VAL,R82I,BMISS

      COMMON /UEDIT/  IVSG(MEDT),PMIN(MEDT),PMAX(MEDT)
      COMMON /COUNTS/ IPQM(0:6,0:14),IWQM(0:6,0:14),ITQM(0:6,0:14),
     .                IGQM(0:6,0:14),IMQM(0:6,0:14)
      COMMON /BUFRLIB_MISSING/BMISS

      DATA PQMST  /'VSIG  PRLC GP07 GP10 QMPR QMGP QMAT QMDD QMWN'/
      DATA PQMSTX /'VSIGX PRLC GP07 GP10 QMPR QMGP QMAT QMDD QMWN'/

      DATA KFIRST/256*0/
      DATA FILL  / 10E5/
      DATA GRAV  /  9.8/

C-----------------------------------------------------------------------
      PRS1(Z) = 1013.25 * (((288.15 - (.0065 * Z))/288.15)**5.256)
      PRS2(Z) = 226.3 * EXP(1.576106E-4 * (11000. - Z))
C-----------------------------------------------------------------------

      mxib  = 100
      nbits =  18

C  PRINT SDMEDIT FLAG FILE RECORD USED TO APPLY Q.C. MARKS FOR THIS RPT
C  --------------------------------------------------------------------

      IF(KFIRST(JTYP).EQ.0) THEN
         PRINT'(/"THE FOLLOWING REPORTS ARE MODIFIED AS INDICATED HERE",
     .    " BY THIS PROGRAM (SEE DOCBLOCK FOR DOCUMENTATION ON FORMAT ",
     .    "BELOW):"//"RPT. ID   N-LAT  W-LON  YYYYMMDDHH  TYP  ",
     .    "PZTQW  LTYP  PRESSURE(S)  MSG TYPE  WMO-BULLHDR  ",
     .    "LAT-LON-BDRY  ITP  NCEP-ANL-NETWORK(S)"/8("-"),2X,2(5("-"),
     .    2X),10("-"),"  ---  -----  ----  -----------  --------  ",
     .    "-----------  ------------  ---  -------------------")'
         KFIRST(JTYP) = 1
      ENDIF

      IF(LPRINT) THEN
         PRINT'(/A128)',CARD
         IF(CARD(25:34).NE.'----------') THEN
            CALL UFBINT(-LUBFJ,HOUR_8,1,1,IRET,'HOUR')
            CALL UFBINT(-LUBFJ,MINU_8,1,1,IRET,'MINU')
            IF(IBFMS(MINU_8).NE.0) MINU_8 = 0.
            PRINT'("For report time (HHMM): ",2I2.2)',
     .       NINT(HOUR_8),NINT(MINU_8)
         ENDIF
      ENDIF


C  PARSE THE SDMEDIT QUALITY FLAGS FOR THIS ENTRY
C  ----------------------------------------------

      IQMPR = IQMF(CARD(42:42))  ! Quality mark on pressure
                                 !  For SFC, MSO - stn & mean sea-level
                                 !  For SHP - mean sea-level
                                 !  For UPA - station and level pressure
      IQMGP = IQMF(CARD(43:43))  ! Quality mark on geopotential or hght
      IQMAT = IQMF(CARD(44:44))  ! Quality mark on air temperature
      IQMDD = IQMF(CARD(45:45))  ! Quality mark on moisture
                                 !  For SFC, SHP, UPA - dwpt temp
                                 !  For ACF E-AMDAR (subtype 006)} -
                                 !     mixing ratio
                                 !  For ACF {MDCRS (subtype 004)} -
                                 !     mixing ratio or rh
                                 !  For ACF {TAMDAR (Panasonic or
                                 !     AirDAT, subtype 010; MADIS, 008,
                                 !     012, 013)} - rh
                                 !  For ACF {{AMDAR fmt (subtype 003);
                                 !     RECCOs (005)}, MSO - dwpt temp
                                 !     or rh
                                 !  For ACF {Catch-all AMDAR (subtype
                                 !     103)} - mixing ratio or dwpt temp
      IQMWN = IQMF(CARD(46:46))  ! Quality mark on wind

C  ENCODE QUALITY FLAGS INTO THIS BUFR REPORT
C  (EACH REPORT TYPE IS TREATED A LITTLE DIFFERENTLY)
C  -------------------------------------------------

      IF(CARD(37:39).EQ.'SAT') THEN
C  Satellite-derived winds encode only wind q.m. (into SWQM)
         IF(LPRINT) THEN
            IF(IQMPR.GE.0) print'("    ... QMPR (=",I2,") is NOT ",
     .       "encoded into reports of type ",A)', IQMPR,CARD(37:39)
            IF(IQMGP.GE.0) print'("    ... QMGP (=",I2,") is NOT ",
     .       "encoded into reports of type ",A)', IQMGP,CARD(37:39)
            IF(IQMAT.GE.0) print'("    ... QMAT (=",I2,") is NOT ",
     .       "encoded into reports of type ",A)', IQMAT,CARD(37:39)
            IF(IQMDD.GE.0) print'("    ... QMDD (=",I2,") is NOT ",
     .       "encoded into reports of type ",A)', IQMDD,CARD(37:39)
         ENDIF
         IF(IQMWN.GE.0) THEN
            UFBINT_8 = IQMWN
            CALL UFBINT(LUBFJ,UFBINT_8,1,1,IRET,'SWQM')
            if(lprint) print'("    ... SWQM set to ",F3.0)', ufbint_8
            IWQM(ITYP,IQMWN) = IWQM(ITYP,IQMWN) + 1
         ENDIF
      ELSEIF(CARD(37:39).EQ.'SFC'.OR.CARD(37:39).EQ.'SHP'.OR.
     .       CARD(37:39).EQ.'MSO') THEN
C  Surface land, marine and mesonet reports encode all q.m. except
C   geopot./height
         IF(IQMPR.GE.0) THEN
            UFBINT_8 = IQMPR
            CALL UFBINT(LUBFJ,UFBINT_8,1,1,IRET,'QMPR')
            if(lprint) print'("    ... QMPR set to ",F3.0)', ufbint_8
            IPQM(ITYP,IQMPR) = IPQM(ITYP,IQMPR) + 1
         ENDIF
         IF(LPRINT) THEN
            IF(IQMGP.GE.0) print'("    ... QMGP (=",I2,") is NOT ",
     .       "encoded into reports of type ",A)', IQMGP,CARD(37:39)
         ENDIF
         IF(IQMAT.GE.0) THEN
            UFBINT_8 = IQMAT
            CALL UFBINT(LUBFJ,UFBINT_8,1,1,IRET,'QMAT')
            if(lprint) print'("    ... QMAT set to ",F3.0)', ufbint_8
            ITQM(ITYP,IQMAT) = ITQM(ITYP,IQMAT) + 1
         ENDIF
         IF(IQMDD.GE.0) THEN
            UFBINT_8 = IQMDD
            CALL UFBINT(LUBFJ,UFBINT_8,1,1,IRET,'QMDD')
            if(lprint) print'("    ... QMDD set to ",F3.0)', ufbint_8
            IMQM(ITYP,IQMDD) = IMQM(ITYP,IQMDD) + 1
         ENDIF
         IF(IQMWN.GE.0) THEN
            UFBINT_8 = IQMWN
            CALL UFBINT(LUBFJ,UFBINT_8,1,1,IRET,'QMWN')
            if(lprint) print'("    ... QMWN set to ",F3.0)', ufbint_8
            IWQM(ITYP,IQMWN) = IWQM(ITYP,IQMWN) + 1
         ENDIF
      ELSEIF(CARD(37:39).EQ.'ACF') THEN
C  Aircraft reports (at least one subtype) encode all q.m. except
C   pressure and geopot./height
         IF(LPRINT) THEN
            IF(IQMPR.GE.0) print'("    ... QMPR (=",I2,") is NOT ",
     .       "encoded into reports of type ",A)', IQMPR,CARD(37:39)
            IF(IQMGP.GE.0) print'("    ... QMGP (=",I2,") is NOT ",
     .       "encoded into reports of type ",A)', IQMGP,CARD(37:39)
         ENDIF
C  Read back report data from aircraft BUFR file
         CALL UFBINT(-LUBFJ,ZQMS_8,7,1,NLEV,
     .               'PSAL FLVL IALT PRLC HEIT HMSL FLVLST');ZQMS=ZQMS_8
         ALL = PMIN(M).LE.0 .AND. PMAX(M).GE.BMISS
         IF(.NOT.ALL) THEN
            IF(IBFMS(ZQMS_8(4)).NE.0) THEN  ! If press missing, use hght
                                    !  to est. pressure (U.S. Std. Atm.)
               PRES = BMISS
               IF(IBFMS(ZQMS_8(1)).EQ.0) THEN
                  HGHT = ZQMS(1) ! PSAL, 1st choice for HGHT (most AMDAR
                                 !                            fmt,
                                 !                          some RECCOS)
               ELSE  IF(IBFMS(ZQMS_8(2)).EQ.0) THEN
                  HGHT = ZQMS(2) ! FLVL, 2nd ch. for HGHT (AIREP/PIREP,
                                 !                      some AMDAR fmt)
               ELSE  IF(IBFMS(ZQMS_8(3)).EQ.0) THEN
                  HGHT = ZQMS(3) ! IALT, 3rd choice for HGHT
               ELSE  IF(IBFMS(ZQMS_8(5)).EQ.0) THEN
                  HGHT = ZQMS(5) ! HEIT, 4th choice for HGHT
               ELSE  IF(IBFMS(ZQMS_8(6)).EQ.0) THEN
                  HGHT = ZQMS(6) ! HMSL, 5th choice for HGHT (E-AMDAR,
                                 !                    Canadian AMDAR,
                                 !            TAMDAR (from Panasonic)
               ELSE  IF(IBFMS(ZQMS_8(7)).EQ.0) THEN
                  HGHT = ZQMS(7) ! FLVLST, 6th choice for HGHT
                                 !       {Catch-all AMDAR (raw BUFR),
                                 !                 most Korean AMDAR}
               ELSE
                  HGHT = BMISS   ! some Korean AMDAR
               ENDIF
               IF(HGHT.LT.BMISS) THEN
                  IF(HGHT.LE.11000) PRES  = NINT(PRS1(HGHT)*10.)
                  IF(HGHT.GT.11000) PRES  = NINT(PRS2(HGHT)*10.)
               ENDIF
            ELSE
               PRES = NINT(ZQMS(4)*.1) ! Reported pressure {only for
                                    !      MDCRS, TAMDAR (from AirDAT
                                    !      and from MADIS), some RECCOs}
            ENDIF
            IF(PRES.LT.BMISS) THEN
               EDIT_ACF = PRES.GE.PMIN(M)*10. .AND. PRES.LE.PMAX(M)*10.
            ELSE
               EDIT_ACF = .TRUE. ! If PRES & HGHT missing, apply q.m.(s)
            ENDIF
         ELSE
            EDIT_ACF = .TRUE.
         ENDIF

         IF(LPRINT) THEN
            IF(IQMAT.GE.0) THEN
               IF(ALL) THEN
                  print'("    ... QMAT set to ",F3.0," on ALL levels")',
     .             REAL(IQMAT)
               ELSE
                  print'("    ... QMAT set to ",F3.0," on ALL pressure",
C DONG  .            '" levels between",I5," and",I5," mb, inclusive")',
     .             " levels between",I12," and",I5," mb, inclusive")',
     .             REAL(IQMAT),NINT(PMAX(M)),NINT(PMIN(M))
               ENDIF
            ENDIF
            IF(IQMDD.GE.0) THEN
               IF(JTYP.LE. 2 .OR. JTYP.EQ. 7 .OR. JTYP.EQ. 9 .OR.
     .            JTYP.EQ.11) THEN
C  .... AIREP, PIREP, AFWA-MDCRS, CANADIAN AMDAR and KOREAN AMDAR
C        subtypes do not encode moisture q.m.
                  print'("    ... QMDD (=",I2,") is NOT encoded into ",
     .             "this report subtype (",I3.3,") of type ",A)',
     .             IQMDD,JTYP,CARD(37:39)
               ELSE
C  .... AMDAR fmt, MDCRS, RECCO, E-AMDAR, TAMDAR( all types) and
C        "Catch-all" AMDAR subtypes do encode moisture q.m.
                  IF(ALL) THEN
                     print'("    ... QMDD set to ",F3.0," on ALL ",
     .                "levels")',REAL(IQMDD)
                  ELSE
                     print'("    ... QMDD set to ",F3.0," on ALL ",
     .                "pressure levels between",I5," and",I5," mb, ",
     .                "inclusive")', REAL(IQMDD),NINT(PMAX(M)),
     .                NINT(PMIN(M))
                  ENDIF
               ENDIF
            ENDIF
            IF(IQMWN.GE.0) THEN
               IF(ALL) THEN
                  print'("    ... QMWN set to ",F3.0," on ALL levels")',
     .             REAL(IQMWN)
               ELSE
                  print'("    ... QMWN set to ",F3.0," on ALL pressure",
     .             " levels between",I5," and",I5," mb, inclusive")',
     .             REAL(IQMWN),NINT(PMAX(M)),NINT(PMIN(M))
               ENDIF
            ENDIF
         ENDIF

         IF(EDIT_ACF) THEN  ! This lvl w/i range of press. for which
                            ! q.m. on one or more variables applied
         IF(IQMAT.GE.0) THEN
            UFBINT_8 = IQMAT
            CALL UFBINT(LUBFJ,UFBINT_8,1,1,IRET,'QMAT')
            ITQM(ITYP,IQMAT) = ITQM(ITYP,IQMAT) + 1
         ENDIF
         IF(IQMDD.GE.0) THEN
            IF((JTYP.GE.3 .AND. JTYP.LE.6) .OR. JTYP.EQ.8 .OR.
     .          JTYP.EQ.10 .OR. JTYP.EQ.12 .OR. JTYP.EQ.13. OR.
     .          JTYP.EQ.103) THEN
C  .... AMDAR fmt, MDCRS, RECCO, E-AMDAR, TAMDAR( all types) and
C        "Catch-all" AMDAR subtypes do encode moisture q.m.
               UFBINT_8 = IQMDD
               CALL UFBINT(LUBFJ,UFBINT_8,1,1,IRET,'QMDD')
               IMQM(ITYP,IQMDD) = IMQM(ITYP,IQMDD) + 1
            ENDIF
         ENDIF
         IF(IQMWN.GE.0) THEN
            UFBINT_8 = IQMWN
            CALL UFBINT(LUBFJ,UFBINT_8,1,1,IRET,'QMWN')
            IWQM(ITYP,IQMWN) = IWQM(ITYP,IQMWN) + 1
         ENDIF
         ENDIF
      ELSEIF(CARD(37:39).EQ.'UPA') THEN
C  Upper-air reports encode all q.m.

C  Read back report data from upper-air BUFR file
         IF(ITYP.EQ.002.AND.(JTYP.GE.101.AND.JTYP.LE.105)) THEN
            CALL UFBINT(-LUBFJ,PQMS_8,10,9000,NLEV,PQMSTX)
         ELSE
            CALL UFBINT(-LUBFJ,PQMS_8,10,255,NLEV,PQMST)
         ENDIF
         PQMS=PQMS_8
         ALL = PMIN(M).LE.0 .AND. PMAX(M).GE.BMISS
         JFIRST = 0
         IF(NLEV.GT.0) THEN
         DO N=1,NLEV    ! Loop through all levels in report BUFR file
            if(ITYP.EQ.002.AND.(JTYP.GE.101.AND.JTYP.LE.105)) then
cppppp
               print *, 'unpacked value for VSIGX is ',pqms(1,n)
cppppp
               if(pqms(1,n).ne.0.) then
                  nib = 0
                  DO I=(NBITS-1),0,-1
                     R82I = (2.)**I
                     R8VAL = pqms(1,n)
                     IF(ABS(R8VAL-R82I).LT.(0.005)) THEN
                        NIB = NIB + 1
                        IF(NIB.GT.MXIB) THEN
                        print *,'IBIT ARRAY OVERFLOW set pqms(1,n) to 0'
                           pqms(1,n) = 0.
                           go to 110
                        ENDIF
                        IBIT(NIB) = NBITS-I
                        go to 100
                     ELSEIF(R82I.LT.R8VAL) THEN
                        NIB = NIB + 1
                        IF(NIB.GT.MXIB) THEN
                        print *,'IBIT ARRAY OVERFLOW set pqms(1,n) to 0'
                           pqms(1,n) = 0.
                           go to 110
                        ENDIF
                        IBIT(NIB) = NBITS-I
                        R8VAL = R8VAL - R82I
                     ENDIF
                  ENDDO
  100             continue
                  print *, nib,' bits are on'
                  do i = 1,nib
                     print *, 'bit number ',ibit(i),' is on'
                     if(ibit(i).eq.1) then
                        pqms(1,n) = 64. ! surface
                        go to 110
                     elseif(ibit(i).eq.2) then
                        pqms(1,n) = 32. ! mandatory
                        go to 110
                     elseif(ibit(i).eq.5) then
                        pqms(1,n) = 4.  ! sig temp
                        go to 110
                     elseif(ibit(i).eq.6) then
                        pqms(1,n) = 4.  ! sig humidity
                        go to 110
                     elseif(ibit(i).eq.7) then
                        pqms(1,n) = 2.  ! sig wind
                        go to 110
                     endif
                  enddo
  903             continue
                  print *, 'SURF, MAND, SIGT, SIGW bits all off, ',
     .             'set pqms(1,n) to 0'
                  pqms(1,n) = 0.
               endif
  110          continue
cppppp
               print *, 'translated value for VSIGX is ',pqms(1,n)
cppppp
            endif
c
            JVSG = NINT(PQMS(1,N))  ! Report level vert. significance
            IF(IVSG(M).NE.JVSG .AND. IVSG(M).NE.0) CYCLE
            HGHT_8 = MIN(PQMS_8(3,N),PQMS_8(4,N)) ! Rpt lvl geopot./hght
            HGHT = HGHT_8
            IF(.NOT.ALL) THEN
               IF(IBFMS(PQMS_8(2,N)).EQ.0) THEN
                  PRES = NINT(PQMS(2,N)*.01)
               ELSEIF(IBFMS(HGHT_8).EQ.0) THEN
                  PRES = BMISS
                  HGHT = HGHT/GRAV  ! Rpt lvl press missing, geopt/hght
                                    ! valid, compute std atmos. press.
                  IF(HGHT.LE.11000) PRES  = NINT(PRS1(HGHT))
                  IF(HGHT.GT.11000) PRES  = NINT(PRS2(HGHT))
                    ! If geopt/hght q.m. missing set it to pressure q.m.
                  IF(IQMGP.LT.0 .AND. IQMPR.GE.0) IQMGP = IQMPR
cppppp
                  if(iqmgp.lt.0 .and. iqmpr.ge.0)
     .             print *,'IQMGP set to IQMPR (',iqmpr,')'
cppppp
               ENDIF
               IF(PRES.LT.BMISS) THEN
                  EDIT_UPA = PRES+.5.GE.PMIN(M) .AND. PRES-.5.LE.PMAX(M)
               ELSE
                  EDIT_UPA = .TRUE. ! If PRES & HGHT missing, apply
                                    ! q.m.(s) on this level
               ENDIF
            ELSE
               IF(IBFMS(PQMS_8(2,N)).NE.0 .AND. IBFMS(HGHT_8).EQ.0) THEN
                    ! If geopt/hght q.m. missing set it to pressure q.m.
                  IF(IQMGP.LT.0 .AND. IQMPR.GE.0) IQMGP = IQMPR
cppppp
                  if(iqmgp.lt.0 .and. iqmpr.ge.0)
     .             print *,'IQMGP set to IQMPR (',iqmpr,')'
cppppp
               ENDIF
               EDIT_UPA = .TRUE.
            ENDIF
            IF(EDIT_UPA) THEN  ! This lvl w/i range of press. for which
                               ! q.m. on one or more variables applied
               IF(IQMPR.GE.0) THEN
                  PQMS(5,N) = IQMPR
                  IF(JFIRST(1).EQ.0) THEN
                     IF(LPRINT) THEN
                        IF(ALL) THEN
                           print'("    ... QMPR set to ",F3.0," on ALL",
     .                     " levels")', PQMS(5,N)
                        ELSE
                           print'("    ... QMPR set to ",F3.0," on ALL",
     .                      " pressure levels between",I5," and",I5,
     .                      " mb, inclusive")',
     .                      PQMS(5,N),NINT(PMAX(M)),NINT(PMIN(M))
                        ENDIF
                     ENDIF
                  ENDIF
                  IPQM(ITYP,IQMPR) = IPQM(ITYP,IQMPR) + 1
                  JFIRST(1) = 1
               ENDIF
               IF(IQMGP.GE.0) THEN
                  PQMS(6,N) = IQMGP
                  IF(JFIRST(2).EQ.0) THEN
                     IF(LPRINT) THEN
                        IF(ALL) THEN
                           if(lprint) print'("    ... QMGP set to ",
     .                      F3.0," on ALL levels")', PQMS(6,N)
                        ELSE
                           if(lprint) print'("    ... QMGP set to ",
     .                      F3.0," on ALL pressure levels between",I5,
     .                      " and",I5," mb, inclusive")',
     .                      PQMS(6,N),NINT(PMAX(M)),NINT(PMIN(M))
                        ENDIF
                     ENDIF
                  ENDIF
                  IGQM(ITYP,IQMGP) = IGQM(ITYP,IQMGP) + 1
                  JFIRST(2) = 1
               ENDIF
               IF(IQMAT.GE.0) THEN
                  PQMS(7,N) = IQMAT
                  IF(JFIRST(3).EQ.0) THEN
                     IF(LPRINT) THEN
                        IF(ALL) THEN
                           if(lprint) print'("    ... QMAT set to ",
     .                      F3.0," on ALL levels")', PQMS(7,N)
                        ELSE
                           if(lprint) print'("    ... QMAT set to ",
     .                      F3.0," on ALL pressure levels between",I5,
     .                      " and",I5," mb, inclusive")',
     .                      PQMS(7,N),NINT(PMAX(M)),NINT(PMIN(M))
                        ENDIF
                     ENDIF
                  ENDIF
                  ITQM(ITYP,IQMAT) = ITQM(ITYP,IQMAT) + 1
                  JFIRST(3) = 1
               ENDIF
               IF(IQMDD.GE.0) THEN
                  PQMS(8,N) = IQMDD
                  IF(JFIRST(4).EQ.0) THEN
                     IF(LPRINT) THEN
                        IF(ALL) THEN
                           if(lprint) print'("    ... QMDD set to ",
     .                      F3.0," on ALL levels")', PQMS(8,N)
                        ELSE
                           if(lprint) print'("    ... QMDD set to ",
     .                      F3.0," on ALL pressure levels between",I5,
     .                      " and",I5," mb, inclusive")',
     .                      PQMS(8,N),NINT(PMAX(M)),NINT(PMIN(M))
                        ENDIF
                     ENDIF
                  ENDIF
                  IMQM(ITYP,IQMDD) = IMQM(ITYP,IQMDD) + 1
                  JFIRST(4) = 1
               ENDIF
               IF(IQMWN.GE.0) THEN
                  PQMS(9,N) = IQMWN
                  IF(JFIRST(5).EQ.0) THEN
                     IF(LPRINT) THEN
                        IF(ALL) THEN
                           if(lprint) print'("    ... QMWN set to ",
     .                      F3.0," on ALL levels")', PQMS(9,N)
                        ELSE
                           if(lprint) print'("    ... QMWN set to ",
     .                      F3.0," on ALL pressure levels between",I5,
     .                      " and",I5," mb, inclusive")',
     .                      PQMS(9,N),NINT(PMAX(M)),NINT(PMIN(M))
                        ENDIF
                     ENDIF
                  ENDIF
                  IWQM(ITYP,IQMWN) = IWQM(ITYP,IQMWN) + 1
                  JFIRST(5) = 1
               ENDIF
            ENDIF
         ENDDO
         ENDIF
         IF(LPRINT) THEN
            IF(PMAX(M).GE.BMISS .AND. PMIN(M).LE.0) THEN
               IF(IQMPR.GE.0 .AND. JFIRST(1).EQ.0) THEN
                  print'("    ... QMPR NOT changed because no ",
     .             "qualifying pressure levels found")'
               ENDIF
               IF(IQMGP.GE.0 .AND. JFIRST(2).EQ.0) THEN
                  print'("    ... QMGP NOT changed because no ",
     .             "qualifying pressure levels found")'
               ENDIF
               IF(IQMAT.GE.0 .AND. JFIRST(3).EQ.0) THEN
                  print'("    ... QMAT NOT changed because no ",
     .             "qualifying pressure levels found")'
               ENDIF
               IF(IQMDD.GE.0 .AND. JFIRST(4).EQ.0) THEN
                  print'("    ... QMDD NOT changed because no ",
     .             "qualifying pressure levels found")'
               ENDIF
               IF(IQMWN.GE.0 .AND. JFIRST(5).EQ.0) THEN
                  print'("    ... QMWN NOT changed because no ",
     .             "qualifying pressure levels found")'
               ENDIF
            ELSE
               IF(IQMPR.GE.0 .AND. JFIRST(1).EQ.0) THEN
                  print'("    ... QMPR NOT changed because no ",
     .             "qualifying pressure levels found between",I5," and",
     .             I5," mb, inclusive")', NINT(PMAX(M)),NINT(PMIN(M))
               ENDIF
               IF(IQMGP.GE.0 .AND. JFIRST(2).EQ.0) THEN
                  print'("    ... QMGP NOT changed because no ",
     .             "qualifying pressure levels found between",I5," and",
     .             I5," mb, inclusive")', NINT(PMAX(M)),NINT(PMIN(M))
               ENDIF
               IF(IQMAT.GE.0 .AND. JFIRST(3).EQ.0) THEN
                  print'("    ... QMAT NOT changed because no ",
     .             "qualifying pressure levels found between",I5," and",
     .             I5," mb, inclusive")', NINT(PMAX(M)),NINT(PMIN(M))
               ENDIF
               IF(IQMDD.GE.0 .AND. JFIRST(4).EQ.0) THEN
                  print'("    ... QMDD NOT changed because no ",
     .             "qualifying pressure levels found between",I5," and",
     .             I5," mb, inclusive")', NINT(PMAX(M)),NINT(PMIN(M))
               ENDIF
               IF(IQMWN.GE.0 .AND. JFIRST(5).EQ.0) THEN
                  print'("    ... QMWN NOT changed because no ",
     .             "qualifying pressure levels found between",I5," and",
     .             I5," mb, inclusive")', NINT(PMAX(M)),NINT(PMIN(M))
               ENDIF
            ENDIF
         ENDIF
         IF(MAX(JFIRST(1),JFIRST(2),JFIRST(3),JFIRST(4),JFIRST(5)).GT.0)
     .    THEN
            PQMS_8=PQMS
C  Encode any new q.m.`s
            IF(NLEV.GT.0) CALL UFBINT(LUBFJ,PQMS_8,10,NLEV,IRET,PQMST)
         ENDIF
      ENDIF

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    PRSRNG
C   PRGMMR: KEYSER           ORG: NP22       DATE: 2016-08-09
C
C ABSTRACT: PARSES VERTICAL SIGNIFICANCE QUALIFIER AND PRESSURE RANGES
C   FOR APPLYING QUALITY MARKS ENTRIES FROM A PARTICULAR RECORD (CARD)
C   IN THE SDMEDIT FLAG FILE.  STORES THIS INFORMATION IN COMMON BLOCK
C   /UEDIT/ FOR LATER USE BY SUBROUTINE APPLY.  THIS SUBROUTINE IS
C   CALLED ONLY FOR UPPER AIR AND AIRCRAFT REPORT TYPES ("UPA" AND
C   "ACF", RESPECTIVELY).
C
C PROGRAM HISTORY LOG:
C 1997-02-01  J. WOOLLEN -- ORIGINAL AUTHOR
C 2004-02-02  D. KEYSER  -- REPLACED CALL TO "BORT" WITH CALL TO
C       "ERREXIT"; STREAMLINED CODE; ADDED DOCBLOCK AND COMMENTS
C 2004-05-17  D. KEYSER  -- ADDED ERROR RETURN ARGUMENT (IER) - WHERE
C       =1 INDICATES VERTICAL SIGNIFICANCE QUALIFIER AND PRESSURE
C       RANGES COULD NOT BE PARSED FROM THIS RECORD IN SDMEDIT FLAG
C       (BEFORE IS CALLED ERREXIT AND ABORTED PROGRAM IN THIS
C       SITUATION)
C 2008-06-10  D. KEYSER   REPLACED CALL TO BUFRLIB ROUTINE PARSEQ WITH
C     CALL TO BUFRLIB ROUTINE PARSTR {PARSEQ HAS BEEN REPLACED WITH
C     PARSTR IN LATEST (28 MAY 2008) VERSION OF THE BUFRLIB, PARSEQ HAS
C     BEEN MARKED AS OBSOLETE}
C 2014-03-05  D. A. KEYSER -- INCREASED MAXIMUM NUMBER OF TIME- AND
C     REPORT TYPE-RELEVANT ENTRIES ALLOWED IN THE SDMEDIT FLAG FILE
C     FROM 1000 TO 2000 (PARAMETER "MEDT")
C 2016-08-09  D. A. Keyser -- Replace assignment of BMISS and instead
C     pass in R*8 value set in main program (using GETBMISS) via COMMON.
C
C USAGE:    CALL PRSRNG (CARD, M, IER)
C   INPUT ARGUMENT LIST:
C     CARD     - CHARACTER*128, STRING CONTAINING ONE ENTIRE LINE
C                (RECORD) AS READ IN FROM THE SDMEDIT FLAG FILE
C     M        - INTEGER, INDEX IN MEMORY POINTING TO THE RECORD NUMBER
C                OF THIS PARTICULAR CARD
C
C   OUTPUT ARGUMENT LIST:
C     IER      - ERROR RETURN CODE (=0 SUCCESSFUL RETURN; =1 PROBLEM,
C                VERTICAL SIGNIFICANCE QUALIFIER AND PRESSURE RANGES
C                COULD NOT BE PARSED FROM RECORD IN SDMEDIT FLAG FILE)
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  WCOSS (Linux)
C
C$$$
      SUBROUTINE PRSRNG(CARD,M,IER)

      PARAMETER (MEDT=2000) ! Allows up to 2000 time- and report type-
                            ! relevant entries in the SDMEDIT flag file

      CHARACTER*128 CARD
      CHARACTER*20  PLEV(11)

      REAL(8)      BMISS

      COMMON /UEDIT/ IVSG(MEDT),PMIN(MEDT),PMAX(MEDT)
      COMMON /BUFRLIB_MISSING/BMISS

C  LOOK FOR A VERITCAL SIGNIFICANCE QUALIFIER IN THE SDMEDIT ENTRY
C  ---------------------------------------------------------------

      IF(CARD(49:52).EQ.'SURF') THEN
         IVSG(M) = 64   ! Surface level
      ELSEIF(CARD(49:52).EQ.'MAND') THEN
         IVSG(M) = 32   ! Mandatory level
      ELSEIF(CARD(49:52).EQ.'SIGT') THEN
         IVSG(M) =  4   ! Significant temperature level
      ELSEIF(CARD(49:52).EQ.'SIGW') THEN
         IVSG(M) =  2   ! Significant wind level
      ELSE
         IVSG(M) =  0   ! Vertical significance qualifier not entered
      ENDIF

C  LOOK FOR A PRESSURE OR A PRESSURE RANGE IN THE SDMEDIT ENTRY
C  (FOR APPLYING Q.M.'s) (default is all pressure levels)
C  ------------------------------------------------------------

      PRS1 = 0
      PRS2 = BMISS

      IF(CARD(55:65).NE.'-----------') THEN
         CALL PARSTR(CARD(55:65),PLEV,11,NPLV,' ',.TRUE.)
         IF(NPLV.GE.1) READ(PLEV(1),'(F8.0)',ERR=900) PRS1
         IF(NPLV.GE.2) THEN
            READ(PLEV(2),'(F8.0)',ERR=900) PRS2
         ELSEIF(NPLV.EQ.1) THEN
             PRS2 = PRS1  ! Only one pressure level
         ENDIF
      ENDIF

C  Account for order variability in the pressure range entries
C  -----------------------------------------------------------

      PMIN(M) = MIN(PRS1,PRS2)
      PMAX(M) = MAX(PRS1,PRS2)

C  EXITS
C  -----

      IER = 0
      RETURN

900   CONTINUE
      PRINT *, '#####PRSRNG: ERROR READING PRESSURE RANGE FROM ENTRY ',
     . 'IN MEMORY RECORD',M,': '
      PRINT *, CARD
      IER = 1
      RETURN

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    IQMF
C   PRGMMR: KEYSER           ORG: NP22       DATE: 2004-02-02
C
C ABSTRACT: THIS FUNCTION CONVERTS A QUALITY MARK FROM CHARACTER FORM
C   TO NUMERIC FORM.
C
C PROGRAM HISTORY LOG:
C 1997-02-01  J. WOOLLEN -- ORIGINAL AUTHOR
C 2004-02-02  D. KEYSER  -- ADDED DOCBLOCK; CHANGED TO AN INTEGER
C       FUNCTION (WAS QMF); MISSING QUALITY MARK IN CHARACTER FORM
C       (I.E., " ") RETURNS -99
C
C USAGE:    IQMF (FLAG)
C   INPUT ARGUMENT LIST:
C     FLAG     - CHARACTER*1, QUALITY MARK IN CHARACTER FORM
C                   'H' - FOR HOLD (KEEP)
C                   'R' - FOR REJECT (FROM REJECT LIST)
C                   'P' - PURGE
C
C   OUTPUT ARGUMENT LIST:
C     IQMF     - INTEGER, QUALITY MARK IN NUMERIC FORM
C                    0  - FOR HOLD (KEEP)
C                   12  - REJECT (FROM REJECT LIST)
C                   14  - PURGE
C                  -99  - NEITHER HOLD, REJECT NOR PURGE
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  WCOSS (Linux)
C
C$$$
      FUNCTION IQMF(FLAG)

      CHARACTER*1 FLAG

      IQMF = -99
      IF(FLAG.EQ.'H') THEN
         IQMF = 0
      ELSEIF(FLAG.EQ.'R') THEN
         IQMF = 12
      ELSEIF(FLAG.EQ.'P') THEN
         IQMF = 14
      ENDIF

      RETURN
      END
