#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#               
# Script name:   bufr_dump_obs.sh   Driver for the obs. data dump util
#
# Author:        D.A. Keyser        Org: NP22           Date: 2017-01-10
#
# Abstract: This script is a driver for the all-purpose BUFR observation
#   database tank dump utility, dumpjb.  It's main purpose is to output BUFR
#   data dumps to the appropriate $COMROOT directory and path file naming
#   structure used by the operational NCEP networks.  The dumping of any number
#   of BUFR data types can thus be accomplished for all NCEP networks via a
#   single line in the analyses scripts which executes this driver script.  A
#   secondary purpose (if so requested) is to: 1) run the prepssmi program
#   which will read SSM/I data from one of three possible just-generated BUFR
#   dump sources [FNOC "operational" products, NCEP Neural Net-3 products, or
#   brightness temperatures (the latter will be used to generate products in-
#   line in prepssmi)], reprocess the data (most likely generating superobs),
#   and output the reprocessed data into a BUFR file ("spssmi"); 2) run a
#   series of three programs (dcodcloc, dataqc, datasort) which will read ERS
#   scatterometer data from the just-generated ERS BUFR dump ("erswnd"), do
#   some special reprocessing of these data, and output the reprocessed data
#   into a BUFR file ("erscat"); 3) run the dcodquikscat program which will
#   read Quikscat scatterometer data from the just-generated Quikscat BUFR dump
#   ("qkscat"), do some special reprocessing of these data (most likely
#   generating superobs), and output the reprocessed data into a BUFR file
#   ("qkswnd"); 4) run the dcodquikscat program which will read ASCAT
#   scatterometer data from the just-generated ASCAT BUFR dump ("ascatt"), do
#   some special reprocessing of these data (possibly generating superobs), and
#   output the reprocessed data into a BUFR file ("ascatw"); 5) run the
#   supertmi program which will read TRMM TMI data from the just-generated TRMM
#   TMI BUFR dump ("trmm") and superob these data and output the reprocessed
#   data into a BUFR file ("sptrmm"); 4) run the dcodwindsat program which will
#   read WindSat scatterometer data from the just-generated WindSat BUFR dump
#   ("wndsat"), do some special reprocessing of these data (including possibly
#   generating superobs), and output the reprocessed data into a BUFR file
#   ("wdsatr").  All output SSM/I, ERS, Quikscat, ASCAT, TRMM TMI and WindSat
#   files are written to the same $COMROOT directory and path file naming
#   structure as the original BUFR data dump files; also all five are then
#   ready to be input to subsequent programs (the first four and the sixth to
#   PREPOBS_PREPDATA and the fifth one to GLOBAL_SSI).  Upon completion of the
#   dumping of requested data types, and the completion of the prepssmi, ERS,
#   Quikscat, ASCAT, TRMM TMI and/or WindSat reprocessing programs (if
#   requested), a status file can be generated (by request) and written to the
#   same path as the dump files. The ASCII status file contains a summary of
#   report counts for all requested subtypes that have been dumped.  It also
#   contains information on the return code for each data group dumped.
#
#   It should be noted that this script can output data dumps to any file path
#   location specified by the imported variable "$COMSP".  It is not tied to
#   the production $COMROOT (e.g., /com or /com2) structure, and is thus
#   suitable for non-operational "over-the-counter" type runs.
#
# Script history log:
# 1996-09-27  D.A. Keyser -- Original version for implementation
# 1997-02-19  D.A. Keyser -- A condition code of 4 out of prepssmi will no
#    longer cause a fatal error in this script
# 1997-04-14  D.A. Keyser -- Added optional ERS scatterometer data processing
#    section to this script
# 1999-02-03  D.A. Keyser -- Total y2k compliance as central date (input script
#    positional parameter 1) is now ALWAYS expected to contain a 4-digit year
# 1999-03-08  D.A. Keyser -- The ucl for the datasort program modified since
#    this program now genrates an NCEP-format BUFR output file (copied to /com
#    for IBM PREPDATA program) as well as the previous WMO BUFR file (still
#    used by operational PREPDATA program on Cray); qsub added to execute new
#    version of prepssmi which will generate an NCEP-BUFR spssmi file and copy
#    it to /com for IBM PREPDATA program
# 1999-06-01  D.A. Keyser -- The ucl for the prepssmi program modified since
#    this program now genrates an NCEP-format BUFR output file (copied to /com
#    for PREPDATA program); the ucl for the datasort program modified since
#    this program now generates only an NCEP-format BUFR output file (copied to
#    /com for PREPDATA program); modified to run on IBM SP
# 2000-02-25  D.A. Keyser -- Modified to run on the IBM-SP
# 2000-05-16  D.A. Keyser -- Added hybrid processing option in SSM/I processing
#    (PREPSSMI_PRODTYPE=COMBINATION)
# 2000-06-26  D.A. Keyser -- Added optional Quikscat scatterometer data
#    processing section to this script; input positional parameter "ntype" ($3)
#    is no longer used - all outputs are tied to the imported variable "$COMSP"
#    (ntype remains as a dummy variable)
# 2001-11-02  D.A. Keyser -- Added optional TRMM TMI data processing section to
#    this script
# 2003-08-13  D.A. Keyser -- Checks to see if a dump file qualifies as a
#    "RESTRICTED" data type - if so, changes group to "rstprod" and permission
#    to "640" (if CHGRP_RSTPROD=YES); set-up to list counts by satellite id in
#    output status file copied to /com if they are present in output file that
#    this script greps; method for greping out data counts from dump output
#    files modified such that if dump file in $COMSP is missing or empty, the
#    status file will always stamp out ZERO reports regardless of what was in
#    the dump output files
# 2004-01-15  D.A. Keyser -- Compares AFWA vs. ARINC ACARS report counts in
#    AIRCAR dump and generates a flag file (*aircar_status_flag*) in $COMSP
#    path indicating which type should be processed in subsequent PREPBUFR
#    steps - this allows AFWA ACARS to serve as a backup when ARINC ACARS is
#    down
# 2004-01-20  D.A. Keyser -- Removed all references to "RUC2B" (folding in its
#    00 and 12Z processing with RUC2A)
# 2004-02-20  D.A. Keyser -- Removed logic that handled split QuikSCAT dump
#    processing (qksca1 --> qkswn1 for first 1/2 of dump period, and qksca2 -->
#    qkswn2 for second 1/2 of dump period) - this was originally done to speed
#    up dumps on slower platforms, but this is no longer necessary - assumes
#    only qkscat is dumped and generates reprocessed dump qkswnd from it
# 2006-03-02  D.A. Keyser -- Added logic to grep out the number of Level 2
#    radial wind and reflectivity replications for each subtype from the dump
#    output files and write to the dump status file; imports new script
#    variable "JOB_NUMBER" {currently either 1, 2, or null (default)} which is
#    appended to the name of the dump status file in /com - this allows two
#    unique status files to be written from two different dump jobs for the
#    same network and cycle time (these are later combined into a single status
#    file in the dump post job)
# 2006-06-06  D.A. Keyser -- Added optional WindSat scatterometer data
#    processing section to this script; added new network option "rtma" (Real-
#    Time Mesoscale Analysis); now looks first for external data cards in
#    optional QuikScat, TRMM TMI and WindSat processing and only if not found
#    uses hardwired "herefiles" in this script as the data cards (adds more
#    fexibility)
# 2007-05-25  D.A. Keyser -- Set-up parm cards for bufr_dcodwindsat to superob
#    on 1 degree lat/lon boxes (had been set to not superob); missing "airsev"
#    dump r.c. reduced from 22 to 5 for all NAM and tm03 NDAS network runs;
#    missing MODIS (Aqua/Terra) IR/WV satwnd dump r.c. reduced from 11 to 4 for
#    all NAM and tm03 NDAS network runs
# 2008-05-05  D.A. Keyser -- Handles ASCAT data (including reprocessing); in
#    grep for dump counts to write into status file, can now skip past dump
#    group numbers (i.e., .DUMP_groupN.) which may not have been included in
#    dump in parent driver script, before would stop dump count grep as soon as
#    first dump group that was not processed was hit
# 2008-11-18  D.A. Keyser -- Added lightning data ("lghtng") to the list of
#    dump types which are considered to be restricted when CHGRP_RSTPROD = YES
# 2010-04-16  D.A. Keyser -- Updated the list of dump types which will either
#    have their return code changed from 11 to 4 if they are missing or from 22
#    to 5 if they are missing because they are expected to either OFTEN or
#    ALWAYS be missing; minor changes to account for new dump type ssmisu;
#    minor changes to account for possible dump2 jobs in all networks
#    (currently dump2 runs only in nam and ndas); will not create an empty
#    spssmi dump file if prepssmi processing does not run
# 2012-01-04  D.A. Keyser -- Updated to handle new "rap" (Rapid Refresh)
#    network and its model runs "rap", "rap_p" and "rap_e"; since TAMDAR data
#    (all types) no longer available, missing dumps of types "tamdar"
#    (004.008), "tmdarp" (004.012) and "tmdarc" (004.013) now get dump r.c.
#    reduced from 22 to 5 for all networks and cycle times
# 2012-05-09  D.A. Keyser -- Improved computation time greatly by having each
#    exeuction of this script in a particular job (which normally runs this
#    script in multiple, simultaneous background threads) generate preliminary
#    report count information (by subtype) which will later be used when the
#    status file is generated - rather than waiting until the last execution of
#    this script (which is not threaded) to do so all at once when the status
#    file is generated; now checks to make sure that report dump counts can
#    never exceed the constrained limit of 9999999 in the dump status file
#    listing - in the rare event that a count does exceed this value, it is set
#    to 9999999 in the dump status file and a diagnostic message is posted to
#    the joblog file and to the dump status file
# 2013-01-17  S. Melchior -- Modified code to properly run on WCOSS system. 
#    Replaced all usage of "timex" with "time -p."  Replaced script variables
#    XLFUNIT_n with FORTn (where n is the unit number connected to the filename
#    defined by the variable FORTn) - needed because ifort uses FORTn.   
#    This script is now set to run under ksh shell as the default.
# 2013-12-02  D.A. Keyser -- Imports new environment variable $HOMEobsproc_dump
#    which points to root directory path for dump subdirectories (in production
#    this is normally /nwprod/obsproc_dump.vX.Y.Z where X.Y.Z version number
#    being used, usually the latest); and imports new environment variable
#    $HOMEobsproc_network which points to directory path for network-specific
#    dump subdirectories (in production this is normally
#    /nwprod/obsproc_NETWORK.vX.Y.Z where NETWORK is, e.g., global, nam, rap,
#    rtma, urma, and x.y.z is version number being used, usually the latest) -
#    these replace /nw${envir}.
# 2016-04-22  D.A. Keyser -- Use NCO-established variables to point to root
#    directories for main software components and input/output directories in
#    order to run on WCOSS Phase 1 or Phase 2 (here, $COMROOT which replaces
#    hardwire to "/com"). Use NCO-established variables (presumably obtained
#    from modules) to point to prod utilities:
#      - $GRBINDEX from module grib_util (default or specified version) which
#        replaces executable grbindex in non-versioned, horizontal structure
#        utility directory path defined by imported variable $EXGRBIX.
#    No longer references unused utility scripts getges.sh or setup.sh. Replaced
#    path to prepobs_prepssmi NESDIS land/sea tag (mask) file from non-
#    versioned, horizontal structure utility directory $utilparm to versioned,
#    vertical structure fixed file directory $FIXbufr.
# 2017-01-10  D.A. Keyser
#    - Added ground-based GPS ("gpsipw") and MT-SAPHIR btemps ("saphir") to the
#      list of dump types which are considered to be restricted when
#      CHGRP_RSTPROD = YES.
#      BENEFIT: For "gpsipw", allows temporary logic in obsproc_global.v2.3.0
#               script exglobal_dump.sh.ecf to be removed in
#               obsproc_global.v2.4.0; allows temporary logic in
#               obsproc_nam.v2.2.0 script exnam_dump.sh.ecf to be removed in
#               obsproc_nam.v2.3.0; allows temporary logic in obsproc_rap.v2.2.0
#               script exrap_dump.sh.ecf to be removed in obsproc_rap.v2.3.0;
#               and allows temporary logic in obsproc_cdas.v2.1.0 script
#               excdas_dump.sh.ecf to be removed in obsproc_cdas.v2.2.0.
#    - Corrected logic error which prevented the number of actual reports from
#      being printed in a diagnostic joblog message when the number of reports
#      exceeded the status file limit of 9999999 for dump group mnemonics with
#      less than 6 characters.
# 2017-11-14 JWhiting 
#    - Fixed bug exposed when fractional center date/time values are provided
#      in positional parameter $1 (cendat); various logic tries to parse the
#      value to pick off the whole number hour value, but this fails when
#      fractional values are sent; added new variable icendat variable which
#      uses ${variable%pattern} shell syntax to truncate decimal content.
#    - added rtma_ru_dump to tests on job name to set run name for status report
#      output); also added leading wildcard ("*") to all job name tests to
#      account for implementation parallel runs which include a leading "p" in
#      the job names.
#    - set default value for imported shell variable prepssmi to NO
#      NOTE: Logic in this script references prepobs_prepssmi.sh script which
#       was never ported to the current WCOSS systems.  The imported shell
#       variable prepssmi is always set to NO in all production runs, but if it
#       were ever to be set to YES, this script would fail.
#    - set default value for imported shell variable prepersd to NO
#      Note: ERS scatterometer wind reprocessing executables (dcodcloc, dataqc,
#       datasort) have not been ported to current WCOSS systems and would fail
#       if invoked.
#
# 2018-12-07  C. Hill -- Each subtype of FM42-based sonde data (002.00[1-5]) is
#      paired with its corresponding subtype of BUFR-based data (002.10[1-5])
#      within the 'pattern' list of potentially missing subtypes (i.e. tanks).
#
# 2021-03-09 SMelchior -- Included gpsro among the data types whose dump files
#    are restricted.
#
# Usage: bufr_dump_obs.sh  yyyymmddhh hh<.hh> ntype dgrp1 dgrp2 ... dgrpN
#
#   Input script positional parameters:
#     yyyymmddhh<.hh> $1  - center of time window for dump
#     hh<.hh>         $2  - radius of time window for dump
#     ntype           $3  - reserved, currently has no meaning
#     dgrp1           $4  - data group 1 - see dumpjb for valid data group list
#     dgrp2               - data group 2
#     ...                ...
#     dgrpN           $#  - data group N
#      NOTE: If data group 1 is set to the string 'null', no data dumps
#            will be performed regardless of the remaining data groups
#            listed.  However, if "STATUS=YES" (see below) a status
#            file containing reports counts/info from all previous runs
#            of this script by a particular parent script will be
#            generated and all outputs from previous runs of this script
#            in the various $DATA/job${DUMP_NUMBER} directories will be
#            combined into the $DATA directory.  This is currently used
#            when running this script in simultaneous background jobs -
#            after all of the background jobs have completed, a final
#            job is run with this string set to null.
#
#   Imported Shell Variables:
#     (NOTE: These do not include most of the variables listed in the
#            dumpjb docblock that are specific to dumpjb - see dumpjb script
#            for a list of the dumpjb variables)
#
#     These must ALWAYS be exported from the parent script --
#
#     COMROOT       Root to input/output "com" directory (in production,
#                   normally either "/com" for WCOSS  Phase 1 or "/com2" for
#                   WCOSS Phase 2)
#     DUMP_NUMBER - string indicating the number associated with this
#                   particular run of this script (needed when this script is
#                   run in simultaneous background jobs)
#     DATA        - string indicating the initial (temporary) directory path
#                   to output data destination
#                   (NOTE: Each particular run of this script will write
#                          output data to $DATA/job$DUMP_NUMBER directory;
#                          only the final run with STATUS=YES will combine
#                          outputs for individual runs into $DATA directory.
#                          This allows this script to run in simultaneous
#                          background jobs.)
#     NET         - string indicating system network (i.e., "arc", "gfs",
#                   "nam", "gdas", "rap" (replacing "ruc"), "rucs", "dump",
#                   "rtma") --
#                   points to proper data card file for programs executed here
#                   and to echo in dump status file
#                   NOTE: NET is changed to gdas in the parent Job script for
#                         the gdas RUN (was gfs - NET remains gfs for gfs RUN)
#     COMSP       - string indicating the final directory/filename path to
#                   output data destination
#                   (e.g., "$COMROOT/nam/prod/ndas.20160428/ndas.t12z.")
#                   {NOTE: If the imported variable "SENDCOM" (see below)
#                          is "NO", then COMSP is hardwired to the string
#                          "$DATA/"}
#     tmmark      - string indicating hour for center time for dump
#                   relative to the analysis time, it is a qualifier in the
#                   output data dump file names (e.g., "tm12", "tm09",
#                   "tm06", "tm03", "tm00")
#     $HOMEbufr_dump - Formerly $HOMEobsproc_dump prior to WCOSS2. String
#                      indicating root directory path to dump subdirectories
#                      (in production this is normally
#                      $NWROOT/bufr_dump.vX.Y.Z where X.Y.Z is version
#                      number being used, usually the latest)
#     $HOMEobsproc   - Formerly $HOMEobsproc_network prior to WCOSS2.
#                      String indicating directory path to network-specific
#                      dump subdirectories (in production this is normally
#                      $NWROOT/obsproc.vX.Y.Z where X.Y.Z is version number
#                      being used, usually the latest)
#
#     These will be set to their default value in this script if not exported
#      from the parent script --
# 
#     JOB_NUMBER  - string indicating the number associated with this
#                   particular dump job (needed when the overall network dump
#                   is split into more than one dump job)
#                   Default is "" (null)
#     STATUS      - string: if = 'YES' will generate a status file which is
#                   written to the same path as the dump files.  This is
#                   written out after all dump files have been created, and
#                   contains both report counts (by subtype) and return code
#                   information for each data group dumped.
#                           if = 'NO' will not generate a status file but will
#                   generate preliminary report count information (by subtype)
#                   which will later be used when the status file is generated.
#                           if = 'NEVER' will not generate either a status file
#                   nor preliminary report count information (by subtype)
#                   generated (i.e., a status file is not being generated by
#                   the executing job).
#                   Default is "YES"
#                   NOTE: If this script is executed more than one time by
#                         the parent script, "STATUS" should be imported as
#                         "NO" unless this is the FINAL execution of this 
#                         script.  The status file will always contain
#                         information from all previous runs of this
#                         script by a particular parent script. The last
#                         execution of this script MUST ALWAYS set STATUS=YES
#                         since this combines output from all previous
#                         executions of this script which write to the
#                         directory $DATA/job${DUMP_NUMBER} . 
#     envir       - string indicating environment under which job runs
#                   (either "prod" or "test")
#                   Default is "prod"
#     DUMP        - string indicating path for dump script file
#                   Default is "$HOMEbufr_dump/ush/dumpjb"
#     LOUD        - string: if != 'off' will turn on script trace (set -x)
#                   in dump script ($DUMP) execution (does NOT apply to
#                   this script)
#                   Default is "off"
#     jlogfile    - string indicating path for sms joblog file
#                   Default is null string of length zero
#     job         - string indicating the job name of the parent script
#                   (i.e., "nam_dump_12")
#                   Default is "j????"
#     cycle       - string indicating the analysis cycle time (i.e., "t00z"
#                   or "t18z")
#                   Default is "t??z"
#                   (Note: This is not critical to processing, it is used
#                          only for generating a header to the dump status
#                          file)
#     SENDCOM     - string: if = 'NO' will redefine "COMSP" variable to be
#                   "$DATA/", regardless of its imported value - this has
#                   the effect of preventing any files from going to an
#                   operational $COMROOT (e.g., /com or /com2) directory path,
#                   and instead sending them to the "DATA" directory
#                   Default is "YES"
#     pgmout      - string indicating path for standard output file (output
#                   always contatenated onto this file)
#                   Default is "$DATA/allout"
#     prepssmi    - string: if = 'YES' will run prepobs_prepssmi to reprocess
#                   SSM/I data (most likely generating superobs) into BUFR
#                   file copied to the designated path to output data
#                   destination; if 'NO' will not run prepobs_prepssmi program
#                   Default is "YES"
#     prepersd    - string: if = 'YES' will reprocess ERS scatterometer wind
#                   data by running three separate programs (dcodcloc, dataqc,
#                   datasort) to ultimately generate a quality controlled and
#                   sorted ERS data file in BUFR copied to the designated path
#                   to output data destination; if 'NO' will not reprocess ERS
#                   scatterometer data by running these three programs
#                   Default is "YES"
#     prepqksd    - string: if = 'YES' will run wave_dcodquikscat to reprocess
#                   Quikscat scatterometer wind data (most likely generating
#                   superobs) into BUFR file copied to the designated path to
#                   output data destination; if 'NO' will not run
#                   wave_dcodquikscat program
#                   Default is "YES"
#     prepascd    - string: if = 'YES' will run wave_dcodquikscat to reprocess
#                   ASCAT scatterometer wind data (possibly generating
#                   superobs) into BUFR file copied to the designated path to
#                   output data destination; if 'NO' will not run
#                   wave_dcodquikscat program
#                   Default is "YES"
#     preptrmm    - string: if = 'YES' will run bufr_supertmi to reprocess
#                   (superob) TRMM TMI data into BUFR file copied to the
#                   designated path to output data destination; if 'NO' will
#                   not run bufr_supertmi program
#                   Default is "YES"
#     prepwindsat - string: if = 'YES' will run bufr_dcodwindsat to reprocess
#                   WindSat scatterometer data into BUFR file copied to the
#                   designated path to output data destination; if 'NO' will
#                   not run bufr_dcodwindsat program
#                   Default is "YES"
#     CHGRP_RSTPROD - string: if = 'YES' will check to see if a dump file
#                   qualifies as a "RESTRICTED" data type and if so, will
#                   change the file's group to "rstprod" and its permission to
#                   "640" so that it can only be read by users in the rstprod
#                   group
#                   Default is "YES"
#
#     These apply ONLY for imported shell variable "prepssmi" set to YES
#      and will be set to their default value in this script if not exported
#      from the parent script --
#
#     EXECbufr    - string indicating directory path for bufr_combfr
#                   executable
#                   Default is "$HOMEbufr_dump/exec"
#     EXECPREP    - string indicating directory path for prepobs_prepssmi
#                   executable
#                   Default is "$HOMEbufr_dump/exec"
#     FIXPREP     - string indicating directory path for prepobs_prepssmi bufr
#                   mnemonic table (this is a partial table, ush
#                   prepobs_prepssmi.sh generates the complete table based on
#                   namelist switches in the prepobs_prepssmi data cards)
#                   Default is "$HOMEbufr_dump/fix"
#     PARMPREP    - string indicating directory path for prepobs_prepssmi
#                   data cards
#                   Default is "$HOMEobsproc/parm"
#     FIXbufr     - string indicating directory path for prepobs_prepssmi
#                   NESDIS land/sea tag (mask) fixed file (only needed if file
#                   input to prepobs_prepssmi contains brightness temperatures
#                   and products are being calculated in-line)
#                   Default is "$HOMEbufr_dump/fix"
#     USHPMI      - string indicating directory path for prepobs_prepssmi
#                   ush script prepobs_prepssmi.sh
#                   Default is "$HOMEbufr_dump/ush"
#     PREPSSMI_PROD_TYPE - string: if = 'GOODBERLET' will input "ssmip" data
#                   dump to prepobs_prepssmi program (FNOC "operational"
#                   derived products); if = 'NEURAL_NET3' will input "ssmipn"
#                   data dump to prepobs_prepssmi program (NCEP Neural Net-3
#                   derived products); if = 'COMBINATION' will input "ssmip"
#                   and "ssmipn" data dumps each into separate executions of
#                   prepobs_prepssmi script and program and then combine the
#                   output spssmi BUFR files into a single file for input to
#                   the prepobs_prepdata program (this is used when it is
#                   necessary to reprocess different variables with different
#                   algorithms; e.g., precipitable water with NN3 algorithm
#                   and wind speed with FNOC algorithm - here the data cards
#                   determine which variable uses which algorithm)
#                   NOTE 1: For "PREPSSMI_PROD_TYPE" = 'COMBINATION', BOTH
#                          "ssmip" and "ssmipn" must appear in the SAME data
#                          group list (i.e., they can't appear in separate
#                          executions of this script in the same job) - their
#                          location in the data group list is not important,
#                          however.
#                   NOTE 2: The value for this variable is overridden if the
#                           prepobs_prepssmi data cards contain IALG=1,2,..,98
#                           which means input the brightness temperature
#                           (ssmit) data dump and let prepobs_prepssmi generate
#                           the products in-line (product type is dependent
#                           upon the value of IALG in data cards).
#                   Default is "GOODBERLET"
#     PMIX        - string indicating executable path for prepobs_prepssmi
#                   program
#                   Default is "$EXECPREP/prepobs_prepssmi"
#     PMIC        - string indicating data card path for prepobs_prepssmi
#                   program
#                   Default is "$PARMPREP/prepobs_prepssmi.${NET}.parm"
#     PMIT        - string indicating mnemonic bufrtable file path for
#                   prepobs_prepssmi program (this is a partial table, ush
#                   prepobs_prepssmi.sh generates the complete table based on
#                   namelist switches in the prepobs_prepssmi data cards)
#                   Default is "$FIXPREP/prepobs_prepssmi.bufrtable"
#     LANDC       - string indicating NESDIS land/sea tag (mask) fixed file
#                   path for prepobs_prepssmi program (only needed if file
#                   input to prepobs_prepssmi contains brightness temperatures
#                   and products are being calculated in-line)
#                   Default is "$FIXbufr/nesdis.lstags.prepssmi"
#
#     These apply ONLY for imported shell variable "prepersd" set to YES
#      and will be set to their default value in this script if not exported
#      from the parent script --
#
#     EXECWAVE    - string indicating directory path for ERS scatterometer
#                   data reprocessing executables (wave_dcodcloc,wave_dataqc,
#                   wave_datasort)
#                   Default is "$HOMEbufr_dump/exec"
#                   NOTE: Also applies for prepqksd=YES or prepascd=YES (see
#                         below)
#     FIXWAVE     - string indicating directory path for ERS scatterometer
#                   data reprocessing fixed fields (including mnemonic
#                   bufrtable file read by wave_datasort)
#                   Default is "$HOMEbufr_dump/fix"
#                   NOTE: Also applies for prepqksd=YES or prepascd=YES (see
#                         below)
#     DCLX        - string indicating executable path for wave_dcodcloc
#                   program
#                   Default is "$EXECWAVE/wave_dcodcloc"
#     DQCX        - string indicating executable path for wave_dataqc
#                   program
#                   Default is "$EXECWAVE/wave_dataqc"
#     DSRX        - string indicating executable path for wave_datasort
#                   program
#                   Default is "$EXECWAVE/wave_datasort"
#     DSRT        - string indicating mnemonic bufrtable file path for
#                   wave_datasort program
#                   Default is "$FIXWAVE/wave_bufrtab.erscat"
#
#     These apply ONLY for imported shell variable "prepqksd" set to YES
#      and will be set to their default value in this script if not exported
#      from the parent script --
#
#     EXECWAVE    - string indicating directory path for wave_dcodquikscat
#                   executable
#                   Default is "$HOMEbufr_dump/exec"
#                   NOTE: Also applies for preperds=YES (see above) or
#                         prepascd=YES (see below)
#     FIXWAVE     - string indicating directory path for wave_dcodquikscat
#                   fixed fields {mnemonic bufrtable file and 0.5 x 0.5
#                   degree land/sea tag (mask) file}
#                   Default is "$HOMEbufr_dump/fix"
#                   NOTE: Also applies for preperds=YES (see above) or
#                         prepascd=YES (see below)
#     PARMWAVE    - string indicating directory path for wave_dcodquikscat
#                   data cards
#                   Default is "$HOMEobsproc/parm"
#                   NOTE: Also applies for prepascd=YES (see below)
#     DQKX        - string indicating executable path for wave_dcodquikscat
#                   program
#                   Default is "$EXECWAVE/wave_dcodquikscat"
#     DQKC        - string indicating data card path for wave_dcodquikscat
#                   program
#                   Default is "$PARMWAVE/wave_dcodquikscat.${NET}.parm"
#                   {If $DQKC not found, reverts to using internal "herefile"}
#     DQKT        - string indicating mnemonic bufrtable file path for
#                   wave_dcodquikscat program
#                   Default is "$FIXWAVE/wave_bufrtab.quikscat"
#     LANDC_DQK   - string indicating 0.5 x 0.5 degree land/sea tag (mask)
#                   fixed file path for wave_dcodquikscat program
#                   Default is "$FIXWAVE/wave_landchxh"
#
#     These apply ONLY for imported shell variable "prepascd" set to YES
#      and will be set to their default value in this script if not exported
#      from the parent script --
#
#     EXECWAVE    - string indicating directory path for wave_dcodquikscat
#                   executable
#                   Default is "$HOMEbufr_dump/exec"
#                   NOTE: Also applies for preperds=YES or prepqksd=YES  (see
#                         above)
#     FIXWAVE     - string indicating directory path for wave_dcodquikscat
#                   fixed fields {mnemonic bufrtable file and 0.5 x 0.5
#                   degree land/sea tag (mask) file}
#                   Default is "$HOMEbufr_dump/fix"
#                   NOTE: Also applies for preperds=YES or prepqksd=YES  (see
#                         above)
#     PARMWAVE    - string indicating directory path for wave_dcodquikscat
#                   data cards
#                   Default is "$HOMEobsproc/parm"
#                   NOTE: Also applies for prepqksd=YES  (see above)
#     DASX        - string indicating executable path for wave_dcodquikscat
#                   program
#                   Default is "$EXECWAVE/wave_dcodquikscat"
#     DASC        - string indicating data card path for wave_dcodquikscat
#                   program
#                   Default is "$PARMWAVE/wave_dcodascat.${NET}.parm"
#                   {If $DASC not found, reverts to using internal "herefile"}
#     DAST        - string indicating mnemonic bufrtable file path for
#                   wave_dcodquikscat program
#                   Default is "$FIXWAVE/wave_bufrtab.ascat"
#     LANDC_DAS   - string indicating 0.5 x 0.5 degree land/sea tag (mask)
#                   fixed file path for wave_dcodquikscat program
#                   Default is "$FIXWAVE/wave_landchxh"
#
#     These apply ONLY for imported shell variable "preptrmm" set to YES
#      and will be set to their default value in this script if not exported
#      from the parent script --
#
#     EXECbufr    - string indicating directory path for bufr_supertmi
#                   executable
#                   Default is "$HOMEbufr_dump/exec"
#     FIXbufr     - string indicating directory path for bufr_supertmi fixed
#                   fields (mnemonic bufrtable file)
#                   Default is "$HOMEbufr_dump/fix"
#     PARMbufr    - string indicating directory path for bufr_supertmi data
#                   cards
#                   Default is "$HOMEobsproc/parm"
#     DTMX        - string indicating executable path for bufr_supertmi
#                   program
#                   Default is "$EXECbufr/bufr_supertmi"
#     DTMC        - string indicating data card path for bufr_supertmi
#                   program
#                   Default is "$PARMbufr/bufr_supertmi.${NET}.parm"
#                   {If $DTMC not found, reverts to using internal "herefile"}
#     DTMT        - string indicating mnemonic bufrtable file path for
#                   bufr_supertmi program
#                   Default is "$FIXbufr/bufr_bufrtab.sptrmm"
#
#     These apply ONLY for imported shell variable "prepwindsat" set to YES
#      and will be set to their default value in this script if not exported
#      from the parent script --
#
#     EXECbufr    - string indicating directory path for bufr_dcodwindsat
#                   executable
#                   Default is "$HOMEbufr_dump/exec"
#     FIXbufr     - string indicating directory path for bufr_dcodwindsat fixed
#                   fields (mnemonic bufrtable file)
#                   Default is "$HOMEbufr_dump/fix"
#     PARMbufr    - string indicating directory path for bufr_dcodwindsat data
#                   cards
#                   Default is "$HOMEobsproc/parm"
#     DWSX        - string indicating executable path for bufr_dcodwindsat
#                   program
#                   Default is "$EXECbufr/bufr_dcodwindsat"
#     DWSC        - string indicating data card path for bufr_dcodwindsat
#                   program
#                   Default is "$PARMbufr/bufr_dcodwindsat.${NET}.parm"
#                   {If $DWSC not found, reverts to using internal "herefile"}
#     DWST        - string indicating mnemonic bufrtable file path for
#                   bufr_dcodwindsat program
#                   Default is "$FIXbufr/bufr_bufrtab.windsat"
#     LANDC_DWS   - string indicating 0.5 x 0.5 degree land/sea tag (mask)
#                   fixed file path for bufr_dcodwindsat program
#                   Default is "$FIXbufr/wave_landchxh"
#
#
#   Modules and files referenced:
#     scripts    :  $DUMP
#                   $USHPMI/prepobs_prepssmi.sh
###############     /nwprod/util/ush/getges.sh (no longer referenced)
#     executables:  $PMIX
#                   $GRBINDEX (presumably from default or specified version of
#                              module grib_util)
#                   $DCLX
#                   $DQCX
#                   $DSRX
#                   $DQKX
#                   $DTMX
#                   $DWSX
#     data cards :  $PMIC ($PMIX)
#     data cards :  $DQKC ($DQKX)
#                   $DTMC ($DTMX)
#                   $DWSC ($DWSX)
#     fixed fields: $FIXWAVE/wave_bufrtab.erscat ($DSRX)
#                   $FIXWAVE/wave_CMOD_DBLUT ($DSRX)
#                   $FIXWAVE/wave_CMOD_QSLUT ($DSRX)
#                   $FIXWAVE/wave_zmask1x1 ($DCLX)
#                   $LANDC_DQK ($DQKX)
#                   $DQKT ($DQKX)
#                   $PMIT ($PMIX)
#                   $DTMT ($DTMX)
#                   $LANDC_DWS ($DWSX)
#                   $DWST ($DWSX)
#                   $LANDC ($PMIX)
#
# Remarks:
#
#     Output dump data set will be in the form (see - @):
#         ${COMSP}dgrpX.${tmmark}.bufr_d
#     (See above for usual string appearance for $COMSP and $tmmark; dgrpX
#      is input positional parameter mnemonic associated with a particular
#      data group)
#
#     prepobs_prepssmi may or may not be run (depending on imported shell
#      variable "prepssmi")
#
#     ERS scatterometer data reprocessing may or may not occur (depending
#      on imported shell variable "prepersd")
#
#     wave_dcodquikscat may or may not be run (depending on imported shell
#      variables "prepqksd" and "prepascd")
#
#     bufr_supertmi may or may not be run (depending on imported shell
#      variable "preptrmm")
#
#     bufr_dcodwindsat may or may not be run (depending on imported shell
#      variable "prepwindsat")
#
#   @ - If imported shell variable "SENDCOM" is set to 'NO', then "COMSP"
#       above is hardwired to be "$DATA/" and output dump data set will be
#       in the form: ${DATA}/dgrpX.${tmmark}.bufr_d
#
#   Condition codes
#     dumpjb -- 
#      0 - all requested data group subtypes found and dumped
#      4 - one or more data group subtype dumps contain zero reports - all
#          of these subtypes are currently EXPECTED to be missing or dump
#          zero reports SOME or ALL of the time for particular data group
#          at the dump cycle time; does not indicate a problem (see $$ below)
#      5 - all data group subtype dumps contain zero reports - this data
#          group is currently EXPECTED to be missing or dump zero reports
#          SOME or ALL of the time at the dump cycle time; does not indicate
#          a problem (see ++ below)
#     11 - one or more data group subtype dumps contain zero reports - at
#          least one of these subtypes is currently EXPECTED to dump reports
#          ALL of the time for particular data group at the dump cycle time;
#          indicates a problem
#     22 - all data group subtype dumps contain zero reports - at least one
#          of the subtypes in this data group is currently EXPECTED to dump
#          reports ALL of the time at the dump cycle time; indicates a
#          problem
#     99 - catastrophic problem -- script aborted
#
#     prepobs_prepssmi -- 
#      0 - program completed successfully
#   < 51 - some problem encountered (see prepobs_prepssmi docblock)
#           (non-fatal)
#   > 50 - catastrophic problem -- script aborted
#
#     wave_dcodcloc --
#      0 - program completed successfully
#   >  0 - some problem encountered (see wave_dcodcloc docblock)
#           (non-fatal)
#
#     wave_dataqc --
#      0 - program completed successfully
#   >  0 - some problem encountered (see wave_dataqc docblock)
#           (non-fatal)
#
#     wave_datasort --
#      0 - program completed successfully
#   >  0 - some problem encountered (see wave_datasort docblock)
#           (non-fatal)
#
#     wave_dcodquikscat -- 
#      0 - program completed successfully
#   >  0 - some problem encountered (see wave_dcodquikscat docblock)
#           (non-fatal)
#
#     bufr_supertmi -- 
#      0 - program completed successfully
#   >  0 - some problem encountered (see bufr_supertmi docblock)
#           (non-fatal)
#
#     bufr_dcodwindsat -- 
#      0 - program completed successfully
#   >  0 - some problem encountered (see wave_dcodwindsat docblock)
#           (non-fatal)
#
#  
#     NOTE: Be careful, if this script ever needs to obtain the file ncepdate
#           from the working directory (and it is written there by parent
#           script), make sure it is correct!  (Right now, this script does
#           not need to obtain the file ncepdate.)
#
#
#  $$ - The following types will have their return code changed from 11 to
#       4 if they are missing because they are currently expected to either
#       OFTEN or ALWAYS be missing.  (Last updated 04/16/10.)
#
#          TYPE                 NETWORK(s)              CENTER TIME(s)
#          ----                 ----------              --------------
#          001.005 (tideg)      RTMA,RAP,RUC,SRUC1^     All
#                               SRUC2^                  All
#          002.001 (raobf)      RAP,RUC                 02-05,08-11,14-17,20-23
#          002.002 (raobm)      All                     All
#          002.003 (raobs)      RAP,RUC                 All
#          002.004 (dropw)      All                     All
#          002.005 (pibal)      NAM,NDAS,RAP,RUC        All
#          002.009 (prflrp)     All                     All
#          002.101 (raobf)      RAP,RUC                 02-05,08-11,14-17,20-23
#          002.102 (raobm)      All                     All
#          002.103 (raobs)      RAP,RUC                 All
#          002.104 (dropw)      All                     All
#          002.105 (pibal)      RAP,RUC                 All
#          004.005 (recco)      All                     All
#          004.007 (acarsa)     All                     All
#          004.008 (tamdar)     All                     All
#xxxxxxxxx 004.008 (tamdar)     NDAS+                   12
#xxxxxxxxx                      RAP,RUC                 09,10,11
#          004.012 (tmdarp)     All                     All
#xxxxxxxxx 004.012 (tmdarp)     All                     08%,09%,10%,11-17,18%
#          004.013 (tmdarc)     All                     All
#xxxxxxxxx 004.013 (tmdarc)     RAP,RUC                 11
#          005.010 (infus)      RAP,RUC                 02,05,08,11,14,17,20,23
#          005.011 (h2ius)      RAP,RUC                 02,05,08,11,14,17,20,23
#          005.012 (visus)      All                     06-11,14%,17%
#          005.021 (infin)      All                     All
#          005.022 (visin)      All                     All
#          005.023 (h20in)      All                     All
#          005.041 (-----)#     NAM                     All
#          005.042 (-----)#     All                     12,18
#                               RAP,RUC                 09-11,13-17,19,20
#          005.043 (-----)#     NAM                     All
#          005.045 (visja)      All                     12,18
#                               RAP,RUC                 09-11,13-17,19,20
#          005.065 (viseu)      NAM,NDAS                00,03*,06
#                               RAP,RUC                 00-07,23
#          005.070 (infmo)      NAM,NDAS+,RAP,RUC       All
#          005.071 (h20mo)      NAM,NDAS+,RAP,RUC       All
#          006.002 (radw25)     RAP,RUC                 00,12
#
#          006.011 (rd2w01)     NAM                     00
#          006.041 (rd2r01)     NAM                     00
#          006.017 (rd2w07)     NAM                     06
#          006.047 (rd2r07)     NAM                     06
#          006.023 (rd2w13)     NAM                     12
#          006.053 (rd2r13)     NAM                     12
#          006.029 (rd2w19)     NAM                     18
#          006.059 (rd2r19)     NAM                     18
#          255.025 (msoflt)     All                     All
#          255.027 (msogeo)     All                     All
#
#  ++ - The following types will have their return code changed from 22 to
#       5 if they are missing because they are currently expected to either
#       OFTEN or ALWAYS be missing.  (Last updated 04/16/10.)
#
#          TYPE                 NETWORK(s)           CENTER TIME(s)
#          ----                 ----------           --------------
#          1bamub               RAP,RUC              08,13
#          1bhrs3               RAP,RUC              04,05,08,10,11,13,17,18,21
#          1bhrs4               RAP,RUC              03,04,05
#          1bmhs                RAP,RUC              03,04,05
#          1bmsu#               NAM                  00,06,18
#          1bhrs2#              NAM                  00,06,18
#          adpupa               NDAS,RAP,RUC         02-05,08-11,14-17,20-23
#          airsev               RAP,RUC              All
#                               NAM                  06,18
#          osbuv8               NAM,NDAS             06      
#                               NDAS+                09      
#          proflr#              SRUC1^               All
#          qkscat#              RAP,RUC              All
#          radwnd               RAP,RUC              01-11,13-23
#          sfcbog               All                  01-11,13-23
#                               GFS                  00,12
#          ssmip                RAP,RUC              01-11,13-23
#          ssmipn               RAP,RUC              01-11,13-23
#          ssmit                RAP,RUC              01-11,13-23
#          wndsat               RTMA,RAP             All
#
#    *-NDAS only
#    %-RAP or RUC only
#    +-TM03 only
#    ^-SRUC1 is SRUC initiating at T+05, SRUC2 is SRUC initiating at T+22
#    #-No longer dumped at time of latest update to this table, included for
#       historical reruns
#    @-Not yet dumped for indicated network(s) at time of latest update to this
#       table, included for parallel testing
#
#
####

set -aux

mkdir -p ${DATA:?}/job${DUMP_NUMBER:?}
cd $DATA/job${DUMP_NUMBER}

#############################################################################
#############################################################################
#                     EXECUTE THE DUMP SCRIPT
#############################################################################
#############################################################################

#============================================
cendat=$1
timwin=$2
ntype=$3
job=${job:-j????}
cycle=${cycle:-t??z}

icendat=${cendat%.*}         # truncate any fractional hour from center date

set +x
echo
echo "cendat = " $cendat
echo "icendat= " $icendat
echo "timwin = " $timwin
echo
echo "job   = " $job
echo "cycle  = " $cycle
echo
set -x
#============================================
shift 3

FORM=ibm

#---------------------------------------------------------------------------
#     The arguments for the DUMP script are described below.         
#---------------------------------------------------------------------------

JOB_NUMBER=${JOB_NUMBER:-""}
echo
echo "JOB_NUMBER = " $JOB_NUMBER
echo
STATUS=${STATUS:-YES}
echo
echo "STATUS = " $STATUS
echo
envir=${envir:-prod}
DUMP=${DUMP:-$HOMEbufr_dump/ush/dumpjb}

LOUD=${LOUD:-off}

if [ "$1" = 'null' ];then
   errdmp=0
else

#---------------------------------------------------------------------------
# The DUMP script  The center of  The radius of  Data Group Names:
# accepts these    the time       the time                
# arguments:       window:        window:
#
# Example.         yyyymmddhh<.hh>  hh<.hh>        dgrp1 dgrp2 ... dgrpN
#---------------------------------------------------------------------------

   $DUMP           $cendat          $timwin        $*      > outout 2>errfile

   errdmp=$?

fi

#.......................................................................

errt=0
typeset -Z2 chr
chr=`expr $icendat % 100`     # use center date w/ fractional hours removed

# Note: The following subtypes are EXPECTED to OFTEN be missing regardless
#       of the center dump time or the data dump network - if only these types
#       are missing the return code of "11" is reduced to "4" (see "for" loop
#       below)

rm pattern

# Mobil land raob, drops, wind profilers orig. from PILOT (pibal) bulletins,
#  reccos, AFWA ACARS, TAMDAR (all types), INSAT satwnds and Georgia & Florida
#  DOT mesonets
cat <<\EOFp > pattern
002.002
002.004
002.009
002.102
002.104
004.005
004.007
004.008
004.012
004.013
005.021
005.022
005.023
255.025
255.027
EOFp

if [[ $job = *nam_dump*_?? ]];then
# The pattern is expanded to include pibals, GMS IR, WV (old SATOB only) and
#  MODIS (Aqua/Terra) IR, WV satwnds for Job nam_dump (or nam_dump2) at all
#  center dump times
cat <<\EOFp1 >> pattern
002.005
005.041
005.043
005.070
005.071
EOFp1
   if [ "$chr" = '00' -o "$chr" = '06' ];then
# The pattern is expanded to include EUMETSAT vis satwnds for Job nam_dump (or
#  nam_dump2) at center dump times 00 or 06Z (nighttime)
cat <<\EOFp1p2 >> pattern
005.065
EOFp1p2
   fi
   if [ "$chr" = '00' ];then
# The pattern is expanded to include NEXRAD Level II radial winds and
#  reflectivity data centered on 01Z for Job nam_dump2 at center dump time 00Z
cat <<\EOFp1p2p1 >> pattern
006.011
006.041
EOFp1p2p1
   elif [ "$chr" = '06' ];then
# The pattern is expanded to include NEXRAD Level II radial winds and
#  reflectivity data centered on 07Z for Job nam_dump2 at center dump time 06Z
cat <<\EOFp1p2p2 >> pattern
006.017
006.047
EOFp1p2p2
   elif [ "$chr" = '12' ];then
# The pattern is expanded to include NEXRAD Level II radial winds and
#  reflectivity data centered on 13Z for Job nam_dump2 at center dump time 12Z
cat <<\EOFp1p2p3 >> pattern
006.023
006.053
EOFp1p2p3
   elif [ "$chr" = '18' ];then
# The pattern is expanded to include NEXRAD Level II radial winds and
#  reflectivity data centered on 19Z for Job nam_dump2 at center dump time 18Z
cat <<\EOFp1p2p4 >> pattern
006.029
006.059
EOFp1p2p4
   fi

elif [[ $job = *gfs_dump*_?? ]];then
# The pattern is expanded to include -nothing right now- for JOB gfs_dump (or
#  gfs_dump2) at all center times
cat <<\EOFp1gfs >> pattern
999.999
EOFp1gfs

elif [[ $job = *rtma_dump*_?? ]];then
# The pattern is expanded to include automated tide gauge reports for JOB
#  rtma_dump at all center times
cat <<\EOFp1rtma >> pattern
001.005
EOFp1rtma

elif [[ $job = *ndas_dump*_tm??_?? ]];then
# The pattern is expanded to include pibals for Job ndas_dump (or ndas_dump2)
#  at all center dump times
cat <<\EOFp1p1 >> pattern
002.005
EOFp1p1
   if [ "$chr" = '00' -o "$chr" = '03' -o "$chr" = '06' ];then
# The pattern is expanded to include EUMETSAT vis satwnds for Job ndas_dump (or
#  ndas_dump2) at center dump times 00, 03 or 06Z (nighttime)
cat <<\EOFp1p3 >> pattern
005.065
EOFp1p3
   fi
   if [[ $job = *ndas_dump*_tm03_?? ]];then
# The pattern is expanded to include MODIS (Aqua/Terra) IR and WV satwnds for
#  Job ndas_dump_tm03 (or ndas_dump2_tm03) at all center dump times
cat <<\EOFp1p4 >> pattern
005.070
005.071
EOFp1p4
      if [ "$chr" = '12' ];then
# The pattern is expanded to include -nothing right now (had been TAMDAR Mesaba
#  aircraft data in 004.008)- for Job ndas_dump_tm03 (or ndas_dump2_tm03) at
#  center dump time 12Z
cat <<\EOFp1p5 >> pattern
999.999
EOFp1p5
      fi
   fi

elif [[ $job = *rap_dump*_?? || $job = *ruc2a_dump*_?? ]];then
# The pattern is expanded to include ship raobs for Jobs rap_dump or ruc2a_dump
#  at all center dump times
# The pattern is also expanded to include pibals for Jobs rap_dump or
#  ruc2a_dump at all center dump times (pibals are already excluded at times
#  other than 00 or 12Z via logic further below - this was added to account for
#  00 and 12Z missing pibals)
# The pattern is also expanded to include automated tide gauge reports for
#  Jobs rap_dump or ruc2a_dump at all center dump times (this was added because
#  since 4/11/2001 these data almost always arrive too late for use by these
#  networks)
# The pattern is also expanded to include MODIS (Aqua/Terra) IR, WV satwnds
#  for Jobs rap_dump or ruc2a_dump at all center dump times
cat <<\EOFp2 >> pattern
002.003
002.005
001.005
005.070
005.071
EOFp2
   if [ "$chr" != '00' -a "$chr" != '12' ];then
# The pattern is expanded to include -nothing right now- for Jobs rap_dump or
#  ruc2a_dump at all center dump times except 00 and 12Z
cat <<\EOFp3p1 >> pattern
999.999
EOFp3p1
   else
# The pattern is expanded to include NEXRAD L2.5 data for Jobs rap_dump or
#  ruc2a_dump at center dump times 00 or 12Z
cat <<\EOFp3p1p1 >> pattern
006.002
EOFp3p1p1
   fi
   if [ "$chr" = '08' -o "$chr" = '09' -o "$chr" = '10' -o "$chr" = '18' ];then
# The pattern is expanded to include -nothing right now (had been TAMDAR PenAir
#  aircraft data in 004.012)- for Jobs rap_dump or ruc2a_dump at center dump
#  times 08, 09, 10 or 18Z (logic further below expands center dump times for
#  Jobs rap_dump or ruc2a_dump to also include these data for 11Z - 18Z,
#   inclusive)
cat <<\EOFp3p2 >> pattern
999.999
EOFp3p2
   fi
   if [ "$chr" -ge '09' -a "$chr" -le '11' ];then
# The pattern is expanded to include -nothing right now (had been TAMDAR Mesaba
#  aircraft data in 004.008)- for Jobs rap_dump or ruc2a_dump at center dump
#  times 09Z - 11Z, inclusive
cat <<\EOFp4p2 >> pattern
999.999
EOFp4p2
      if [ "$chr" = '11' ];then
# The pattern is expanded to include -nothing right now (had been TAMDAR
#  Chautauqua aircraft data in 004.013)- for Jobs rap_dump or ruc2a_dump at
#  center dump time 11Z
cat <<\EOFp4p2p1 >> pattern
999.999
EOFp4p2p1
      fi
   elif [ \( "$chr" -ge '00' -a "$chr" -le '07' \) -o "$chr" = '23' ];then
# The pattern is expanded to include EUMETSAT vis satwnds for Jobs rap_dump or
#  ruc2a_dump at center dump times 23Z - 07Z, inclusive (nighttime)
cat <<\EOFp5p2 >> pattern
005.065
EOFp5p2
   elif [ "$chr" = '14' -o "$chr" = '17' ];then
# The pattern is expanded to include GOES high-density visible satwnds for Jobs
#  rap_dump or ruc2a_dump at center dump times 14 or 17Z (nighttime)
cat <<\EOFp5p2p1 >> pattern
005.012
EOFp5p2p1
   fi
   if [ "$chr" = '02' -o "$chr" = '05' -o "$chr" = '08' -o "$chr" = '11' -o \
        "$chr" = '14' -o "$chr" = '17' -o "$chr" = '20' -o "$chr" = '23' ];then
# The pattern is expanded to include GOES high-density IR and water vapor
#  imager satwnds for Jobs rap_dump or ruc2a_dump at center dump times 02, 05,
#  08, 11, 14, 17, 20 or 23Z
cat <<\EOFp5p2p2 >> pattern
005.010
005.011
EOFp5p2p2
   fi
   if [ "$chr" -ge '09' -a "$chr" -le '20' ];then
# The pattern is expanded to include GMS vis satwnds (old SATOB and new BUFR)
#  for Jobs rap_dump or ruc2a_dump at center dump times 09Z - 20Z, inclusive
#  (nighttime)
cat <<\EOFp5p2p3 >> pattern
005.042
005.045
EOFp5p2p3
   fi
   if [ "$chr" != '00' -a "$chr" != '01' -a \
        "$chr" != '06' -a "$chr" != '07' -a \
        "$chr" != '12' -a "$chr" != '13' -a \
        "$chr" != '18' -a "$chr" != '19' ];then
# The pattern is expanded to include RAOB fixed land for Jobs rap_dump or
#  ruc2a_dump at center dump times 02-05Z, 08-11Z, 14-17Z, or 20-23Z
cat <<\EOFp5p2p4 >> pattern
002.001
002.101
EOFp5p2p4
   fi

elif [[ $job = *sruc_dump*_???? ]];then
# The pattern is expanded to include automated tide gauge reports for Job
#  sruc_dump (both T+0:05 and T+0:22) at all center dump times
cat <<\EOFp3p3 >> pattern
001.005
EOFp3p3
   if [[ $job = *sruc_dump*_??05 ]];then
# The pattern is expanded to include -nothing right now- for Job sruc_dump at
#  T+0:05 (only) at all center dump times
cat <<\EOFp3p4 >> pattern
999.999
EOFp3p4
   fi
fi

if [ "$chr" -ge '6' -a "$chr" -le '11' ];then
# The pattern is expanded to include GOES high-density visible satwnds for
#  all jobs centered on center dump times 6Z - 11Z, inclusive (nighttime)
cat <<\EOFp5 >> pattern
005.012
EOFp5

elif [[ $job = *nam_dump*_18 ]];then
# The pattern is expanded to include -nothing right now- for Job nam_dump (or
#  nam_dump2) at center dump time 18Z
cat <<\EOFp6 >> pattern
999.999
EOFp6

fi

if [ "$chr" -ge '11' -a "$chr" -le '17' ];then
# The pattern is expanded to include -nothing right now (had been TAMDAR PenAir
#  aircraft data in 004.012)- for all jobs at center dump times 11Z - 17Z,
#  inclusive (no flights)
cat <<\EOFp7 >> pattern
999.999
EOFp7
fi

if [ "$chr" = '12' -o "$chr" = '18' ];then
# The pattern is expanded to include  GMS vis satwnds (old SATOB and new BUFR)
#  for all jobs at center dump times 12 or 18Z (nighttime)
cat <<\EOFp8 >> pattern
005.042
005.045
EOFp8
fi

if [ "$1" != 'null' ];then

# Loop through all groups being dumped - assign proper return code to
#  each dumped data group

cat <<EOFds1 >> status1.out

-------------------------------------------------------------------------------

Dump Script run for data groups: $*

EOFds1

for n
do
   echo "                                 -- Data Group $n --" >> status1.out

   grep "DESCRIPTOR" $DATA/${n}.out >> status1.out

   grep ">>>" $DATA/${n}.out > grep1.out
   errgrep=$?
   if [ $errgrep -eq '0' ];then
      nn=`head -n 1 grep1.out`
      msg="**WARNING: $n dump incomplete -- $nn"
      set +u
      [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
      set -u
      echo "$msg" >> outout
      echo "$msg" >> status1.out
      echo " " >> outout
      echo " " >> status1.out
    fi
    rm grep1.out
   errn=`grep "Dump Status: $n " outout | cut -f2 -d=`
   if [ "$errn" -eq '0' ];then
      msg="$n dump SUCCESSFUL, all subtypes contain reports (RC=$errn)"
   elif [ "$errn" -gt '22' ];then
      msg="**FATAL ERROR in $n dump, NO REPORTS DUMPED (RC=$errn)"
   elif [ "$errn" -eq '22' ];then
      msg="**WARNING: $n dump EMPTY, all subtypes missing (RC=$errn)"

######if [[ $job = ruc2a_dump*_?? && $n = satwnd ]];then
#xNote: For Job ruc2a_dump and SATWND at all center dump times except 00, 01,
#xxxxx  04, 07, 10, 12, 13, 16, 19, and 22Z, all subtypes are expected to
#xxxxx  OFTEN be missing - the return code of "22" is reduced to "5" here
######   [ "$chr" != '00' -a "$chr" != '01' -a "$chr" != '04' -a \
######     "$chr" != '07' -a "$chr" != '10' -a "$chr" != '12' -a \
######     "$chr" != '13' -a "$chr" != '16' -a "$chr" != '19' -a \
######     "$chr" != '22' ]  &&  errn=5

######elif [[ $job = ruc2a_dump*_?? && ( $n = airsev || $n = qkscat ) ]];then
      if [[ ( $job = *rap_dump*_?? || $job = *ruc2a_dump*_?? ) && \
            ( $n = airsev || $n = qkscat ) ]];then
# Note: For Jobs rap_dump or ruc2a_dump at all center dump times, AIRSEV (all
#       subtypes) and QKSCAT (all subtypes) are expected to OFTEN be missing -
#       the return code of "22" is reduced to "5" here
         errn=5

      elif [[ ( $job = *rap_dump*_?? || $job = *ruc2a_dump*_?? ) && \
              ( $n = ssmip || $n = ssmipn || $n = ssmit || $n = radwnd ) ]];then
# Note: For Jobs rap_dump or ruc2a_dump and SSMIP, SSMIPN, SSMIT, or RADWND at
#       all center dump times except 00 and 12Z; all subtypes are expected to
#       OFTEN be missing - the return code of "22" is reduced to "5" here
         [ "$chr" != '00' -a "$chr" != '12' ]  &&  errn=5

      elif [[ ( $job = *rap_dump*_?? || $job = *ruc2a_dump*_?? || \
                $job = *ndas_dump*_tm??_?? ) && $n = adpupa ]];then
# Note: For Jobs rap_dump, ruc2a_dump or ndas_dump (or ndas_dump2) and ADPUPA
#       at center dump times 02-05Z, 08-11Z, 14-17Z, or 20-23Z; all subtypes
#       are expected to OFTEN be missing - the return code of "22" is reduced
#       to "5" here
         [ "$chr" != '00' -a "$chr" != '01' -a \
           "$chr" != '06' -a "$chr" != '07' -a \
           "$chr" != '12' -a "$chr" != '13' -a \
           "$chr" != '18' -a "$chr" != '19' ]  &&  errn=5

      elif [[ $job = *sruc_dump*_??05 && $n = proflr ]];then
# Note: For Job sruc_dump at T+0:05 (only) at all center dump times, PROFLR
#       (all subtypes) is expected to ALWAYS be missing - the return code of
#       "22" is reduced to "5" here
#  Note: This is no longer dumped, but we'll keep the check in here for now
         errn=5

      elif [[ $job = *gfs_dump*_?? && $n = sfcbog ]];then
# Note: For Job gfs_dump (or gfs_dump2) at all center dump times, SFCBOG (all
#       subtypes) is expected to ALWAYS be missing - the return code of "22" is
#       reduced to "5" here
         errn=5

      elif [[ ( $job = *rtma_dump*_?? || $job = *rap_dump*_?? || \
                $job = *ruc2a_dump*_?? ) && $n = wndsat ]];then
# Note: For Jobs rtma_dump, rap_dump or ruc2a_dump at all center dump times, 
#    WNDSAT (all subtypes) is expected to ALWAYS be missing - the return 
#    code of "22" is reduced to "5" here
         errn=5

      elif [[ $job = *nam_dump*_?? && $n = airsev ]];then
# Note: For Job nam_dump (or nam_dump2) at center dump times 06 or 18Z, AIRSEV
#       (all subtypes) is expected to OFTEN be missing - the return code of
#       "22" is reduced to "5" here
         [ "$chr" != '00' -a "$chr" != '12' ] && errn=5

      elif [[ $job = *nam_dump*_06 && ( $n = 1bmsu || $n = 1bhrs2 ) ]];then
# Note: For Job nam_dump (or nam_dump2) at all center times except 12Z, MSU and
#       HIRS2 1B data are expected to OFTEN be missing - the return code of
#       "22" is reduced to "5" here
#  Note: This is no longer dumped, but we'll keep the check in here for now
         [ "$chr" != '12' ] && errn=5

      elif [[ ( $job = *rap_dump*_?? || $job = *ruc2a_dump*_?? ) && \
              ( $n = 1bhrs4 || $n = 1bmhs ) ]];then
# Note: For Jobs rap_dump or ruc2a_dump at center times 03, 04 or 05Z, 1BHRS4
#       and 1BMHS data are expected to OFTEN be missing - the return code of
#       "22" is reduced to "5" here
         [ "$chr" = '03' -o "$chr" = '04' -o "$chr" = '05' ] && errn=5

      elif [[ ( $job = *rap_dump*_?? || $job = *ruc2a_dump*_?? ) && \
              $n = 1bhrs3 ]];then
# Note: For Jobs rap_dump or ruc2a_dump at center times 04, 05, 08, 10, 11, 13,
#       17, 18 or 21Z, 1BHRS3 data are expected to OFTEN be missing - the
#       return code of "22" is reduced to "5" here
         [ "$chr" = '04' -o "$chr" = '05' -o "$chr" = '08' -o \
           "$chr" = '10' -o "$chr" = '11' -o "$chr" = '13' -o \
           "$chr" = '17' -o "$chr" = '18' -o "$chr" = '21' ] && errn=5

      elif [[ ( $job = *rap_dump*_?? || $job = *ruc2a_dump*_?? ) && \
              $n = 1bamub ]];then
# Note: For Jobs rap_dump or ruc2a_dump at center times 08 or 13Z, 1BAMUB data
#       are expected to OFTEN be missing - the return code of "22" is reduced
#       to "5" here
         [ "$chr" = '08' -o "$chr" = '13' ] && errn=5

      else

# Note: For SFCBOG at all center dump times other than 00 and 12Z, all subtypes
#       are expected to ALWAYS (SFCBOG) be missing - the return code of "22" is
#       reduced to "5" here (does not include Job gfs_dump, or gfs_dump2, which
#       has already been tested for SFCBOG above)

         if [ "$chr" != '00' -a "$chr" != '12' ];then
            [ "$n" = 'sfcbog' ]  &&  errn=5
         fi
         if [[ $job = *ndas_dump*_tm03_12 || $job = *ndas_dump*_tm06_12 || \
               $job = *ndas_dump*_tm12_18 || $job = *nam_dump*_06 ]];then
            if [ $n = osbuv8 ];then
# Note: For Jobs tm03 or tm06 ndas_dump (or ndas_dump2) at 12Z cycle time or
#       tm12 ndas_dump (or ndas_dump2) at 18Z cycle time or nam_dump (or
#       nam_dump2) at 06Z cycle time, OSBUV8 data (all subtypes) are expected
#       to OFTEN be missing - the return code of "22" is reduced to "5" here
               errn=5
            fi
         fi
      fi
   elif [ "$errn" -eq '11' ];then
      grep  "  HAS        0" $DATA/${n}.out | cut -c1-7 | \
      grep -v -f pattern > list_11
      errgrep=$?
      grep  "  HAS        0" $DATA/${n}.out | cut -c1-7 | \
      grep -f pattern > list_04

# Note: A non-zero "errgrep" mean all missing subtypes in this group are
#       EXPECTED to OFTEN be missing regardless of the center dump time - the
#       return code of "11" is reduced to "4" here (see "pattern" above)

      if [ "$errgrep" -ne '0' ];then
         errn=4
      else

#########if [[ $job = ruc2a_dump*_?? && $n = satwnd ]];then
#XNote: For Job ruc2a_dump and SATWND at all center dump times other than 00,
#xxxxxx 01, 04, 07, 10, 12, 13, 16, 19 and 22Z, one or more subtypes are
#xxxxxx expected to OFTEN be missing - the return code of "11" is reduced to
#xxxxxx "4" here
#xxxxxx (Note: After all is said and done, the only times when a ruc2a dump
#xxxxxx        could return an 11 for GOES visible winds are 00?, 01, 04, 12?,
#xxxxxx        13, 16, 19, 22Z)
#########   [ "$chr" != '00' -a "$chr" != '01' -a "$chr" != '04' -a \
#########     "$chr" != '07' -a "$chr" != '10' -a "$chr" != '12' -a \
#########     "$chr" != '13' -a "$chr" != '16' -a "$chr" != '19' -a \
#########     "$chr" != '22' ] && errn=4
#########else
#xNote: For all jobs dumping ADPUPA at all center dump times other than 00 and
#xxxxxx 12Z
#xxxxxx , and for all jobs EXCEPT job ruc2a_dump dumping SATWND at center
#xxxxxx dump times other than 00, 03, 06, 09, 12, 15, 18, and 21Z;
#xxxxxx one or more
#xxxxxx subtypes are expected to OFTEN
#xxxxxx (ADPUPA) or ALWAYS (SATWND)
#xxxxxx be missing - the return code of "11" is reduced to "4" here
#xxxxxx {Note 1: At 03, 09, 15, and 21Z only NDAS jobs dump SATWNDs here and
#xxxxxx          these include only GOES and EUMETSAT SATWNDs (no other
#xxxxxx          foreign winds)}
#xxxxxx (Note 2: Missing GOES visible SATWNDS at 06 and 09Z due to nighttime
#xxxxxx          have already been accounted for by this script)

#########   if [ "$chr" != '00' -a "$chr" != '12' ];then
#########      [ "$n" = 'adpupa' ]  &&  errn=4
#########      if [ "$chr" != '03' -a "$chr" != '06' -a "$chr" != '09' -a \
#########           "$chr" != '15' -a "$chr" != '18' -a "$chr" != '21' ];then
#########         [ "$n" = 'satwnd' ]  &&  errn=4
#########      fi
#########   fi
#########fi
         [ "$errn" -eq '4' ]  &&  cat list_11 >> list_04
      fi
   else
msg="**WARNING: $n dump RETURN CODE NOT RECOGNIZED, status unknown (RC=$errn)"
   fi

   [ "$errt" -lt "$errn" ]  &&  errt=$errn

   if [ "$errn" -eq '5' ];then
msg="$n dump EMPTY, all subtypes missing as expected at this time (RC=$errn)"
msg45="[For $n, original R.C. 22 reduced to 5, all subtypes expected to be
 missing at this time]"
   fi

   if [ "$errn" -eq '4' ];then
msg45="[For $n, original R.C. 11 reduced to 4, missing data of these subtypes
 are expected at this time]"
      nindx=`wc list_04  | awk '{ print $1 }'`
      mindx=0
      until [ "$mindx" -eq "$nindx" ]
      do
         mindx=`expr $mindx + 1`
         subt=`head -n${mindx} list_04 | tail -n1`
         msg="$n dump SUCCESSFUL, subtype $subt missing as expected at this \
time (RC=$errn)"
         set +u
         [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
         set -u
      done
      msg="$n dump SUCCESSFUL, some subtypes missing as expected at this time \
(RC=$errn)"
   elif [ "$errn" -eq '11' ];then
      nindx=`wc list_11  | awk '{ print $1 }'`
      mindx=0
      until [ "$mindx" -eq "$nindx" ]
      do
         mindx=`expr $mindx + 1`
         subt=`head -n${mindx} list_11 | tail -n1`
         msg="**WARNING: $n dump INCOMPLETE, subtype $subt missing (RC=$errn)"
         set +u
         [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
         set -u
      done
      nindx=`wc list_04  | awk '{ print $1 }'`
      mindx=0
      until [ "$mindx" -eq "$nindx" ]
      do
         mindx=`expr $mindx + 1`
         subt=`head -n${mindx} list_04 | tail -n1`
         msg="... even though $n dump incomplete, subtype $subt missing as \
expected at this time"
         set +u
         [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
         set -u
      done
      msg="**WARNING: $n dump INCOMPLETE, 1 or more subtypes missing (RC=$errn)"
   else
      set +u
      [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
      set -u
   fi
   echo "$msg" >> outout
   echo "$msg" >> status1.out
   if [ "$errn" -eq '4' -o "$errn" -eq '5' ];then
      echo "$msg45" >> outout
      echo "$msg45" >> status1.out
   fi
   echo " " >> outout
   echo " " >> status1.out
set +x
echo
echo "The foreground exit status for dumpjb, $n only, is " $errn
echo
set -x
done

if [ "$errdmp" -eq '11' -a "$errt" -eq '4' ];then
   msg1="==> The final overall return code for this run of dumpjb is 4, \
reduced from
     original value of 11 for reasons noted just above"
elif [ "$errdmp" -eq '22' -a "$errt" -eq '5' ];then
   msg1="==> The final overall return code for this run of dumpjb is 5, \
reduced from
     original value of 22 for reasons noted just above"
else
   msg1="==> The final overall return code for this run of dumpjb remains $errt"
fi
echo "$msg1" >> outout
echo "     " >> outout
echo "$msg1" >> status1.out
echo "     " >> status1.out

errdmp=$errt
rm pattern

#.......................................................................

if [ "$LOUD" != 'off' ];then
   set +x
   echo
echo "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
echo "------------------------------------------------------------------------"
echo "      LOUD != off - below is script trace for this run of dumpjb"
echo "------------------------------------------------------------------------"
   echo
   cat errfile
   echo
echo "------------------------------------------------------------------------"
echo "             end of script trace for this run of dumpjb"
echo "------------------------------------------------------------------------"
echo "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
   echo
   set -x
else
   cat errfile >> outout
   cat errfile >> status1.out
fi
rm errfile

set +x
echo
echo "---------------------------------------------------"
echo "************  COMPLETED SCRIPT dumpjb  ************"
echo "**  foreground exit status for ALL GROUPS is $errdmp  **"
echo "---------------------------------------------------"
echo
set -x

#fi for $1!=null test
fi


#  Move output files to proper naming structure
#  --------------------------------------------

errmv=0

pgmout_this=$DATA/job${DUMP_NUMBER}/allout

#============================================
SENDCOM=${SENDCOM:-YES}
[ "$SENDCOM" = 'NO' ]  &&  COMSP=$DATA/
pgmout=${pgmout:-$DATA/allout}
prepssmi=${prepssmi:-NO}
prepersd=${prepersd:-NO}
prepqksd=${prepqksd:-YES}
prepascd=${prepascd:-YES}
preptrmm=${preptrmm:-YES}
prepwindsat=${prepwindsat:-YES}
CHGRP_RSTPROD=${CHGRP_RSTPROD:-YES}
set +x
echo
echo "SENDCOM = " $SENDCOM
echo
echo "pgmout = " $pgmout
echo
echo "pgmout_this = " $pgmout_this
echo
echo "prepssmi = " $prepssmi
echo
echo "prepersd = " $prepersd
echo
echo "prepqksd = " $prepqksd
echo
echo "prepascd = " $prepascd
echo
echo "preptrmm = " $preptrmm
echo
echo "prepwindsat = " $prepwindsat
echo
echo "CHGRP_RSTPROD = " $CHGRP_RSTPROD
echo
set -x
#============================================

for n
do
   [ "$1" = 'null' ]  &&  break
     
   cat $DATA/${n}.out >> outout
   rm  $DATA/${n}.out
   if [ -s $DATA/${n}.${FORM} ];then
######cp  $DATA/${n}.${FORM} ${COMSP}${n}.${tmmark}.bufr_d
      #mv  $DATA/${n}.${FORM} ${COMSP}${n}.${tmmark}.bufr_d
      cpfs $DATA/${n}.${FORM} ${COMSP}${n}.${tmmark}.bufr_d #NCO bugzilla Diane Stokes
      errmvt=$?
######rm  $DATA/${n}.${FORM}
      errmvl=$errmv
      errmv=`expr $errmvl + $errmvt`
   else
      [ "$SENDCOM" != 'NO' ] && cp /dev/null ${COMSP}${n}.${tmmark}.bufr_d
   fi

   if [ "$CHGRP_RSTPROD" = 'YES' ]; then
      if [ $n = adpsfc -o $n = aircar -o $n = aircft -o $n = msonet -o \
           $n = sfcshp -o $n = lghtng -o $n = gpsipw -o $n = saphir -o \
           $n = gpsro ]; then
         chgrp rstprod ${COMSP}${n}.${tmmark}.bufr_d
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 ${COMSP}${n}.${tmmark}.bufr_d
            msg="NOTE: $n dump contains RESTRICTED data, only users in \
rstprod group have read permission"
            set +u
            [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
            set -u
            echo "$msg" >> outout
            echo "$msg" >> status1.out
            echo " " >> outout
            echo " " >> status1.out
         else
            cp /dev/null ${COMSP}${n}.${tmmark}.bufr_d
            msg="**WARNING: $n dump contains RESTRICTED data, since user \
$USER is not in rstprod group a null file is copied in its place"
            set +u
            [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
            set -u
            echo "$msg" >> outout
            echo "$msg" >> status1.out
            echo " " >> outout
            echo " " >> status1.out
         fi
      fi
   fi

done

if [ -f outout ];then
   cat outout >> dumpjb.out
   cat outout >> $pgmout_this
   rm outout
fi


#############################################################################
#############################################################################
#           EXECUTE THE PREPOBS_PREPSSMI PROGRAM (IF REQUESTED)
#############################################################################
#############################################################################

PREPSSMI_PROD_TYPE=${PREPSSMI_PROD_TYPE:-GOODBERLET}

kindx=1
ssmip_flag=0
ssmipn_flag=0
ssmit_flag=0
[ "$PREPSSMI_PROD_TYPE" = 'COMBINATION' ]  &&  ssmit_flag=1

> ssmi_count

until [ $kindx -eq 3 ]
do

kindx=`expr $kindx + 1`

ssmif=ssmip

kflag=0

for nnn
do
   if [ "$nnn" = 'ssmit' -o "$nnn" = 'ssmip' -o "$nnn" = 'ssmipn' ];then

      eval iflag=\$${nnn}_flag
      [ "$iflag" -eq '1' ]  &&  continue

      if [ "$kindx" -eq '2' ];then

#  set-up variables and file paths to run prepobs_prepssmi
#  -------------------------------------------------------

#============================================

         EXECbufr=${EXECbufr:-$HOMEbufr_dump/exec}
         EXECPREP=${EXECPREP:-$HOMEbufr_dump/exec}
         PARMPREP=${PARMPREP:-$HOMEobsproc/parm}
         FIXPREP=${FIXPREP:-$HOMEbufr_dump/fix}
         FIXbufr=${FIXbufr:-$HOMEbufr_dump/fix}
         USHPMI=${USHPMI:-$HOMEbufr_dump/ush}

         PMIT=${PMIT:-$FIXPREP/prepobs_prepssmi.bufrtable}
         PMIX=${PMIX:-$EXECPREP/prepobs_prepssmi}
         PMIC=${PMIC:-$PARMPREP/prepobs_prepssmi.${NET}.parm}
         [ -s $PMIC ]  ||  break 2
         pmic=$PMIC

         LANDC=${LANDC:-$FIXbufr/nesdis.lstags.prepssmi}

         set +x
         echo
         echo "EXECPREP = " $EXECPREP
         echo
         echo "PARMPREP = " $PARMPREP
         echo
         echo "FIXbufr  = " $FIXbufr
         echo
         echo "FIXPREP = " $FIXPREP
         echo
         echo "PREPSSMI_PROD_TYPE = " $PREPSSMI_PROD_TYPE
         echo
         set -x

      fi

      if [ "$PREPSSMI_PROD_TYPE" = 'COMBINATION' ];then
         grep ${nnn}: $PMIC | awk -F: '{print $2}' > insert
         mcount=`grep -n IVAR $PMIC | grep = | cut -f1 -d: | head -n 1`
         mcount=`expr $mcount - 1`
         mtotal=`cat <$PMIC | wc -l`
         head -n $mcount $PMIC > top_part
         mcount=`expr $mtotal - $mcount - 1`
         tail -n $mcount $PMIC > bottom_part
         cat top_part insert bottom_part > prepobs_prepssmi.cards.$nnn
         pmic=$DATA/job${DUMP_NUMBER}/prepobs_prepssmi.cards.$nnn
         rm top_part bottom_part insert
         eval ${nnn}_flag=1
      else
         kindx=3
      fi

# Grep out value for IALG switch in the data cards, as this helps determine
#  which BUFR SSM/I data dump file is to be input to prepobs_prepssmi
# -------------------------------------------------------------------------

      IALG=`grep IALG $pmic | awk -F, \
       '{print $1; print $2; print $3; print $4; print$5}' | grep IALG | \
       awk -F= '{print $2}'`

      if [ "$IALG" -gt '0' ];then
         ssmif=ssmit
      else
         if [ "$PREPSSMI_PROD_TYPE" = 'NEURAL_NET3' ];then
            ssmif=ssmipn
         elif [ "$PREPSSMI_PROD_TYPE" = 'COMBINATION' ];then
            ssmif=$nnn
         else
            ssmif=ssmip
         fi
      fi

      set +x
      echo
      echo "ssmif = " $ssmif
      echo
      set -x

      kflag=1

      break
#============================================
   fi
done

[ "$kflag" -eq '0' ]  &&  kindx=3

cp /dev/null sstgrb
cp /dev/null sstgrb.index

errsmi=0
msg1=NO

for nnn
do
   [ "$nnn" = $ssmif -a "$SENDCOM" != 'NO' ]  &&  \
    cp /dev/null ${COMSP}spssmi.${tmmark}.bufr_d
   if [ "$nnn" = $ssmif -a "$prepssmi" = 'YES' ];then

      set +x
      echo "****************************************************************\
***************" >> status1.out
      msg="PROGRAM prepobs_prepssmi will run from input $ssmif dump"
      echo "    "
      echo "##################################################################"
      echo "$msg"
      echo "##################################################################"
      echo "    "
      for OUT in prepssmi.out $pgmout_this status1.out
      do
         echo "    " >> $OUT
         [ "$OUT" != 'status1.out' ] && echo "#############################\
#####################################" >> $OUT
         echo "$msg" >> $OUT
         [ "$OUT" != 'status1.out' ] && echo "#############################\
#####################################" >> $OUT
         [ "$OUT" != 'status1.out' ] && echo "    " >> $OUT
      done
      set -x
      set +u
      [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
      set -u

      if [ -s ${COMSP}$ssmif.${tmmark}.bufr_d ];then

#  more variables and file paths to run prepobs_prepssmi
#  -----------------------------------------------------

         cd $DATA/job${DUMP_NUMBER}

         cp ${COMSP}$ssmif.${tmmark}.bufr_d ssmif_in
         SSMI_IN=ssmif_in
         ssmiOU=${COMSP}spssmi.${tmmark}.bufr_d

         if [ "$PREPSSMI_PROD_TYPE" = 'COMBINATION' ];then
            ssmiOU_save=$ssmiOU
            ssmiOU=$ssmiOU_save.$ssmif
            cp /dev/null $ssmiOU
         fi

         DATA_save=$DATA
         DATA=$DATA/job${DUMP_NUMBER}
         pgmout_save=$pgmout
         pgmout=$pgmout_this
         PMIC_save=$PMIC
         PMIC=$pmic

# !!! the prepobs_prepssmi.sh script was never transitioned to vert structure !!!
# !!! it doesn't exist and won't be found on /nwprod*/ !!!
# !!! This logic needs to be reviewed and corrected. !!!
         time -p $USHPMI/prepobs_prepssmi.sh $cendat
         errsmi=$?

         if [ "$errsmi" -eq 0 -a -s grep.count.ssmi ];then
            echo "    " >> status1.out
            if [ "$PREPSSMI_PROD_TYPE" = 'COMBINATION' ];then
               cut -c46- grep.count.ssmi | tr 'A-Z' 'a-z' | tr -d '<<<' \
                >>status1.out
            else
               cut -c41- grep.count.ssmi | tr -d '<<<' >> status1.out
            fi
            rm grep.count.ssmi
         fi

         DATA=$DATA_save
         pgmout=$pgmout_save
         PMIC=$PMIC_save

         [ -s msg1_file ]  && msg1="`cat msg1_file` from input $ssmif dump"
         [ -s msg2_file ]  && msg2="`cat msg2_file` from input $ssmif dump"

         cp ssmi.bufr  $ssmiOU
         errmvt=$?
         errmvl=$errmv
         errmv=`expr $errmvl + $errmvt`

         rm ssmif_in ssmi.bufr

      else

         msg2="**PROGRAM  PREPOBS_PREPSSMI  NOT RUN BECAUSE $ssmif dump \
file not found"

      fi

      set +u
      if [ -n "$jlogfile" ];then
         [ "$msg1" != 'NO' ]  &&  $DATA/postmsg "$jlogfile" "$msg1"
         $DATA/postmsg "$jlogfile" "$msg2"
      fi
      set -u

      set +x
      echo
echo "************************************************************************"
      [ "$msg1" != 'NO' ]  &&  echo "$msg1"
      echo "$msg2"
echo "************************************************************************"
      echo
      set -x

      for OUT in prepssmi.out $pgmout_this status1.out
      do
         echo "    " >> $OUT
         [ "$msg1" != 'NO' ]  &&  echo "$msg1" >> $OUT
         if [ "$OUT" = 'status1.out' ];then
            echo $msg2 | grep "generated" 
            errgrep=$?
            if [ "$errgrep" -eq '0' ]; then
               echo "$msg2" | cut -c1-66 >> $OUT
            else
               echo "$msg2" >> $OUT
            fi
         else
            echo "$msg2" >> $OUT
         fi
         echo "    " >> $OUT
      done

echo "**********************************************************************\
*********" >> status1.out
      [ "$errsmi" -gt '50' ]  && exit $errsmi

      break

   fi
done

done

set +u
if [ "$PREPSSMI_PROD_TYPE" = 'COMBINATION' -a -n "$ssmiOU_save" ];then
set -u

#  if reprocessed ssmipn and ssmip dumps exist, run combfr to combine
#   them into reprocessed spssmi file
#  ------------------------------------------------------------------

   if [ -s $ssmiOU_save.ssmipn -a -s $ssmiOU_save.ssmip ];then
      cp $ssmiOU_save.ssmipn bufr1
      cp $ssmiOU_save.ssmip  bufr2
      cat <<EOFd > cards
bufr1
bufr2
EOFd

      pgm=`basename $EXECbufr/bufr_combfr`
      if [ -s $DATA/prep_step ];then
         set +u
         . $DATA/prep_step
         set -u
      else
         [ -f errfile ] && rm errfile
         export FORT01=0
         unset `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`
      fi

      FORT50=combfr.spssmi
      time -p $EXECbufr/bufr_combfr <cards > outout 2> errfile
      err=$?
######cat errfile
      cat errfile >> outout
      rm errfile
      cat outout >> combfr.out
      cat outout >> $pgmout_this
      set +x
      echo
      echo "The foreground exit status for BUFR_COMBFR to make combined \
spssmi file is " $err
      echo
      set -x
      rm bufr1 bufr2
      if [ "$err" -gt '0' ];then
         msg="**FATAL ERROR PROGRAM  BUFR_COMBFR (making combined spssmi \
file) RETURN CODE $err"
      else
         set +x
         echo
   echo "--------------------------------------------------------------------"
   echo "**** COMPLETED PROGRAM bufr_combfr to make combined spssmi file ****"
   echo "--------------------------------------------------------------------"
         echo
         set -x
         msg="program BUFR_COMBFR completed normally - combined spssmi file \
generated"
      fi
      rm outout
      set +u
      [ -n "$jlogfile" ]  &&  $DATA/postmsg "$jlogfile" "$msg"
      set -u
      echo "    " >> $pgmout_this
      echo "$msg" >> $pgmout_this
      echo "    " >> $pgmout_this
      [ "$err" -gt '0' ]  &&  exit 99
   elif [ -s $ssmiOU_save.ssmipn ];then
      cp $ssmiOU_save.ssmipn combfr.spssmi
      err=$?
   elif [ -s $ssmiOU_save.ssmip ];then
      cp $ssmiOU_save.ssmip combfr.spssmi
      err=$?
   else
      cp /dev/null combfr.spssmi
      err=$?
   fi
   rm $ssmiOU_save.ssmipn $ssmiOU_save.ssmip
   if [ $err -eq 0 ];then
      cp combfr.spssmi $ssmiOU_save
      if [ -s ssmi_count ];then
         n1=`head -n1 ssmi_count`
         n2=`tail -n1 ssmi_count`
         nt=`expr $n1 + $n2`
         rm ssmi_count
      else
         nt="?????"
      fi
cat << EOF_C >> status1.out

*******************************************************************************

spssmi dump file generated by combining reprocessed ssmipn & ssmip dump files

>>>  COMBINED TOTAL FOR ALL DATA TYPES ...      $nt

reprocessed ssmipn and ssmip dump files removed

*******************************************************************************

EOF_C
   fi
   ssmiOU=$ssmiOU_save
fi

[ ! -s ${COMSP}spssmi.${tmmark}.bufr_d -a -f ${COMSP}spssmi.${tmmark}.bufr_d \
 -a $prepssmi != YES ]  &&  rm ${COMSP}spssmi.${tmmark}.bufr_d
#############################################################################
#############################################################################
#             EXECUTE ERS SCATTEROMETER REPROCESSING (IF REQUESTED)
#############################################################################
#############################################################################

for nnn
do

   if [ "$nnn" = 'erswnd' ];then

      msg1=NO
      msg2="**NO ERS SCATTEROMETER DATA reprocessed due to above PGM FAIL \
--> non-fatal"
      [ "$SENDCOM" != 'NO' ] && cp /dev/null ${COMSP}erscat.${tmmark}.bufr_d

      if [ "$prepersd" = 'YES' ];then

echo "*******************************************************************\
************" >> status1.out

         if [ -s ${COMSP}erswnd.${tmmark}.bufr_d ];then

#  set-up variables and file paths
#  -------------------------------

            cd $DATA/job${DUMP_NUMBER}

            cp ${COMSP}erswnd.${tmmark}.bufr_d erswnd_bufr
            ersIN=erswnd_bufr
            ersOU=${COMSP}erscat.${tmmark}.bufr_d

#============================================
            EXECWAVE=${EXECWAVE:-$HOMEbufr_dump/exec}
            FIXWAVE=${FIXWAVE:-$HOMEbufr_dump/fix}

# !!! These scatterometer wind reprocessing executables were never transitioned !!!
# !!! to vertical structure;  they do not exist and won't be found on /nwprod*/ !!!
# !!! This block of logic needs to be reviewed and corrected. !!!
            DCLX=${DCLX:-$EXECWAVE/wave_dcodcloc}
            DQCX=${DQCX:-$EXECWAVE/wave_dataqc}
            DSRT=${DSRT:-$FIXWAVE/wave_bufrtab.erscat}
            DSRX=${DSRX:-$EXECWAVE/wave_datasort}

            set +x
            echo
            echo "EXECWAVE = " $EXECWAVE
            echo
            echo "FIXWAVE  = " $FIXWAVE
            echo
            set -x

#============================================


#  get the "best" pressure grib file valid at cycle time and create
#   grib index file here to ensure that it's from the same network
#   (10 meter temperature and wind fields are needed)
#  ----------------------------------------------------------------

            set +x
            echo
echo "VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV"
echo "             Get pressure grib file valid at center dump time"
echo "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
            echo
            set -x
# DAK: 9/3/14: The below will no longer work after the next GFS upgrade due to
#      planned changes to getges.sh and the naming conventions used for pressure grib
#      files. Will comment out and print a diagnostic. (This logic is no longer
#      executed in production since there are no ERS data sources.)
######      /nwprod/util/ush/getges.sh -t pgbcur -v $cendat pgrb
######      err1=$?
            set +x
            echo
            echo "===> Cannot obtain pressure grib file valid at center dump \
time due to planned changes to getges.sh and the naming conventions used"
            echo "     for pressure grib files in fall 2014."
            echo
            set -x
            err1=99
            set +x
            echo
echo "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
            echo
            set -x
            err2=99
            if [ "$err1" -eq '0' ];then
               rm errfile
               $GRBINDEX pgrb pgrb.index 2>errfile
               err2=$?
               [ "$err2" -ne '0' ] && cat errfile
               rm errfile
            fi

#  if not already available, get the most recent daily 1 deg. X 1 deg.
#   GRIB sst data set (unblocked)  - create grib index file here to ensure
#   that it's from the same network and time
#  (NOTE: sstgrb available at 00, 06, 12, and 18Z from GDAS, but
#         the index file only available at 00Z each day)
#  -----------------------------------------------------------------------

            if [ -s sstgrb -a -s sstgrb.index ];then
               err3=0
               err4=0
            else

               set +x
               echo
echo "VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV"
echo "                 Get sst grib file valid at center dump time"
echo "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
               echo
               set -x
# DAK: 4/25/16: The below path to getges.sh will no longer be valid at some
#      point soon with the conversion of utility scripts to versioned vertical
#      structure and the migration to Phase 2 on WCOSS.  The path will likely
#      be obtained from the default or specified version of module prod_util.
#      Will comment out and print a diagnostic.  (This logic is no longer
#      executed in production since there are no ERS data sources.)
######         /nwprod/util/ush/getges.sh -t sstgrb -v $cendat sstgrb
######         err3=$?
               set +x
               echo
               echo "===> Cannot obtain sst grib file valid at center dump \
time due to conversion of utility scripts to versioned vertical structure"
               echo "     and the migration to Phase 2 on WCOSS."
               echo
               set -x
               err3=99
               set +x
               echo
echo "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
               echo
               set -x
               err4=99
               if [ "$err3" -eq '0' ];then
                  rm errfile
                  $GRBINDEX sstgrb sstgrb.index 2> errfile
                  err4=$?
                  [ "$err4" -ne '0' ] && cat errfile
                  rm errfile
               fi
            fi

            toterr=`expr $err1 + $err2 + $err3 + $err4`

            if [ "$toterr" -eq '0' ];then


#  run dcodcloc (colocation)
#  -------------------------

cat << EOFdat2 > datefile
      $cendat
EOFdat2

               typeset -Z2 dumhr
               dumhr=`expr $icendat % 100`
               echo $dumhr > coloc.parm

               cat datefile
               cat coloc.parm

               export FORT01=0
               unset `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`

               FORT11=datefile
               FORT12=$ersIN
               FORT13=pgrb
               FORT14=pgrb.index
               FORT15=sstgrb
               FORT16=sstgrb.index
               FORT19=$FIXWAVE/wave_zmask1x1
               FORT52=ers2dat.in

               time -p $DCLX <coloc.parm >outout 2>errfile
               errers=$?
               cat errfile >>outout
               rm errfile
               cat outout >> ersproc.out
               cat outout >> $pgmout_this
               rm outout
               set +x
               echo
               echo "The foreground exit status for WAVE_DCODCLOC is " $errers
               echo
               set -x
               if [ "$errers" -ne '0' ];then
msg1="**NON-FATAL ERROR PROGRAM  WAVE_DCODCLOC  RETURN CODE $errers"
               else
                  set +x
                  echo
echo "------------------------------------------------------------"
echo "************  COMPLETED PROGRAM wave_dcodcloc   ************"
echo "------------------------------------------------------------"
                  echo
                  set -x


#  run dataqc (quality control and reprocessing of ERS data)
#  ---------------------------------------------------------

                  export FORT01=0
                  unset `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`

                  FORT11=ers2dat.in
                  FORT51=ers2.94
                  FORT90=datefile
                  FORT94=ers2.9998
                  FORT95=iang8
                  FORT96=err8x

                  time -p $DQCX >outout 2> errfile
                  errers=$?
                  cat errfile >>outout
                  rm errfile
                  cat outout >> ersproc.out
                  cat outout >> $pgmout_this
                  rm outout
                  set +x
                  echo
echo "The foreground exit status for WAVE_DATAQC is " $errers
                  echo
                  set -x
                  if [ "$errers" -ne '0' ];then
msg1="**NON-FATAL ERROR PROGRAM  WAVE_DATAQC  RETURN CODE $errers"
                  else
                     set +x
                     echo
echo "----------------------------------------------------------"
echo "************  COMPLETED PROGRAM wave_dataqc   ************"
echo "----------------------------------------------------------"
                     echo
                     set -x


#  run datasort (time sorting, minimization and ambiguity removal)
#  ---------------------------------------------------------------

                     export FORT01=0
                     unset `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`
                     FORT12=ers2.94
                     FORT20=$DSRT
                     FORT30=$ersIN
                     FORT40=$FIXWAVE/wave_CMOD_DBLUT
                     FORT42=$FIXWAVE/wave_CMOD_QSLUT
                     FORT60=erscat_bufr
                     FORT91=ers2.so
                     FORT92=ers2.in
                     FORT93=ers2.94er

                     time -p $DSRX >outout 2> errfile
                     errers=$?
                     cat errfile >>outout
                     rm errfile
                     cat outout >> ersproc.out
                     cat outout >> $pgmout_this
                     set +x
                     echo
echo "The foreground exit status for WAVE_DATASORT is " $errers
                     echo
                     set -x
                     if [ "$errers" -ne '0' ];then
msg1="**NON-FATAL ERROR PROGRAM  WAVE_DATASORT  RETURN CODE $errers"
                     else
                        grep -e ">>>  TOTAL NUMBER OF REPROCESSED ERS WIND \
REPORTS WRITTEN ..." outout > grep.count.ers
                        errgrep=$?
                        if [ "$errgrep" -eq '0' ];then
                           echo "    " >> status1.out
                           cut -c2- grep.count.ers | tr -d '<<<' >> status1.out
                        fi
                        rm grep.count.ers
                        set +x
                        echo
echo "------------------------------------------------------------"
echo "************  COMPLETED PROGRAM wave_datasort   ************"
echo "------------------------------------------------------------"
                        echo
                        set -x
                        msg1="ERS programs WAVE_DCODCLOC, WAVE_DATAQC and \
WAVE_DATASORT  all completed normally"

########################cp ers2.so ${COMSP}ers2.sort
                        cp erscat_bufr $ersOU
                        errcpy=$?
                        if [ "$errcpy" -eq '0' ];then
msg2="ERS scatterometer data reprocessed successfully into NCEP BUFR file"
                        else
                           msg2="**NO ERS SCATTEROMETER DATA reprocessed \
into NCEP BUFR file; copy to $COMROOT failed --> non-fatal"
                        fi
                     fi
                     rm outout
                     rm erscat_bufr ers2.so ers2.94er ers2.in
                  fi
                  rm ers2.94 ers2.9998 iang8 err8x
               fi
               rm erswnd_bufr datefile ers2dat.in coloc.parm

            else

               msg2="**NO ERS SCATTEROMETER DATA reprocessed; grib files \
not found --> non-fatal"

            fi
            rm pgrb pgrb.index

         else

            msg2="**NO ERS SCATTEROMETER DATA reprocessed; erswnd dump file \
not found --> non-fatal"

         fi

         set +u
         if [ -n "$jlogfile" ];then
            [ "$msg1" != 'NO' ] && $DATA/postmsg "$jlogfile" "$msg1"
            $DATA/postmsg "$jlogfile" "$msg2"
         fi
         set -u

         set +x
         echo
echo "************************************************************************"
         [ "$msg1" != 'NO' ]  &&  echo "$msg1"
         echo "$msg2"
echo "************************************************************************"
         echo
         set -x

         for OUT in ersproc.out $pgmout_this status1.out
         do
            echo "    " >> $OUT
            [ "$msg1" != 'NO' ]  &&  echo "$msg1" >> $OUT
            echo "$msg2" >> $OUT
            echo "    " >> $OUT
         done

echo "*******************************************************************\
************" >> status1.out

      fi

      break
   fi
done

rm sstgrb sstgrb.index

#############################################################################
#############################################################################
#        EXECUTE QUIKSCAT SCATTEROMETER REPROCESSING (IF REQUESTED)
#############################################################################
#############################################################################

for nnn
do

   if [ $nnn = qkscat ];then

      msg1=NO
      msg2="**NO QUIKSCAT SCATTEROMETER DATA reprocessed (from input \
qkscat dump) due to above PGM FAIL --> non-fatal"

      [ "$SENDCOM" != 'NO' ] && cp /dev/null ${COMSP}qkswnd.${tmmark}.bufr_d

      if [ "$prepqksd" = 'YES' ];then

         echo "**************************************************************\
*****************" >> status1.out
         echo " "  >> status1.out
         echo "PROGRAM wave_dcodquikscat will run from input qkscat dump"  \
          >> status1.out

         if [ -s ${COMSP}qkscat.${tmmark}.bufr_d ];then

#  set-up variables and file paths
#  -------------------------------

            cd $DATA/job${DUMP_NUMBER}

            cp ${COMSP}qkscat.${tmmark}.bufr_d qkscat_bufr
            qksIN=qkscat_bufr
            qksOU=${COMSP}qkswnd.${tmmark}.bufr_d

#============================================
            EXECWAVE=${EXECWAVE:-$HOMEbufr_dump/exec}
            FIXWAVE=${FIXWAVE:-$HOMEbufr_dump/fix}
            PARMWAVE=${PARMWAVE:-$HOMEobsproc/parm}

            DQKT=${DQKT:-$FIXWAVE/wave_bufrtab.quikscat}
            DQKX=${DQKX:-$EXECWAVE/wave_dcodquikscat}
            DQKC=${DQKC:-$PARMWAVE/wave_dcodquikscat.${NET}.parm}

            LANDC_DQK=${LANDC_DQK:-$FIXWAVE/wave_landchxh}

            set +x
            echo
            echo "EXECWAVE = " $EXECWAVE
            echo
            echo "FIXWAVE  = " $FIXWAVE
            echo
            echo "PARMWAVE  = " $PARMWAVE
            echo
            set -x

#============================================


#  run dcodquikscat (internal date checking, overland check, wind
#   vector selection, q.c. checks maybe, superobing maybe, reprocess
#   into BUFR file for PREPOBS_PREPDATA program)
#  -----------------------------------------------------------------

            if [ -s $DQKC ]; then
               cp $DQKC dcodquikscat.parm
            else
cat <<EOFd > dcodquikscat.parm
 &RDATA
  ITYPE  = 1,      ! Report type (1 for QUIKSCAT, 2 for ASCAT; here 1)
  IPRINT = 0,      ! Toggle (0=off,1=on) for text listing of all reprocessed
                   !  reports in unit 51
  ISUPOB = 1,      ! Superob the reports (=1, =0 - don't superob reports)
  DELAT  = 0.5,    !  - latitude  spacing (degrees) of superob grid box
  DELON  = 0.5,    !  - longitude spacing (degrees) of superob grid box
  LIMCNT = 2,      !  - minimum number of reports per superob box required to
                   !    make superob
  IQCPOR = 1,      ! Perform probability of rain q.c.
  PORLIM = 0.10,   !  - if IQCPOR = 1, all rpts with p.o.r. .gt. this are tossed
  IQCEDG = 1,      ! Perform orbital swath edge q.c.
  IEDLLM = 8,      !  - if IQCPOR = 1, all rpts with cell # .le. this are tossed
  IEDULM = 64,     !  - if IQCPOR = 1, all rpts with cell # .ge. this are tossed
  IQCWVC = 0,      ! Examine wind vector cell quality flags {CURRENTLY NO (0)
                   !  FOR QUIKSCAT}
  LATS   =-90,     ! Southernmost lat boundary (deg) for accepting rpts (N+; S-)
  LATN   =+90,     ! Northernmost lat boundary (deg) for accepting rpts (N+; S-)
  LONW   =360,     ! Westernmost  lon boundary (deg) for accepting rpts (0-360W)
  LONE   =  0      ! Easternmost  lon boundary (deg) for accepting rpts (0-360W)
 /
EOFd
            fi

            export FORT01=0
            unset `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`

            FORT11=$qksIN
            FORT19=$LANDC_DQK
            FORT20=$DQKT
            FORT51=qkswnd_listing
            FORT52=qkswnd_bufr

            time -p $DQKX >outout < dcodquikscat.parm 2> errfile
            errqks=$?
            cat errfile >>outout
            rm errfile
            cat outout >> dcodquikscat.out
            cat outout >> $pgmout_this
            set +x
            echo
echo "The foreground exit status for WAVE_DCODQUIKSCAT is " $errqks
            echo
            set -x
            if [ "$errqks" -ne '0' ];then
               msg1="**NON-FATAL ERROR PROGRAM  WAVE_DCODQUIKSCAT \
(making qkswnd file) RETURN CODE $errqks"
            else
               grep -e ">>>  TOTAL NUMBER OF REPROCESSED QUIKSCAT WIND \
REPORTS WRITTEN ..." outout > $DATA/grep_quikd.count
               errgrep=$?
               if [ "$errgrep" -eq '0' ];then
                  echo "    " >> status1.out
                  cut -c2- $DATA/grep_quikd.count | tr -d '<<<' >> status1.out
               fi
               rm  $DATA/grep_quikd.count
               set +x
               echo
echo "------------------------------------------------------------"
echo "**********  COMPLETED PROGRAM wave_dcodquikscat   ***********"
echo "------------------------------------------------------------"
               echo
               set -x
               msg1="program WAVE_DCODQUIKSCAT completed normally, qkswnd \
file generated from input qkscat dump"

               cp qkswnd_bufr $qksOU
               errcpy=$?
               msg2=NO
               [ "$errcpy" -ne '0' ] && msg2="**NO QUIKSCAT SCATTEROMETER \
DATA reprocessed into NCEP BUFR file (qkswnd); copy to $COMROOT failed --> \
non-fatal"
            fi
            rm qkscat_bufr qkswnd_bufr outout

         else

            msg2="**NO QUIKSCAT SCATTEROMETER DATA reprocessed; qkscat dump \
file not found --> non-fatal"

         fi

         set +u
         if [ -n "$jlogfile" ];then
            [ "$msg1" != 'NO' ]  &&  $DATA/postmsg "$jlogfile" "$msg1"
            [ "$msg2" != 'NO' ]  &&  $DATA/postmsg "$jlogfile" "$msg2"
         fi
         set -u

         set +x
         echo
echo "************************************************************************"
         [ "$msg1" != 'NO' ]  &&  echo "$msg1"
         [ "$msg2" != 'NO' ]  &&  echo "$msg2"
echo "************************************************************************"
         echo
         set -x

         for OUT in dcodquikscat.out $pgmout_this status1.out
         do
            echo "    " >> $OUT
            if [ "$msg1" != 'NO' ];then
               if [ "$OUT" = 'status1.out' ];then
                  echo $msg1 | grep "generated" 
                  errgrep=$?
                  if [ "$errgrep" -eq '0' ]; then
                     echo "$msg1" | cut -c1-67 >> $OUT
                  else
                     echo "$msg1" >> $OUT
                  fi
               else
                  echo "$msg1" >> $OUT
               fi
            fi
            [ "$msg2" != 'NO' ]  &&  echo "$msg2" >> $OUT
            echo "    " >> $OUT
         done

echo "*******************************************************************\
************" >> status1.out

      fi

      break
   fi
done

#############################################################################
#############################################################################
#          EXECUTE ASCAT SCATTEROMETER REPROCESSING (IF REQUESTED)
#############################################################################
#############################################################################

for nnn
do

   if [ $nnn = ascatt ];then

      msg1=NO
      msg2="**NO ASCAT SCATTEROMETER DATA reprocessed (from input ascatt \
dump) due to above PGM FAIL --> non-fatal"

      [ "$SENDCOM" != 'NO' ] && cp /dev/null ${COMSP}ascatw.${tmmark}.bufr_d

      if [ "$prepascd" = 'YES' ];then

         echo "**************************************************************\
*****************" >> status1.out
         echo " "  >> status1.out
         echo "PROGRAM wave_dcodquikscat will run from input ascatt dump"  \
          >> status1.out

         if [ -s ${COMSP}ascatt.${tmmark}.bufr_d ];then

#  set-up variables and file paths
#  -------------------------------

            cd $DATA/job${DUMP_NUMBER}

            cp ${COMSP}ascatt.${tmmark}.bufr_d ascatt_bufr
            ascIN=ascatt_bufr
            ascOU=${COMSP}ascatw.${tmmark}.bufr_d

#============================================
            EXECWAVE=${EXECWAVE:-$HOMEbufr_dump/exec}
            FIXWAVE=${FIXWAVE:-$HOMEbufr_dump/fix}
            PARMWAVE=${PARMWAVE:-$HOMEobsproc/parm}

            DAST=${DAST:-$FIXWAVE/wave_bufrtab.ascat}
            DASX=${DASX:-$EXECWAVE/wave_dcodquikscat}
            DASC=${DASC:-$PARMWAVE/wave_dcodascat.${NET}.parm}

            LANDC_DAS=${LANDC_DAS:-$FIXWAVE/wave_landchxh}

            set +x
            echo
            echo "EXECWAVE = " $EXECWAVE
            echo
            echo "FIXWAVE  = " $FIXWAVE
            echo
            echo "PARMWAVE  = " $PARMWAVE
            echo
            set -x

#============================================


#  run dcodquikscat (internal date checking, overland check, wind
#   vector selection, q.c. checks maybe, superobing maybe, reprocess
#   into BUFR file for PREPOBS_PREPDATA program)
#  -----------------------------------------------------------------

            if [ -s $DASC ]; then
               cp $DASC dcodascat.parm
            else
cat <<EOFd > dcodascat.parm
 &RDATA
  ITYPE  = 2,      ! Report type (1 for QUIKSCAT, 2 for ASCAT; here 2)
  IPRINT = 1,      ! Toggle (0=off,1=on) for text listing of all reprocessed
                   !  reports in unit 51
  ISUPOB = 0,      ! Superob the reports (=1, =0 - don't superob reports)
  DELAT  = 0.5,    !  - latitude  spacing (degrees) of superob grid box
  DELON  = 0.5,    !  - longitude spacing (degrees) of superob grid box
  LIMCNT = 2,      !  - minimum number of reports per superob box required to
                   !    make superob
  IQCPOR = 0,      ! Perform probability of rain q.c. {CURRENTLY NO (0) FOR
                   !  ASCAT}
  PORLIM = 1.00,   !  - if IQCPOR = 1, all rpts with p.o.r. .gt. this are tossed
  IQCEDG = 0,      ! Perform orbital swath edge q.c. {CURRENTLY NO (0) FOR
                   !  ASCAT}
  IEDLLM = -99999, !  - if IQCPOR = 1, all rpts with cell # .le. this are tossed
  IEDULM =  99999, !  - if IQCPOR = 1, all rpts with cell # .ge. this are tossed
  IQCWVC = 1,      ! Examine wind vector cell quality flags
  LATS   =-90,     ! Southernmost lat boundary (deg) for accepting rpts (N+; S-)
  LATN   =+90,     ! Northernmost lat boundary (deg) for accepting rpts (N+; S-)
  LONW   =360,     ! Westernmost  lon boundary (deg) for accepting rpts (0-360W)
  LONE   =  0      ! Easternmost  lon boundary (deg) for accepting rpts (0-360W)
 /
EOFd
            fi

            export FORT01=0 
            unset `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`

            FORT11=$ascIN
            FORT19=$LANDC_DAS
            FORT20=$DAST
            FORT51=ascatw_listing
            FORT52=ascatw_bufr

            time -p $DASX >outout < dcodascat.parm 2> errfile
            errasc=$?
            cat errfile >>outout
            rm errfile
            cat outout >> dcodascat.out
            cat outout >> $pgmout_this
            set +x
            echo
echo "The foreground exit status for WAVE_DCODQUIKSCAT is " $errasc
            echo
            set -x
            if [ "$errasc" -ne '0' ];then
               msg1="**NON-FATAL ERROR PROGRAM  WAVE_DCODQUIKSCAT \
(making ascatw file) RETURN CODE $errasc"
            else
               grep -e ">>>  TOTAL NUMBER OF REPROCESSED  ASCAT   WIND \
REPORTS WRITTEN ..." outout > $DATA/grep_ascatd.count
               errgrep=$?
               if [ "$errgrep" -eq '0' ];then
                  echo "    " >> status1.out
                  cut -c2- $DATA/grep_ascatd.count | tr -d '<<<' >> status1.out
               fi
               rm  $DATA/grep_ascatd.count
               set +x
               echo
echo "------------------------------------------------------------"
echo "**********  COMPLETED PROGRAM wave_dcodquikscat   ***********"
echo "------------------------------------------------------------"
               echo
               set -x
               msg1="program WAVE_DCODQUIKSCAT completed normally, ascatw \
file generated from input ascatt dump"

               cp ascatw_bufr $ascOU
               errcpy=$?
               msg2=NO
               [ "$errcpy" -ne '0' ] && msg2="**NO ASCAT SCATTEROMETER DATA \
reprocessed into NCEP BUFR file (ascatw); copy to $COMROOT failed --> non-fatal"
            fi
            rm ascatt_bufr ascatw_bufr outout

         else

            msg2="**NO ASCAT SCATTEROMETER DATA reprocessed; ascatt dump file \
not found --> non-fatal"

         fi

         set +u
         if [ -n "$jlogfile" ];then
            [ "$msg1" != 'NO' ]  &&  $DATA/postmsg "$jlogfile" "$msg1"
            [ "$msg2" != 'NO' ]  &&  $DATA/postmsg "$jlogfile" "$msg2"
         fi
         set -u

         set +x
         echo
echo "************************************************************************"
         [ "$msg1" != 'NO' ]  &&  echo "$msg1"
         [ "$msg2" != 'NO' ]  &&  echo "$msg2"
echo "************************************************************************"
         echo
         set -x

         for OUT in dcodascat.out $pgmout_this status1.out
         do
            echo "    " >> $OUT
            if [ "$msg1" != 'NO' ];then
               if [ "$OUT" = 'status1.out' ];then
                  echo $msg1 | grep "generated" 
                  errgrep=$?
                  if [ "$errgrep" -eq '0' ]; then
                     echo "$msg1" | cut -c1-67 >> $OUT
                  else
                     echo "$msg1" >> $OUT
                  fi
               else
                  echo "$msg1" >> $OUT
               fi
            fi
            [ "$msg2" != 'NO' ]  &&  echo "$msg2" >> $OUT
            echo "    " >> $OUT
         done

echo "*******************************************************************\
************" >> status1.out

      fi

      break
   fi
done

#############################################################################
#############################################################################
#              EXECUTE TRMM TMI DATA REPROCESSING (IF REQUESTED)
#############################################################################
#############################################################################

for nnn
do

   if [ "$nnn" = 'trmm' ];then

      msg1=NO
      msg2="**NO TRMM TMI DATA reprocessed (from input trmm dump) due to \
above PGM FAIL --> non-fatal"

      [ "$SENDCOM" != 'NO' ] && cp /dev/null ${COMSP}sptrmm.${tmmark}.bufr_d

      if [ "$preptrmm" = 'YES' ];then

         echo "**************************************************************\
*****************" >> status1.out
         echo " "  >> status1.out
         echo "PROGRAM bufr_supertmi will run from input trmm dump"  >> \
status1.out

         if [ -s ${COMSP}trmm.${tmmark}.bufr_d ];then

#  set-up variables and file paths
#  -------------------------------

            cd $DATA/job${DUMP_NUMBER}

            cp ${COMSP}trmm.${tmmark}.bufr_d trmm_bufr
            tmiIN=trmm_bufr
            tmiOU=${COMSP}sptrmm.${tmmark}.bufr_d

#============================================
            EXECbufr=${EXECbufr:-$HOMEbufr_dump/exec}
            FIXbufr=${FIXbufr:-$HOMEbufr_dump/fix}
            PARMbufr=${PARMbufr:-$HOMEobsproc/parm}

            DTMT=${DTMT:-$FIXbufr/bufr_bufrtab.sptrmm}
            DTMX=${DTMX:-$EXECbufr/bufr_supertmi}
            DTMC=${DTMC:-$PARMbufr/bufr_supertmi.${NET}.parm}

            set +x
            echo
            echo "EXECbufr = " $EXECbufr
            echo
            echo "FIXbufr  = " $FIXbufr
            echo
            echo "PARMbufr  = " $PARMbufr
            echo
            set -x

#============================================


#  run supertmi to superob TRMM TMI data and write reprocessed data
#   into BUFR file for subsequent SSI analysis program)
#  -----------------------------------------------------------------

            if [ -s $DTMC ]; then
               cp $DTMC supertmi.parm
            else
cat <<EOFd > supertmi.parm
 &INPUT
  IPRINT = 0,      ! Toggle (0=off,1=on) for text listing of all reprocessed
                   !  reports in unit 51
  DELAT  = 1.0,    ! Latitude  spacing (degrees) of superob grid box
  DELON  = 1.0,    ! Longitude spacing (degrees) of superob grid box
  LIMCNT = 6,      ! Minimum number of reports per superob box required to make
                   !  superob
  LATS   =-90,     ! Southernmost lat boundary (deg) for accepting rpts (N+; S-)
  LATN   =+90,     ! Northernmost lat boundary (deg) for accepting rpts (N+; S-)
  LONW   =360,     ! Westernmost  lon boundary (deg) for accepting rpts (0-360W)
  LONE   =  0      ! Easternmost  lon boundary (deg) for accepting rpts (0-360W)
 /
EOFd
            fi

            export FORT01=0
            unset `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`

            FORT11=$tmiIN
            FORT20=$DTMT
            FORT51=sptrmm_listing
            FORT52=sptrmm_bufr

            time -p $DTMX >outout < supertmi.parm 2> errfile
            errtmi=$?
            cat errfile >>outout
            rm errfile
            cat outout >> supertmi.out
            cat outout >> $pgmout_this
            set +x
            echo
echo "The foreground exit status for BUFR_SUPERTMI is " $errtmi
            echo
            set -x
            if [ "$errtmi" -ne '0' ];then
               msg1="**NON-FATAL ERROR PROGRAM  BUFR_SUPERTMI (making \
sptrmm file) RETURN CODE $errtmi"
            else
               grep -e ">>>  TOTAL NUMBER OF REPROCESSED (SUPEROBED) \
TRMM TMI REPORTS WRITTEN .." outout > grep.count.sptrmm
               errgrep=$?
               if [ "$errgrep" -eq '0' ];then
                  echo "    " >> status1.out
                  cut -c2- grep.count.sptrmm | tr -d '<<<' >> status1.out
               fi
               rm grep.count.sptrmm
               set +x
               echo
echo "------------------------------------------------------------"
echo "************  COMPLETED PROGRAM bufr_supertmi  *************"
echo "------------------------------------------------------------"
               echo
               set -x
               msg1="program BUFR_SUPERTMI completed normally, sptrmm file \
generated from input trmm dump"

               cp sptrmm_bufr $tmiOU
               errcpy=$?
               msg2=NO
               [ "$errcpy" -ne '0' ] && msg2="**NO TRMM TMI DATA reprocessed \
into NCEP BUFR file (sptrmm); copy to $COMROOT failed --> non-fatal"
            fi
            rm trmm_bufr sptrmm_bufr outout

         else

            msg2="**NO TRMM TMI DATA reprocessed; trmm dump file not found \
--> non-fatal"

         fi

         set +u
         if [ -n "$jlogfile" ];then
            [ "$msg1" != 'NO' ]  &&  $DATA/postmsg "$jlogfile" "$msg1"
            [ "$msg2" != 'NO' ]  &&  $DATA/postmsg "$jlogfile" "$msg2"
         fi
         set -u

         set +x
         echo
echo "************************************************************************"
         [ "$msg1" != 'NO' ]  &&  echo "$msg1"
         [ "$msg2" != 'NO' ]  &&  echo "$msg2"
echo "************************************************************************"
         echo
         set -x

         for OUT in supertmi.out $pgmout_this status1.out
         do
            echo "    " >> $OUT
            if [ "$msg1" != 'NO' ];then
               if [ "$OUT" = 'status1.out' ];then
                  echo $msg1 | grep "generated" 
                  errgrep=$?
                  if [ "$errgrep" -eq '0' ]; then
                     echo "$msg1" | cut -c1-64 >> $OUT
                  else
                     echo "$msg1" >> $OUT
                  fi
               else
                  echo "$msg1" >> $OUT
               fi
            fi
            [ "$msg2" != 'NO' ]  &&  echo "$msg2" >> $OUT
            echo "    " >> $OUT
         done

echo "*******************************************************************\
************" >> status1.out

      fi

      break
   fi
done

#############################################################################
#############################################################################
#        EXECUTE WINDSAT SCATTEROMETER REPROCESSING (IF REQUESTED)
#############################################################################
#############################################################################

for nnn
do

   if [ $nnn = wndsat ];then

      msg1=NO
      msg2="**NO WINDSAT SCATTEROMETER DATA reprocessed (from input wndsat \
dump) due to above PGM FAIL --> non-fatal"

      [ "$SENDCOM" != 'NO' ] && cp /dev/null ${COMSP}wdsatr.${tmmark}.bufr_d

      if [ "$prepwindsat" = 'YES' ];then

         echo "**************************************************************\
*****************" >> status1.out
         echo " "  >> status1.out
         echo "PROGRAM bufr_dcodwindsat will run from input wndsat dump"  \
          >> status1.out

         if [ -s ${COMSP}wndsat.${tmmark}.bufr_d ];then

#  set-up variables and file paths
#  -------------------------------

            cd $DATA/job${DUMP_NUMBER}

            cp ${COMSP}wndsat.${tmmark}.bufr_d wndsat_bufr
            wnsIN=wndsat_bufr
            wnsOU=${COMSP}wdsatr.${tmmark}.bufr_d

#============================================
            EXECbufr=${EXECbufr:-$HOMEbufr_dump/exec}
            FIXbufr=${FIXbufr:-$HOMEbufr_dump/fix}
            PARMbufr=${PARMbufr:-$HOMEobsproc/parm}

            DWST=${DWST:-$FIXbufr/bufr_bufrtab.windsat}
            DWSX=${DWSX:-$EXECbufr/bufr_dcodwindsat}
            DWSC=${DWSC:-$PARMbufr/bufr_dcodwindsat.${NET}.parm}

            LANDC_DWS=${LANDC_DWS:-$FIXbufr/wave_landchxh}

            set +x
            echo
            echo "EXECbufr = " $EXECbufr
            echo
            echo "FIXbufr  = " $FIXbufr
            echo
            echo "PARMbufr  = " $PARMbufr
            echo
            set -x

#============================================


#  run dcodwindsat (internal date checking, overland check, wind
#   vector selection, superobing maybe, reprocess into BUFR file
#   for PREPOBS_PREPDATA program)
#  ----------------------------------------------------------------

            if [ -s $DWSC ]; then
               cp $DWSC dcodwindsat.parm
            else
cat <<EOFd > dcodwindsat.parm
 &RDATA
  IPRINT = 0,      ! Toggle (0=off,1=on) for text listing of all reprocessed
                   !  reports in unit 51
  ISUPOB = 1,      ! Toggle (0=off,1=on) superobing of reports
  DELAT  = 1.0,    !  - latitude  spacing (deg) of superob grid box (ISUPOB=1)
  DELON  = 1.0,    !  - longitude spacing (deg) of superob grid box (ISUPOB=1)
  LIMCNT = 1,      !  - minimum number of reports per superob box required to
                   !     make superob
  LATS   =-90,     ! Southernmost lat boundary (deg) for accepting rpts (N+; S-)
  LATN   =+90,     ! Northernmost lat boundary (deg) for accepting rpts (N+; S-)
  LONW   =360,     ! Westernmost  lon boundary (deg) for accepting rpts (0-360W)
  LONE   =  0      ! Easternmost  lon boundary (deg) for accepting rpts (0-360W)
 /
EOFd
            fi

            export FORT01=0
            unset `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`

            FORT11=$wnsIN
            FORT19=$LANDC_DWS
            FORT20=$DWST
            FORT51=wdsatr_listing
            FORT52=wdsatr_bufr

            time -p $DWSX >outout < dcodwindsat.parm 2> errfile
            errwns=$?
            cat errfile >>outout
            rm errfile
            cat outout >> dcodwindsat.out
            cat outout >> $pgmout_this
            set +x
            echo
echo "The foreground exit status for BUFR_DCODWINDSAT is " $errwns
            echo
            set -x
            if [ "$errwns" -ne '0' ];then
               msg1="**NON-FATAL ERROR PROGRAM  BUFR_DCODWINDSAT (making \
wdsatr file) RETURN CODE $errwns"
            else
               grep -e " >>>  TOTAL NUMBER OF REPROCESSED WINDSAT REPORTS \
WRITTEN .." outout > $DATA/grep_wsatd.count
               errgrep=$?
               if [ "$errgrep" -eq '0' ];then
                  echo "    " >> status1.out
                  cut -c2- $DATA/grep_wsatd.count | tr -d '<<<' >> status1.out
               fi
               rm  $DATA/grep_wsatd.count
               set +x
               echo
echo "------------------------------------------------------------"
echo "**********  COMPLETED PROGRAM bufr_dcodwindsat   ***********"
echo "------------------------------------------------------------"
               echo
               set -x
               msg1="program BUFR_DCODWINDSAT completed normally, wdsatr file \
generated from input wndsat dump"

               cp wdsatr_bufr $wnsOU
               errcpy=$?
               msg2=NO
               [ "$errcpy" -ne '0' ] && msg2="**NO WINDSAT SCATTEROMETER \
DATA reprocessed into NCEP BUFR file (wdsatr); copy to $COMROOT failed --> \
non-fatal"
            fi
            rm wndsat_bufr wdsatr_bufr outout

         else

            msg2="**NO WINDSAT SCATTEROMETER DATA reprocessed; wndsat dump \
file not found --> non-fatal"

         fi

         set +u
         if [ -n "$jlogfile" ];then
            [ "$msg1" != 'NO' ]  &&  $DATA/postmsg "$jlogfile" "$msg1"
            [ "$msg2" != 'NO' ]  &&  $DATA/postmsg "$jlogfile" "$msg2"
         fi
         set -u

         set +x
         echo
echo "************************************************************************"
         [ "$msg1" != 'NO' ]  &&  echo "$msg1"
         [ "$msg2" != 'NO' ]  &&  echo "$msg2"
echo "************************************************************************"
         echo
         set -x

         for OUT in dcodwindsat.out $pgmout_this status1.out
         do
            echo "    " >> $OUT
            if [ "$msg1" != 'NO' ];then
               if [ "$OUT" = 'status1.out' ];then
                  echo $msg1 | grep "generated" 
                  errgrep=$?
                  if [ "$errgrep" -eq '0' ]; then
                     echo "$msg1" | cut -c1-67 >> $OUT
                  else
                     echo "$msg1" >> $OUT
                  fi
               else
                  echo "$msg1" >> $OUT
               fi
            fi
            [ "$msg2" != 'NO' ]  &&  echo "$msg2" >> $OUT
            echo "    " >> $OUT
         done

echo "*******************************************************************\
************" >> status1.out

      fi

      break
   fi
done

if [ "$STATUS" != 'NEVER' -a -s $pgmout_this ];then

#  Generate preliminary report count information (by subtype) which will later
#   be used when the status file is generated
#  ---------------------------------------------------------------------------

##cat <<\EOFblank > blank
##
##
##
##EOFblank
cat <<\EOFblank > blank

EOFblank

   > updated_counts.out
   rm status2.out
   #tr -cd '[:print:]\n\r'  < $pgmout_this > $pgmout_this #IG
   grep --text -e "Dumping [0-2]" -e "Missing [0-2]" $pgmout_this | cut -f2- -d" " \
    > cutLv.allout
   grep --text "^.......  HAS" $pgmout_this | cut -f2- -d" " > cutRv.allout
   paste -d"\0\n" cutLv.allout cutRv.allout > pasteB.allout
   cut -c1-64 pasteB.allout > cutLv.allout
   cut -c65-  pasteB.allout > cutRv.allout
   rm pasteB.allout
   paste -d"\0" cutLv.allout cutRv.allout > paste.part1
   grep --text -e "Domain for [0-2]" $pgmout_this > paste.part2
   paste -d"\n\n" paste.part1 paste.part2 blank > counts.out
   nindx=`cat <counts.out | wc -l`
   mindx=0
   until [ "$mindx" -eq "$nindx" ]
   do
      mindx=`expr $mindx + 1`
      head -n${mindx} counts.out | tail -n1 > temp1
      grep --text -e "in data group" temp1 | grep -v -e "Domain"
      err_grep=$?
      if [ $err_grep -ne 0 ]; then
         cat temp1 >> updated_counts.out
      else
         group=`cut -f5 -d" " temp1`
         if [ -s ${COMSP}${group}.${tmmark}.bufr_d ]; then

#  Currently, the dump status file is constrained to hold dump counts no
#   greater than 9999999 - the logic to this point allows dump counts as large
#   as 999999999 (and some day we may want to allow counts this high in the
#   status file but this will involve changing downstream scripts/codes that
#   read the status file for dump count alerting, etc.) - in the rare event
#   that there are > 9999999 reports dumped, set the listing in the status file
#   to 9999999 and post a diagnostic message to the joblog file

#   ... the below command determines if characters 65 and 66 in the temp1 file
#       are integers - if so, then the dump count exceeds the 9999999 limit!
            echo $(cat temp1 | cut -c65-66) | egrep '^[0-9]+$' >/dev/null 2>&1
            if [ "$?" -eq "0" ]; then
#       ... the dump count exceeds the 9999999 limit! - cut out characters 65
#           and 66 in the temp1 file and set the dump count to 9999999
               cut -c1-64 temp1 > cutLv.allout.temp1
               echo "9999999 REPORTS" > cutRv.allout.temp1
# line below repl. w/ line 2 below; gets wrong field when dump grp mnem < 6 char
#####          dump_count=`cat temp1 | cut -f9 -d" "`
               dump_count=`cat temp1 | awk '{ printf $9 }'`
               dump_mtype=`cat temp1 | cut -c1-3`
               dump_stype=`cat temp1 | cut -c5-7`
               msg="***WARNING: DUMP COUNT FOR TYPE=\
NC${dump_mtype}${dump_stype} (=$dump_count) > 9999999 (LISTER LIMIT), SET \
COUNT TO 9999999"
               [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
               echo $msg >> status2.out
               echo ""   >> status2.out
               paste -d"\0" cutLv.allout.temp1 cutRv.allout.temp1 > temp1
            else
#       ... the dump count does not exceed the 9999999 limit, cut out
#           blank characters 65 and 66 in the temp1 file
               cut -c1-64 temp1 > cutLv.allout.temp1
               cut -c67-  temp1 > cutRv.allout.temp1
               paste -d"\0" cutLv.allout.temp1 cutRv.allout.temp1 > temp1
            fi
            cat temp1 >> updated_counts.out
         else
            sed "s/HAS......... REPORTS/HAS      0 REPORTS/g" temp1 > temp2
            cat temp2 >> updated_counts.out
         fi
      fi
   done

   rm cutLv.allout cutRv.allout  paste.part1 paste.part2 blank \
      temp1 temp2 counts.out cutLv.allout.temp1 cutRv.allout.temp1
fi

#############################################################################
#############################################################################
#               GENERATE THE STATUS FILE (IF REQUESTED)
#############################################################################
#############################################################################

if [ "$STATUS" = 'YES' ];then

   cd $DATA
   [ -f status1.out ] && rm status1.out
   [ -f status2.out ] && rm status2.out
   [ -f updated_counts.out ] && rm updated_counts.out

#  Combine outputs from previous (and possibly simultaneous) runs of this
#   script

   for n in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 \
 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
   do
      if [ ! -d $DATA/job${n} ]; then
         [ $n -lt $DUMP_NUMBER ]  &&  continue
         break
      fi
      [ -f $DATA/job${n}/status1.out ] && cat $DATA/job${n}/status1.out \
       >> status1.out
      [ -f $DATA/job${n}/status2.out ] && cat $DATA/job${n}/status2.out \
       >> status2.out
      [ -f $DATA/job${n}/updated_counts.out ] && cat $DATA/job${n}/updated_counts.out \
       >> updated_counts.out
      [ -f $DATA/job${n}/allout ] && cat $DATA/job${n}/allout >> $pgmout
   done

#  Generate status file to alert that all dumps for this time have been
#   produced - also summarizes substype report counts and data group return
#   code information

WALLCLOK=`date -u`

if [ "$job" = 'j????' ];then
   run="???"
elif [[ $job = *ndas_dump*_tm??_?? ]];then
   run=ndas
elif [[ $job = *rap_dump*_pcyc_?? ]];then
   run=rap_p
elif [[ $job = *rap_dump*_erly_?? ]];then
   run=rap_e
elif [[ $job = *rap_dump*_ehrrr_?? ]];then
   run=rap_eh
elif [[ $job = *ruc2a_dump*_?? ]];then
   run=ruc2a
elif [[ $job = *rtma_ru_dump* ]];then
   run=rtma_ru
else
   run=$NET
fi

if [ "$run" != '???' ]; then
   run_uc=$(echo $run | tr [a-z] [A-Z])
else
   run_uc="???"
fi

tmmark_uc=$(echo $tmmark | tr [a-z] [A-Z])
cycle_uc=$(echo $cycle | tr [a-z] [A-Z] | cut -c2-)

cat <<EOFs1 > status.out
                           
  #### INFORMATION REGARDING ${tmmark_uc} OBSERVATIONAL DATA DUMP FOR ${cycle_uc} ${run_uc} RUN ####
                        PARENT SCRIPT IS: $job

                          CENTRAL DATE IS $cendat
                   
             THIS STATUS FILE CREATED $WALLCLOK

        THIS CREATION TIME MUST BE EARLIER THAN THE START TIME FOR ANY
    OPERATIONAL ANALYSES JOBS THAT READ FROM THESE OBSERVATIONAL DATA DUMP
       FILES (i.e., Jobs NAM_PREP, NDAS_PREP, RAP_PREP, RAP_PREP_PCYC,
       RAP_PREP_ERLY, RAP_PREP_EHRRR, RUC2A_PREP, GFS_PREP, GDAS_PREP)
                   
*******************************************************************************

              ++++ NUMBER OF REPORTS DUMPED BY DATA SUBTYPE ++++ 
                  TIME WINDOW RADIUS IS DATA GROUP DEPENDENT
                  GEOGRAPHICAL DOMAIN IS DATA GROUP DEPENDENT
                   
EOFs1
   cat updated_counts.out >> status.out
   rm updated_counts.out

cat <<\EOFs2 >> status.out


###############################################################################
EOFs2

   #tr -cd '[:print:]\n\r' < $pgmout > $pgmout #IG
   grep --text -q -e " SAT. ID " -e "          %#" $pgmout 
   errgrep=$?
   if [ $errgrep -eq 0 ]; then
cat <<\EOFs3 >> status.out


*******************************************************************************

+++ NUMBER OF REPORTS DUMPED BY SATELLITE ID IN EACH SATELLITE DATA SUBTYPE +++

EOFs3

      grep --text -e " SAT. ID " -e "          %#" $pgmout | sed "s/%#/  /g" \
       >> status.out
   fi

   grep --text -q -e " replicated observations" -e "          #%" $pgmout
   errgrep=$?
   if [ $errgrep -eq 0 ]; then
cat <<\EOFs4 >> status.out


*******************************************************************************

   +++ TOTAL NUMBER OF REPLICATED OBS IN EACH NEXRAD LEVEL 2 DATA SUBTYPE +++

EOFs4

      grep --text -e " replicated observations" -e "          #%" $pgmout | \
       sed "s/#%/  /g" >> status.out
   fi

cat <<\EOFs5 >> status.out


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                 ++++ SPECIFIC INFORMATION ON EACH DATA GROUP DUMP ++++

EOFs5
   cat status1.out >> status.out
   rm status1.out

   if [ -s status2.out ]; then
cat <<\EOFs6 >> status.out


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

             ++++ LISTING OF DUMP COUNTS OVERFLOWING LIMIT OF 9999999 ++++

EOFs6
      cat status2.out >> status.out
      rm status2.out
   fi

   cat status.out > ${COMSP}status${JOB_NUMBER}.${tmmark}.bufr_d
   errmvt=$?
   errmvl=$errmv
   errmv=`expr $errmvl + $errmvt`

fi

if [ "$JOB_NUMBER" -ne 2 -a -s ${COMSP}status${JOB_NUMBER}.${tmmark}.bufr_d -a \
     -f ${COMSP}aircar.${tmmark}.bufr_d ]; then

# Compare AFWA ACARS vs. ARINC ACARS report counts in AIRCAR dump and generate
#  a flag file (*aircar_status_flag*) in $COMSP path indicating which type
#  should be processed from the AIRCAR dump in PREPBUFR; this flag file will
#  later be read by the script prepobs_makeprepbufr.sh - normally there are
#  more ARINC ACARS (primary) and ONLY this type will be processed from AIRCAR
#  dump; on rare occasions the ARINC feed may go down and there are more AFWA
#  ACARS (backup) and then ONLY this type will be processed from AIRCAR dump

   count_004004=`grep -Fe "004.004" \
    ${COMSP}status${JOB_NUMBER}.${tmmark}.bufr_d | \
    awk -F" HAS" '{print$2}' | awk -F" REPORTS" '{print$1}'`
   set +u
   [ -z "$count_004004" ]  &&  count_004004=0
   set -u

   count_004007=`grep -Fe "004.007" \
    ${COMSP}status${JOB_NUMBER}.${tmmark}.bufr_d | \
    awk -F" HAS" '{print$2}' | awk -F" REPORTS" '{print$1}'`
   set +u
   [ -z "$count_004007" ]  &&  count_004007=0
   set -u

   if [ $count_004004 -ge $count_004007 ]; then
      msgp=""
      msg="process only message type 004.004 for AIRCAR data in PREPBUFR"
   else
      msgp="***WARNING: ARINC ACARS dump count ($count_004004) < AFWA ACARS \
dump count ($count_004007)"
      set +u
      [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msgp"
      set -u
      msg="process only message type 004.007 for AIRCAR data in PREPBUFR"
   fi
   echo $msg > ${COMSP}aircar_status_flag.${tmmark}.bufr_d
   set +x
   echo
   echo $msgp
   echo $msg
   echo
   set -x
fi

errtot=`expr $errdmp + $errmv`

if [ "$errtot" -gt 0 ];then
   exit $errtot
elif [ "$errsmi" -gt 0 ];then
   errsmi=`expr $errsmi + 20`
   exit $errsmi
elif [ "$errtot" -eq 0 ];then
   exit
fi
