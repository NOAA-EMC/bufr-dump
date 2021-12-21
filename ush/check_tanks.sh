
###############################################################################
#
# D. Keyser - NP22
#
# This script identifies dump mnemonics which are not linked to any data
#  presence in the BUFR database tanks over the past 10 days.  In this case,
#  it returns a status code of 1.  It can be called by the dump scripts,
#  alerting them to not attempt to dump a mnemonic whose tank(s) are either 1)
#  no longer in the database, or 2) not yet available in the database.  This
#  prevents empty dump files from being generated day after day {in case 1,
#  until the dump script is modified to remove the attempt to dump the
#  mnemonic in question, in case 2, until the tank(s) associated with the
#  mnemonic in question are populated with data}, and it prevents dump alerts
#  from being generated for the mnemonic in question.
#
#  If at least one database tank associated with the mnemonic is present on at
#  least one day in the last ten (even if it is zero bytes), this script
#  returns with status code zero.  Otherwise, it returns with status code 1.
#  Note that "group" mnemonics, i.e., those associated with more than one
#  database tank file, can also be checked here.  In this case, only one of
#  the database tank files needs to present over one of the past ten days in
#  order to genrrate a status code of zero.
#
#
# Usage:
#      sh check_tanks.sh <dump mnemonic>
#
# Script variables imported:
#  DATA - path to working directory coming into this script
#         (NO DEFAULT - MUST BE PASSED INTO THIS SCRIPT)
#         (working direct. in this script is $DATA/check_tanks_<dump mnemonic>)
#  TANK - path to database tanks up to the YYYYMMDD
#         (default is $DCOMROOT/prod)
#  LIST - complete path to bufr_dumplist fixed file
#         (default is $FIXbufr_dump/bufr_dumplist, formerly
#          $FIXobsproc_shared_bufr_dumplist prior to WCOSS2)
#         (If $LIST not imported, then $FIXbufr_dump MUST
#          BE PASSED INTO THIS SCRIPT)
#  cycle - e.g., t12z
#         (default is t00z)
#
#   Modules and files referenced:
#     executables:  $NDATE (presumably from default or specified version of
#                           module prod_util)
#
# Remarks:
#   Only one dump mnemonic at a time can be processed by this script.
#
########################################################################

set -uax

if [ "$#" -ne '1' ]; then
   set +x
   echo
   echo " ===> Must have 1 script argement"
   echo
   echo "See "Usage" in Docblock for scripts $0 "
   echo
   exit 0 # in this case do not assume all database tank files are missing
fi

name=$1

mkdir -p $DATA/check_tanks_${name}
cd $DATA/check_tanks_${name}
rm *

TANK=${TANK:-${DCOMROOT:?}/prod}

LIST=${LIST:-$FIXbufr_dump/bufr_dumplist}

set +u
if [ -z "$PDY" ]; then
# Run set-pdy if it has not yet been run
   cycle=${cycle:-t00z}
   set +x; echo -e "\n---> path to setpdy.sh below is: `which setpdy.sh`"; set -x
   setpdy.sh
   . $DATA/check_tanks_${name}/PDY
fi
PDYm8=`$NDATE -24 ${PDYm7}00 | cut -c1-8`
PDYm9=`$NDATE -24 ${PDYm8}00 | cut -c1-8`
PDYm10=`$NDATE -24 ${PDYm9}00 | cut -c1-8`

#####env 

TYPE=`grep "_$name " $LIST|cut -c 12-14`

if [ $TYPE = nem ]; then
   > $DATA/check_tanks_${name}/line
   grep "_$name " $LIST > $DATA/check_tanks_${name}/line
   [ -s $DATA/check_tanks_${name}/line ]  &&  \
    MTST="`cat $DATA/check_tanks_${name}/line|cut -c 16-500|cut -f 1 -d#`"
   if [ ! -s $DATA/check_tanks_${name}/line ]; then
      echo
      echo "Group mnemonic $name not found in $LIST"
      echo
      exit 0 # in this case do not assume all database tank files are missing
   fi
fi

for mtst in $MTST
do
   mt=`echo "$mtst"|cut -c 1-3`
   st=`echo "$mtst"|cut -c 4-6`
   tankfile=b${mt}/xx${st}

   for pdy in $PDY $PDYm1 $PDYm2 $PDYm3 $PDYm4 $PDYm5 $PDYm6 $PDYm7 $PDYm8 \
              $PDYm9 $PDYm10; do
      [ -f $TANK/$pdy/$tankfile ]  &&  exit 0 # found a database tank file!
   done
done

msg="***WARNING: no $name dump produced - tanks empty for past 10 days"
set +u
[ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
set -u
set +x
echo ""
echo "$msg"
echo " "
set -x

exit 1 # all database tank files for past 10 days are missing for this mnemonic

