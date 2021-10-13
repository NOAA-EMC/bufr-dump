#!/bin/sh
# build.sh
#   compilation wrapper for sorc/*.fd code packages
#
#   Usage:
#      [opt: debug=yes] build.sh [opt: *.fd package list] 
#
#        arguments: source directory list of packages to build
#                   blank  compiles all sorc/*.fd packages
#
#   importable var:  debug 
#      if debug is passed in w/ value 'yes', then DEBUG2 compiler options 
#       are enabled in the makefiles
# 
# modification history
#   1 Aug 2019 - original script
#  29 Aug 2019 - added optional debug importable env variable to enable 
#                  DEBUG2 compile options
#----
set -x
set -e    # fail if an error is hit so that errors do not go unnoticed

lab='build.sh'
usage="[optional: debug=yes] build.sh [optional: *.fd sorc subdir list (blank==all)]" 
[[ "$1" == '-h' ]] && { echo "$lab: USAGE: $usage" ; exit ; }

# check to see if DEBUG2 compile options are requested
debug=${debug:-''}                # default to not using DEBUG2 flags

echo "$lab: welcome to bufr_dump sorc build script ($(date))"
[[ "$debug" == 'yes' ]] && echo "$lab:  -debug options enabled (debug='$debug')"


module reset
module load PrgEnv-intel/8.1.0
module load craype/2.7.8
module load intel/19.1.3.304
module load cray-mpich/8.1.4
source ./load_libs.rc  # use modules to set library related environment variables
echo ; module list
set -x
FC=ftn

if [ $# -eq 0 ]; then
  dir_list=*.fd
else
  dir_list=$*
fi

set +x
echo ; echo "$lab: list of dirs to build..."
echo $dir_list
set -x


clobber=${clobber:-clobber_yes}  # user can override the default of running "make clobber"
#set +x                      # jaw db
for sdir in $dir_list; do

 dir=${sdir%\/}  # chop trailing slash if necessary
#echo "$lab: ------- making ${dir%\.fd*}"

 if [ "$debug" = 'yes' ] ; then 

# set DEBUG2 compiler options
# ---
#  echo "$lab:  -compiler DEBUG2 options are ENABLED"
   if [[ "$sdir" =~ dcodquikscat ]] ; then # extra option in dcodquikscat makefile
     export DEBUG2="-ftrapuv -check all -check nooutput_conversion -fp-stack-check -fstack-protector"
   else 
     export DEBUG2="-ftrapuv -check all -fp-stack-check -fstack-protector"
   fi # sdir = dcodquikscat
#  echo "$lab:  -DEBUG2='$DEBUG2'"
 else
    DEBUG2=""
 fi # debug = yes

 cd $dir
#[ $clobber != clobber_no ]  && { echo "$lab:  -clobbering:" ; make clobber ; }
#echo ; echo "$lab:  -compiling:"
 make FC=$FC 
#echo ; echo "$lab:  -results:"
 ls -l
 cd ..
done

set +x
echo ; echo "$lab: end of script ($(date))"

