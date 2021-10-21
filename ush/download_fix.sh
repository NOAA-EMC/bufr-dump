#!/bin/bash

####  UNIX Script Documentation Block
#
# Script name:   download_fix.sh    Script for downloading binary fix files
#
# Author:        R. Mahajan         Org: NCEP/EMC       Date: 2021-10-21
#
#
# Abstract: This script downloads the binary fix files that used to be a part of the
# VLab repository.
# The downloaded files are:
#    nam_expdomain_halfdeg_imask.gbl
#    nam_expdomain_guam_halfdeg_imask.gbl
#    wave_landchxh
#
# These files are currently hosted on the EMC FTP server at:
#    https://ftp.emc.ncep.noaa.gov/EIB/rmahajan/bufr-dump-fix.tgz
#
# A user should not need to execute this script manually, as the data will be
# downloaded and installed as part of the configure, build and install step
# See the CMakeLists.txt file in fix/ directory.
#
# In the case that the data needs to be downloaded manually and into the cloned
# repository, execute this script from the root of the cloned directory
# e.g.
# $> pwd
#     /path/to/bufr-dump
# $> ls -1
#    CMakeLists.txt
#    fix
#    modulefiles
#    sorc
#    ush
# $> ./ush/download_fix.sh
#
# Usage: ./ush/download_fix.sh [yes|no]
#    Script parameters:
#      yes|no - Force download of fix files to overwrite previous download [Default: NO]
#
####

set -eu

# Force option to overwrite fix binary files from a previous download
force=${1:-"NO"}

URL=https://ftp.emc.ncep.noaa.gov/EIB/rmahajan
tarball=bufr-dump-fix.tgz

# fix files to download (contents of the tarball should match)
fix_files=( \
  nam_expdomain_halfdeg_imask.gbl \
  nam_expdomain_guam_halfdeg_imask.gbl \
  wave_landchxh \
)

# Toggle download if any of the fix files are not present
download=false
for file in ${fix_files[@]}; do
  if [[ ! -f fix/$file ]]; then
    echo -e "\nFix file $file not found in cloned repository!"
    download=true
    break
  fi
done

# Force download if desired
if [[ "$force" =~ [yYtY] ]]; then
  echo -e "\nUsing force to fresh download of fix files"
  download=true
fi

if $download; then
  echo -e "\nDownloading $tarball from $URL"
  wget -q $URL/$tarball
  tar xzf $tarball
  rm -f $tarball
else
  echo -e "\nFix files present in the cloned repository. Nothing to download!\n"
fi
