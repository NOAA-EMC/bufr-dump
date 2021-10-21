#!/bin/bash

set -eux

target=${1:-${target:-"null"}}

readlink=$(which readlink)
[[ $(uname -s) == Darwin ]] && readlink=$(which greadlink)

# Location of PWD and package source directory.
pkg_root=`dirname $(${readlink} -f $0)`

target=$(echo $target | tr [:upper:] [:lower:])
if [[ "$target" =~ ^(wcoss2|hera|orion)$ ]]; then
  source $pkg_root/versions/build.ver
  set +x
  module use $pkg_root/modulefiles
  module load bufrdump_$target
  module list
  set -x
fi

# Determine install path.
INSTALL_PREFIX=${INSTALL_PREFIX:-$pkg_root/install}

# Create a build directory and cd into it.
[[ -d build  ]] && rm -rf build
mkdir -p build && cd build

# build and install.
cmake -DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX ${CMAKE_OPTS:-} ..
make -j ${BUILD_JOBS:-4} VERBOSE=${BUILD_VERBOSE:-}
make install

exit 0
