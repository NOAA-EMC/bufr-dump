help([[
Load environment to build bufr-dump on Hera
]])

load("cmake/3.20.1")

prepend_path("MODULEPATH", "/scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack")
load("hpc/1.1.0")
load("hpc-intel/18.0.5.274")

-- Load common modules for this package
load("bufrdump_common")

whatis("Description: bufr-dump build environment")
