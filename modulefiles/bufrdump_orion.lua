help([[
Load environment to build bufr-dump on Orion
]])

load("cmake/3.18.1")

prepend_path("MODULEPATH", "/apps/contrib/NCEP/libs/hpc-stack/modulefiles/stack")
load("hpc/1.1.0")
load("hpc-intel/2018.4")

-- Load common modules for this package
load("bufrtranjb_common")

whatis("Description: bufr-dump build environment")
