help([[
Load environment to build bufr-dump on WCOSS2
]])

load(pathJoin("PrgEnv-intel", PrgEnv_intel_ver))
load(pathJoin("intel", intel_ver))
load(pathJoin("cmake", cmake_ver))

-- Load common modules for this package
load("bufrdump_common")

whatis("Description: bufr-dump build environment")
