help([[
Load environment to build bufr-dump on WCOSS2
]])

PrgEnv_intel_ver=os.getenv("PrgEnv_intel_ver") or "default"
intel_ver=os.getenv("intel_ver") or "default"
cmake_ver=os.getenv("cmake_ver") or "default"

load(pathJoin("PrgEnv-intel", PrgEnv_intel_ver))
load(pathJoin("intel", intel_ver))
load(pathJoin("cmake", cmake_ver))

-- Load common modules for this package
load("bufrdump_common")

whatis("Description: bufr-dump build environment")
