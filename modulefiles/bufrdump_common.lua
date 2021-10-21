help([[
Load common modules to build bufr-dump on all machines
]])

bacio_ver=os.getenv("bacio_ver") or "default"
w3emc_ver=os.getenv("w3emc_ver") or "default"
sp_ver=os.getenv("sp_ver") or "default"
bufr_ver=os.getenv("bufr_ver") or "default"

load(pathJoin("bacio", bacio_ver))
load(pathJoin("w3emc", w3emc_ver))
load(pathJoin("sp", sp_ver))
load(pathJoin("bufr", bufr_ver))
