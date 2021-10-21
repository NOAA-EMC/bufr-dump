# Set URL, sha256sum and tar name of the file to download.
set(URL "https://ftp.emc.ncep.noaa.gov/static_files/public/obsproc")
set(SHA "0c2d467f3f5fef14fc9f57fc6c461f8ebf68716c81c7b3ab9974d268c99cc442")
string(SUBSTRING ${SHA} 0 6 SHORTSHA)
set(TAR "bufr-dump-fix-${SHORTSHA}.tgz") # poor-man's version control

# Download binary fix files and check SHA256
file(DOWNLOAD
  ${URL}/${TAR}
  ${CMAKE_CURRENT_BINARY_DIR}/${TAR}
  INACTIVITY_TIMEOUT 30
  TIMEOUT 90
  SHOW_PROGRESS
  STATUS status
  EXPECTED_HASH SHA256=${SHA}
  )

# Extract downloaded tarball.
file(ARCHIVE_EXTRACT INPUT ${TAR})

# Collect list of fix files to install.
list(APPEND fix_files
  bufr_bufrtab.sptrmm
  bufr_bufrtab.windsat
  bufr_dumplist
  wave_bufrtab.ascat
  wave_bufrtab.erscat
  wave_bufrtab.quikscat
  ${CMAKE_CURRENT_BINARY_DIR}/fix/nam_expdomain_guam_halfdeg_imask.gbl
  ${CMAKE_CURRENT_BINARY_DIR}/fix/nam_expdomain_halfdeg_imask.gbl
  ${CMAKE_CURRENT_BINARY_DIR}/fix/wave_landchxh
  )

# Install.
install(FILES ${fix_files}
        DESTINATION "fix")
