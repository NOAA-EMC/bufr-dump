list(APPEND ush_files
  bufr_dump_obs.sh
  check_tanks.sh
  dumpjb
  )
install(PROGRAMS ${ush_files}
        DESTINATION "ush")
