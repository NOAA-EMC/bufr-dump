list(APPEND release_notes_files
  Release_Notes.txt
  )
install(PROGRAMS ${release_notes_files}
        DESTINATION "release_notes")
