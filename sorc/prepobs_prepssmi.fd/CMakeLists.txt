list(APPEND fortran_src
  prepssmi.f
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ")
endif()

set(exe_name prepobs_prepssmi)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  w3emc::w3emc_4
  bacio::bacio_4
  bufr::bufr_4)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
