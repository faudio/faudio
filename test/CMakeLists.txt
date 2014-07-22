
# Uses EXTS, APPLE and MSYS + options and MAIN_LIBRARY

# add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/all_tests.c)

if(BUILD_TESTS)
  add_executable(faudio_tests ${SRCS_ALL} main.c)
  
  set_target_properties(faudio_tests PROPERTIES 
    OUTPUT_NAME                 "faudio-tests"
    RUNTIME_OUTPUT_DIRECTORY    "../bin"
    )
  if(APPLE)
    target_link_libraries(faudio_tests ${MAIN_LIBRARY})
  else()
    target_link_libraries(faudio_tests ${MAIN_STATIC_LIBRARY})
  endif()
endif(BUILD_TESTS)
