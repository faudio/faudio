
# Uses EXTS, APPLE and MSYS + options and MAIN_LIBRARY

list(APPEND SRCS_ALL 
  main.c)

if(BUILD_TESTS)
  add_executable(sclaudio_tests ${SRCS_ALL})
  set_target_properties(sclaudio_tests PROPERTIES 
    OUTPUT_NAME                 "sclaudio_tests"
    RUNTIME_OUTPUT_DIRECTORY    "../bin"
    )
  target_link_libraries(sclaudio_tests ${MAIN_LIBRARY})
endif(BUILD_TESTS)