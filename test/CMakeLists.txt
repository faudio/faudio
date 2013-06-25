
# Uses EXTS, APPLE and MSYS + options and MAIN_LIBRARY

if(BUILD_TESTS)

  add_executable(fae_tests ${SRCS_ALL} "main.c")
  
  set_target_properties(fae_tests PROPERTIES 
    OUTPUT_NAME                 "fae_tests"
    RUNTIME_OUTPUT_DIRECTORY    "../bin"
    )
  if(APPLE)
    target_link_libraries(fae_tests ${MAIN_LIBRARY})
  else()
    target_link_libraries(fae_tests ${MAIN_STATIC_LIBRARY})
  endif()

endif(BUILD_TESTS)
