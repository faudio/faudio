
# Uses EXTS, APPLE and MSYS + options and MAIN_LIBRARY

if(BUILD_TESTS)

  add_executable(fa_tests ${SRCS_ALL} "main.c")
  
  set_target_properties(fa_tests PROPERTIES 
    OUTPUT_NAME                 "fa_tests"
    RUNTIME_OUTPUT_DIRECTORY    "../bin"
    )
  if(APPLE)
    target_link_libraries(fa_tests ${MAIN_LIBRARY})
  else()
    target_link_libraries(fa_tests ${MAIN_STATIC_LIBRARY})
  endif()

endif(BUILD_TESTS)
