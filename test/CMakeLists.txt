
# Uses EXTS, APPLE and MSYS + options and MAIN_LIBRARY

list(APPEND SRCS_ALL 
  main.c)

if(BUILD_TESTS)
  add_executable(doremir_audio_tests ${SRCS_ALL})
  set_target_properties(doremir_audio_tests PROPERTIES 
    OUTPUT_NAME                 "doremir_audio_tests"
    RUNTIME_OUTPUT_DIRECTORY    "../bin"
    )
  if(APPLE)
    target_link_libraries(doremir_audio_tests ${MAIN_LIBRARY})
  else()
    target_link_libraries(doremir_audio_tests ${MAIN_STATIC_LIBRARY})
  endif()
endif(BUILD_TESTS)