
if(BUILD_TESTS)
  set(SCL_TEST_DEFINITIONS ${SCL_DEFINITIONS})
  add_executable(scl_audio_tests "main.cc")
  if(APPLE)
    set_target_properties(scl_audio_tests PROPERTIES
      OUTPUT_NAME               "scl_audio_tests"
      RUNTIME_OUTPUT_DIRECTORY  "../bin" 
     )
  endif ()
  if(WIN32)
    set_target_properties(scl_audio_tests PROPERTIES
      OUTPUT_NAME               "scl_audio_tests"
     )
  endif()
  target_link_libraries(scl_audio_tests
    ${MAIN_LIBRARY}
    ${GTEST})
endif()

add_custom_target(test        
  COMMENT "Run test suite"
  DEPENDS scl_audio_tests
  COMMAND "${CMAKE_CURRENT_BINARY_DIR}/../bin/scl_audio_tests")

