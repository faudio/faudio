
aux_source_directory(atomic           ATOMIC_SRC)
aux_source_directory(audio_concept    AUDIO_CONCEPT_SRC)
aux_source_directory(audio_processor  AUDIO_PROCESSOR_SRC)
# aux_source_directory(audio_types      AUDIO_TYPES_SRC)
# aux_source_directory(concept          CONCEPT_SRC)
# aux_source_directory(future           FUTURE_SRC)
# aux_source_directory(immutable        IMMUTABLE_SRC)
# aux_source_directory(improving        IMPROVING_SRC)
# aux_source_directory(lockfree         LOCKFREE_SRC)
# aux_source_directory(logging          LOGGING_SRC)
# aux_source_directory(parallel         PARALLEL_SRC)
# aux_source_directory(reference        REFERENCE_SRC)
# aux_source_directory(thread           THREAD_SRC) 
# aux_source_directory(utility          UTILITY_SRC)

set(SRCS
  ${ATOMIC_SRC}
  ${AUDIO_CONCEPT_SRC}
  ${AUDIO_PROCESSOR_SRC}
  ${AUDIO_TYPES_SRC}
  ${CONCEPT_SRC}
  ${FUTURE_SRC}
  ${IMMUTABLE_SRC}
  ${IMPROVING_SRC}
  ${LOCKFREE_SRC}
  ${LOGGING_SRC}
  ${PARALLEL_SRC}
  ${REFERENCE_SRC}
  ${THREAD_SRC}
  ${UTILITY_SRC}
  )

if(BUILD_TESTS)
  add_executable(scl_audio_tests
    ${SRCS}
    "main.cc")
  set_target_properties(scl_audio_tests PROPERTIES
    OUTPUT_NAME               "scl_audio_tests"
    RUNTIME_OUTPUT_DIRECTORY  "../bin" 
   )     
  target_link_libraries(scl_audio_tests
    ${DEPENDENCIES}
    ${GTEST_LIBRARY}
    )
endif()

add_custom_target(test        
  COMMENT "Run test suite"
  DEPENDS scl_audio_tests
  COMMAND "${CMAKE_CURRENT_BINARY_DIR}/../bin/scl_audio_tests")

