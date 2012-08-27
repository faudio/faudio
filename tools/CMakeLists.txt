
option(BUILD_SCL_GENERATE_AUDIO_TYPES "" TRUE)

if(BUILD_SCL_GENERATE_AUDIO_TYPES)
  add_executable(scl_generate_audio_types
    "generate_audio_types.cc")
  set_target_properties(scl_generate_audio_types PROPERTIES
    OUTPUT_NAME               "scl_generate_audio_types"
    RUNTIME_OUTPUT_DIRECTORY  "../bin" 
   )
  target_link_libraries(scl_generate_audio_types
    BoostProgramOptions
    ${DEPENDENCIES}
    ${MAIN_LIBRARY}
    )
endif()

