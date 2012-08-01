
if ( APPLE )
  list ( APPEND SOURCE_FILES sclaudiox/processor/plugin/au.mm )
endif ()

if ( WIN32 )
  list ( APPEND SOURCE_FILES sclaudiox/processor/plugin/au_nil.cc )
endif ()

list ( APPEND SOURCE_FILES 
  sclaudiox/scheduling/future.cc
  sclaudiox/processor.cc
  sclaudiox/processor/plugin/vst2.cc
  sclaudiox/processor/synth/fluid.cc
  # sclaudiox/processor/synth/fluid_nil.cc
  sclaudiox/device/audio.cc
  sclaudiox/device/midi.cc
  sclaudiox/device/session.cc
  sclaudiox/stream.cc
  sclaudiox/stream/device/audio.cc
  sclaudiox/stream/device/midi.cc
  sclaudiox/util/immutable_list.cc
  sclaudiox/util/symbol.cc
  sclaudio.cc )


if ( APPLE AND BUILD_DYLIB )
  add_library ( sclaudio_dylib SHARED
    ${SOURCE_FILES}
    )
  set_target_properties ( sclaudio_dylib PROPERTIES
    OUTPUT_NAME                 sclaudio-${VERSION}
    LIBRARY_OUTPUT_DIRECTORY    "../lib"
    )
  target_link_libraries ( sclaudio_dylib ${DEPENDENCIES} )
  set(MAIN_LIBRARY sclaudio_dylib PARENT_SCOPE)
endif()

if( APPLE AND BUILD_FRAMEWORK )
  add_library ( ScoreCleanerAudio_framework SHARED
    ${SOURCE_FILES}
    )
  set_target_properties ( ScoreCleanerAudio_framework PROPERTIES
    FRAMEWORK                   TRUE
    OUTPUT_NAME                 ScoreCleanerAudio
    FRAMEWORK_VERSION           ${VERSION}
    LIBRARY_OUTPUT_DIRECTORY    "../Frameworks"
    INSTALL_NAME_DIR            "@executable_path/../Frameworks/ScoreCleanerAudio.framework/ScoreCleanerAudio"
    COMPILE_FLAGS               "-Wno-deprecated-declarations -Wno-shift-overflow -Wno-parentheses"
    )
  target_link_libraries ( ScoreCleanerAudio_framework ${DEPENDENCIES} )
  set(MAIN_LIBRARY ScoreCleanerAudio_framework PARENT_SCOPE)
endif ()

if (WIN32)
  add_library ( sclaudio_dll SHARED
    ${SOURCE_FILES}
    )
  set_target_properties ( sclaudio_dll PROPERTIES
    OUTPUT_NAME                 sclaudio-${VERSION}
    LIBRARY_OUTPUT_DIRECTORY    "../bin"
    ARCHIVE_OUTPUT_DIRECTORY    "../bin"
    RUNTIME_OUTPUT_DIRECTORY    "../bin"
    )
  target_link_libraries ( sclaudio_dll ${DEPENDENCIES} )
  set(MAIN_LIBRARY sclaudio_dll PARENT_SCOPE)
endif()

