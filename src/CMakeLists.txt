
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
    LIBRARY_OUTPUT_DIRECTORY    "../"
    )
  target_link_libraries ( sclaudio_dylib ${DEPENDENCIES} )
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
    )
  target_link_libraries ( ScoreCleanerAudio_framework ${DEPENDENCIES} )

  add_custom_command ( TARGET ScoreCleanerAudio_framework POST_BUILD
    COMMAND mkdir -p ../Frameworks
    COMMAND rm -rf ../Frameworks/FluidSynth.framework
    COMMAND cp -R ../../lib/mac/frameworks/FluidSynth.framework ../Frameworks/FluidSynth.framework
    )
endif ()














#
# ##
# ## Build command-line tool
# ##
#
# option(BUILD_SCL_CL "Build the SCL command-line tool" TRUE)
#
# if(${BUILD_SCL_CL})
#   set(SCL_CL_DEFINITIONS ${SCL_DEFINITIONS})
#   message("Compiler definitions (command-line tool): ${SCL_CL_DEFINITIONS}")
#
#   add_executable(SCL_CL "main.cc")
#
#   if(${SCL_OSX})
#     set_target_properties(SCL_CL
#       PROPERTIES
#         OUTPUT_NAME         "SCL"
#         OSX_ARCHITECTURES       i386
#         RUNTIME_OUTPUT_DIRECTORY  "../bin"
#       COMPILE_DEFINITIONS       "${SCL_CL_DEFINITIONS}")
#   endif()
#   if(${SCL_WIN})
#     set_target_properties(SCL_CL
#       PROPERTIES
#         OUTPUT_NAME         "SCL_cl"
#         COMPILE_DEFINITIONS     "${SCL_CL_DEFINITIONS}")
#   endif()
#
#   if (MSVC_IDE)
#     set_target_properties(SCL_CL PROPERTIES PREFIX "../")
#   endif()
#
#   # TODO if building dylib and/or framework on OS X, this only links with framwork
#   target_link_libraries(SCL_CL SCL)
# endif()
