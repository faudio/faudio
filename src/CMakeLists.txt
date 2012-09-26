
if(APPLE)
  list(APPEND SOURCE_FILES 
    sclaudiox/processor/plugin/au.mm)
else()
  list(APPEND SOURCE_FILES 
    sclaudiox/processor/plugin/au_nil.cc)
endif ()

if(ENABLE_FLUIDSYNTH)
  list(APPEND SOURCE_FILES 
    sclaudiox/processor/synth/fluid.cc)
endif()

list(APPEND SOURCE_FILES 
  sclaudiox/scheduling/future.cc
  sclaudiox/processor.cc
  sclaudiox/processor/plugin/vst2.cc
  sclaudiox/device/audio.cc
  sclaudiox/device/midi.cc
  sclaudiox/device/session.cc
  sclaudiox/stream.cc
  sclaudiox/stream/device/audio.cc
  sclaudiox/stream/device/midi.cc
  sclaudiox/util/immutable_list.cc
  sclaudiox/util/symbol.cc
  sclaudio.cc)


if(APPLE)
  if(BUILD_DYLIB)
    add_library(sclaudio SHARED ${SOURCE_FILES})
    set_target_properties(sclaudio PROPERTIES 
      OUTPUT_NAME                 "sclaudio-${VERSION}"
      LIBRARY_OUTPUT_DIRECTORY    "../lib"
      INSTALL_NAME_DIR            "@executable_path/../lib/sclaudio-${VERSION}.dylib"
      )
    target_link_libraries(sclaudio ${DEPENDENCIES})
    set(MAIN_LIBRARY sclaudio PARENT_SCOPE)
  endif()
  if(BUILD_FRAMEWORK)
    add_library(ScoreCleanerAudio SHARED ${SOURCE_FILES})
    set_target_properties(ScoreCleanerAudio PROPERTIES
      FRAMEWORK                   TRUE
      OUTPUT_NAME                 "ScoreCleanerAudio"
      FRAMEWORK_VERSION           ${VERSION}
      LIBRARY_OUTPUT_DIRECTORY    "../Frameworks"
      INSTALL_NAME_DIR            "@executable_path/../Frameworks/ScoreCleanerAudio.framework/ScoreCleanerAudio"
      COMPILE_FLAGS               "-Wno-deprecated-declarations -Wno-shift-overflow -Wno-parentheses"
      LINK_FLAGS                  "${STD_LINK_FLAGS}"
     )
    target_link_libraries(ScoreCleanerAudio ${DEPENDENCIES})
    set(MAIN_LIBRARY ScoreCleanerAudio PARENT_SCOPE)
  endif()
endif ()

if(WIN32)
  if(BUILD_SHARED)
    add_library(sclaudio SHARED ${SOURCE_FILES})
    set_target_properties(sclaudio PROPERTIES 
      OUTPUT_NAME                 "sclaudio-${VERSION}"
      LIBRARY_OUTPUT_DIRECTORY    "../bin"
      )
    target_link_libraries(sclaudio ${DEPENDENCIES})
    set(MAIN_LIBRARY sclaudio PARENT_SCOPE)    
  endif()
endif()

#add_executable(scl_main "main.cc")
#set_target_properties(scl_main PROPERTIES
#  RUNTIME_OUTPUT_DIRECTORY    "../bin" 
#  COMPILE_FLAGS               "-Wno-deprecated-declarations -Wno-shift-overflow -Wno-parentheses"
#  LINK_FLAGS                  "${STD_LINK_FLAGS}"
# )
#target_link_libraries(scl_main ${MAIN_LIBRARY})
