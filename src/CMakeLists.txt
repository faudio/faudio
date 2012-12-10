
# Uses EXTS, APPLE and MSYS + options
# Sets MAIN_LIBRARY

aux_source_directory("doremir"        SRCS)
aux_source_directory("doremir/midi"   SRCS_MIDI)
aux_source_directory("doremir/atomic" SRCS_ATOMIC)
aux_source_directory("doremir/thread" SRCS_THREAD)

set (SRCS_ALL ${SRCS} ${SRCS_MIDI} ${SRCS_ATOMIC} ${SRCS_THREAD})

if(APPLE)
  if(BUILD_SHARED)
    add_library(sclaudio SHARED ${SRCS_ALL})
    set_target_properties(sclaudio PROPERTIES 
      OUTPUT_NAME                 "sclaudio-${VERSION}"
      LIBRARY_OUTPUT_DIRECTORY    "../lib"
      INSTALL_NAME_DIR            "@executable_path/../lib/sclaudio-${VERSION}.dylib"
      )
    target_link_libraries(sclaudio ${EXTS})
    set(MAIN_LIBRARY sclaudio PARENT_SCOPE)
  endif(BUILD_SHARED)
  if(BUILD_FRAMEWORK)
    add_library(ScoreCleanerAudio SHARED ${SRCS_ALL})
    set_target_properties(ScoreCleanerAudio PROPERTIES
      FRAMEWORK                   TRUE
      OUTPUT_NAME                 "ScoreCleanerAudio"
      FRAMEWORK_VERSION           ${VERSION}
      LIBRARY_OUTPUT_DIRECTORY    "../Frameworks"
      INSTALL_NAME_DIR            "@executable_path/../Frameworks/ScoreCleanerAudio.framework/ScoreCleanerAudio"
      LINK_FLAGS                  "${STD_LINK_FLAGS}"
     )
    target_link_libraries(ScoreCleanerAudio ${EXTS})
    set(MAIN_LIBRARY ScoreCleanerAudio PARENT_SCOPE)
  endif(BUILD_FRAMEWORK)
endif(APPLE)

if(MSYS)
  if(BUILD_SHARED)
    add_library(sclaudio SHARED ${SRCS_ALL})
    set_target_properties(sclaudio PROPERTIES 
      OUTPUT_NAME                 "sclaudio"
      RUNTIME_OUTPUT_DIRECTORY    "../bin"
      )
    target_link_libraries(sclaudio ${EXTS})
    set(MAIN_LIBRARY sclaudio PARENT_SCOPE)    
  endif(BUILD_SHARED)
endif(MSYS)
