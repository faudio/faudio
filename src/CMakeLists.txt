
# Uses EXTS, APPLE and MSYS + options
# Sets MAIN_LIBRARY

aux_source_directory("doremir"        SRCS)
aux_source_directory("doremir/midi"   SRCS_MIDI)
aux_source_directory("doremir/atomic" SRCS_ATOMIC)
aux_source_directory("doremir/thread" SRCS_THREAD)

set (SRCS_ALL ${SRCS} ${SRCS_MIDI} ${SRCS_ATOMIC} ${SRCS_THREAD})

if(APPLE)
  if(BUILD_SHARED)
    add_library(doremir_audio SHARED ${SRCS_ALL})
    set_target_properties(doremir_audio PROPERTIES 
      OUTPUT_NAME                 "doremir_audio-${VERSION}"
      LIBRARY_OUTPUT_DIRECTORY    "../lib"
      INSTALL_NAME_DIR            "@executable_path/../lib/doremir_audio-${VERSION}.dylib"
      )
    target_link_libraries(doremir_audio ${EXTS})
    set(MAIN_LIBRARY doremir_audio PARENT_SCOPE)
  endif(BUILD_SHARED)
  if(BUILD_FRAMEWORK)
    add_library(DoReMIRAudio SHARED ${SRCS_ALL})
    set_target_properties(DoReMIRAudio PROPERTIES
      FRAMEWORK                   TRUE
      OUTPUT_NAME                 "DoReMIRAudio"
      FRAMEWORK_VERSION           ${VERSION}
      LIBRARY_OUTPUT_DIRECTORY    "../Frameworks"
      INSTALL_NAME_DIR            "@executable_path/../Frameworks/DoReMIRAudio.framework/DoReMIRAudio"
      LINK_FLAGS                  "${STD_LINK_FLAGS}"
     )
    target_link_libraries(DoReMIRAudio ${EXTS})
    set(MAIN_LIBRARY DoReMIRAudio PARENT_SCOPE)
  endif(BUILD_FRAMEWORK)
endif(APPLE)

if(MSYS)
  if(BUILD_SHARED)
    add_library(doremir_audio SHARED ${SRCS_ALL})
    set_target_properties(doremir_audio PROPERTIES 
      OUTPUT_NAME                 "doremir_audio"
      RUNTIME_OUTPUT_DIRECTORY    "../bin"
      )
    target_link_libraries(doremir_audio ${EXTS})
    set(MAIN_LIBRARY doremir_audio PARENT_SCOPE)    
  endif(BUILD_SHARED)
endif(MSYS)
