
# Uses EXTS, APPLE and MSYS + options
# Sets MAIN_LIBRARY

# To update, use 
#  find src -name '*'
list(APPEND SRCS
  doremir.c
  doremir/audio_engine.c
  doremir/atomic/queue.c
  doremir/atomic/ring_buffer.c
  doremir/atomic/stack.c
  doremir/device/audio.c
  doremir/buffer.c
  doremir/list.c
  doremir/map.c
  doremir/graph.c
  doremir/midi.c
  doremir/pair.c
  doremir/plot_osx.m
  doremir/priority_queue.c
  doremir/processor.c
  doremir/processor/vm.c
  doremir/processor/id.c
  doremir/processor/const.c
  doremir/processor/unary.c
  doremir/processor/binary.c
  doremir/processor/split.c
  doremir/processor/delay.c
  doremir/processor/seq.c
  doremir/processor/par.c
  doremir/processor/loop.c
  doremir/ratio.c
  doremir/scheduler.c
  doremir/set.c
  doremir/string.c
  doremir/time.c
  doremir/type.c
  doremir/thread/future.c
  doremir/thread/improving.c
  )
list(APPEND SRCS_OSX
  doremir/atomic_osx.c
  doremir/thread_osx.c
  )
list(APPEND SRCS_MSYS
  doremir/atomic_win.c
  doremir/thread_win.c
  )

if(APPLE)
  set(SRCS_ALL ${SRCS} ${SRCS_OSX})
endif()
if(MSYS)
  set(SRCS_ALL ${SRCS} ${SRCS_MSYS})
endif()

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
  if(True)
    add_library(doremir_audio_static ${SRCS_ALL})
    set_target_properties(doremir_audio_static PROPERTIES 
      OUTPUT_NAME                 "doremir_audio"
      RUNTIME_OUTPUT_DIRECTORY    "../lib"
      )
    target_link_libraries(doremir_audio_static ${EXTS})
    set(MAIN_STATIC_LIBRARY doremir_audio_static PARENT_SCOPE)    
  endif(True)
endif(MSYS)
