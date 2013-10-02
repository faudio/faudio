
# Uses EXTS, APPLE and MSYS + options
# Sets MAIN_LIBRARY

# To update, use 
#  find src -name '*'
list(APPEND SRCS
  fa.c
  core.c
  action.c
  atomic/queue.c
  atomic/stack.c
  audio.c
  midi.c
  dynamic.c
  buffer.c
  error.c
  list.c
  map.c 
  graph.c
  midi/message.c
  pair.c
  pair/left.c
  plot/gnuplot.c
  plot.c
  priority_queue.c
  ratio.c
  set.c
  signal.c
  string.c
  string/trex.c
  string/parson.c
  time.c
  type.c
  )
list(APPEND SRCS_OSX
  ../platform/macosx/fae/atomic.c
  ../platform/macosx/fae/thread.c
  ../platform/macosx/fae/string.c
  ../platform/macosx/fae/device.c
  ../platform/macosx/fae/system/directory.c
  # ../platform/macosx/fae/system/event.c
  )
if(${ENABLE_COREPLOT})
  list(APPEND SRCS_OSX
    ../platform/macosx/fae/plot/coreplot.m
  )
endif()

list(APPEND SRCS_MSYS
  )

if(APPLE)
  set(SRCS_ALL ${SRCS} ${SRCS_OSX})
endif()
if(MSYS)
  set(SRCS_ALL ${SRCS} ${SRCS_MSYS})
endif()

if(APPLE)
  if(BUILD_SHARED)
    add_library(faudio SHARED ${SRCS_ALL})
    set_target_properties(faudio PROPERTIES 
      OUTPUT_NAME                 "faudio"
      LIBRARY_OUTPUT_DIRECTORY    "../lib"
      INSTALL_NAME_DIR            "/usr/local/lib"
      )
    target_link_libraries(faudio ${EXTS})
    set(MAIN_LIBRARY faudio PARENT_SCOPE)
  endif(BUILD_SHARED)
  if(BUILD_FRAMEWORK)
    add_library(faudio_framework SHARED ${SRCS_ALL})
    set_target_properties(faudio_framework PROPERTIES
      FRAMEWORK                   TRUE
      OUTPUT_NAME                 "Faudio"
      FRAMEWORK_VERSION           ${VERSION}
      LIBRARY_OUTPUT_DIRECTORY    "../Frameworks"
      INSTALL_NAME_DIR            "@executable_path/../Frameworks/Faudio.framework/Faudio"
      LINK_FLAGS                  "${STD_LINK_FLAGS}"
     )
    target_link_libraries(faudio_framework ${EXTS})
    set(MAIN_LIBRARY faudio_framework PARENT_SCOPE)
  endif(BUILD_FRAMEWORK)
endif(APPLE)

if(MSYS)
  if(BUILD_SHARED)
    add_library(faudio SHARED ${SRCS_ALL})
    set_target_properties(faudio PROPERTIES 
      OUTPUT_NAME                 "ae"
      RUNTIME_OUTPUT_DIRECTORY    "../bin"
      )
    target_link_libraries(faudio ${EXTS})
    set(MAIN_LIBRARY faudio PARENT_SCOPE)    
  endif(BUILD_SHARED)
  if(True)
    add_library(faudio_static ${SRCS_ALL})
    set_target_properties(faudio_static PROPERTIES 
      OUTPUT_NAME                 "ae"
      RUNTIME_OUTPUT_DIRECTORY    "../lib"
      )
    target_link_libraries(faudio_static ${EXTS})
    set(MAIN_STATIC_LIBRARY faudio_static PARENT_SCOPE)    
  endif(True)
endif(MSYS)
