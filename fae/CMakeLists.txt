
# Uses EXTS, APPLE and MSYS + options
# Sets MAIN_LIBRARY

# To update, use 
#  find src -name '*'
list(APPEND SRCS
  core.c
  fae.c
  atomic/queue.c
  atomic/ring_buffer.c
  atomic/stack.c
  device/file.c
  device/buffer.c
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
    add_library(fae SHARED ${SRCS_ALL})
    set_target_properties(fae PROPERTIES 
      OUTPUT_NAME                 "fae"
      LIBRARY_OUTPUT_DIRECTORY    "../lib"
      INSTALL_NAME_DIR            "/usr/local/lib"
      )
    target_link_libraries(fae ${EXTS})
    set(MAIN_LIBRARY fae PARENT_SCOPE)
  endif(BUILD_SHARED)
  if(BUILD_FRAMEWORK)
    add_library(FAE_framework SHARED ${SRCS_ALL})
    set_target_properties(FAE_framework PROPERTIES
      FRAMEWORK                   TRUE
      OUTPUT_NAME                 "FAE"
      FRAMEWORK_VERSION           ${VERSION}
      LIBRARY_OUTPUT_DIRECTORY    "../Frameworks"
      INSTALL_NAME_DIR            "@executable_path/../Frameworks/FAE.framework/FAE"
      LINK_FLAGS                  "${STD_LINK_FLAGS}"
     )
    target_link_libraries(FAE_framework ${EXTS})
    set(MAIN_LIBRARY FAE_framework PARENT_SCOPE)
  endif(BUILD_FRAMEWORK)
endif(APPLE)

if(MSYS)
  if(BUILD_SHARED)
    add_library(fae SHARED ${SRCS_ALL})
    set_target_properties(fae PROPERTIES 
      OUTPUT_NAME                 "ae"
      RUNTIME_OUTPUT_DIRECTORY    "../bin"
      )
    target_link_libraries(fae ${EXTS})
    set(MAIN_LIBRARY fae PARENT_SCOPE)    
  endif(BUILD_SHARED)
  if(True)
    add_library(fae_static ${SRCS_ALL})
    set_target_properties(fae_static PROPERTIES 
      OUTPUT_NAME                 "ae"
      RUNTIME_OUTPUT_DIRECTORY    "../lib"
      )
    target_link_libraries(fae_static ${EXTS})
    set(MAIN_STATIC_LIBRARY fae_static PARENT_SCOPE)    
  endif(True)
endif(MSYS)
