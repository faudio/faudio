
# Uses EXTS, APPLE and MSYS + options
# Sets MAIN_LIBRARY

# To update, use
#  find src -name '*'
list(APPEND SRCS
  utility.c
  core.c
  action.c
  atomic/queue.c
  # atomic/stack.c
  atomic/ring_buffer.c
  audio.c
  midi.c
  dynamic.c
  buffer.c
  error.c
  io.c
  io/ogg.c
  io/endian.c
  list.c
  map.c
  midi/message.c
  pair.c
  pair/left.c
  # plot/gnuplot.c
  # plot.c
  priority_queue.c
  ratio.c
  set.c
  signal.c
  string.c
  string/trex.c
  string/parson.c
  time.c
  option.c
  )
list(APPEND SRCS_OSX
  ../platform/macosx/atomic.c
  ../platform/macosx/thread.c
  ../platform/macosx/signal.c
  ../platform/macosx/string.c
  ../platform/macosx/device.c
  ../platform/macosx/au.m
  ../platform/macosx/vst.cc
  ../platform/macosx/clock.c
  ../platform/macosx/system/directory.c
  # ../platform/macosx/fae/system/event.c
  )

list(APPEND SRCS_MSYS
  ../platform/windows/atomic.c
  ../platform/windows/thread.c
  ../platform/windows/device.c
  ../platform/windows/system/directory.c
  ../platform/windows/signal.c
  ../platform/windows/clock.c
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
  set(MACOSX_FRAMEWORK_IDENTIFIER "com.doremir.Faudio")
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
    add_library(faudio_shared SHARED ${SRCS_ALL})
    set_target_properties(faudio_shared PROPERTIES
      OUTPUT_NAME                 "faudio"
      RUNTIME_OUTPUT_DIRECTORY    "../bin"
      )
    target_link_libraries(faudio_shared ${EXTS})
    set(MAIN_LIBRARY faudio_shared PARENT_SCOPE)
  endif(BUILD_SHARED)
  if(True)
    add_library(faudio_static ${SRCS_ALL})
    set_target_properties(faudio_static PROPERTIES
      OUTPUT_NAME                 "faudio"
      RUNTIME_OUTPUT_DIRECTORY    "../lib"
      )
    target_link_libraries(faudio_static ${EXTS})
    set(MAIN_STATIC_LIBRARY faudio_static PARENT_SCOPE)
  endif(True)
endif(MSYS)
