
if(BUILD_DEPENDENCIES)
  set(AUDIO_ENGINE_BUILD_COMPONENTS True)
endif()

# FIXME can't pass these yet
set(AUDIO_ENGINE_BUILD_COMPONENTS True)
set(AUDIO_ENGINE_SHOW_COMPONENT_OUTPUT True)

set(AUDIO_ENGINE_WORKING_DIR "${CMAKE_SOURCE_DIR}/..")
message(">>>> ${AUDIO_ENGINE_WORKING_DIR}")

include(AudioEngine)
message(${AUDIO_ENGINE_SYSTEM_NAME})

predicate_file_exists(portaudio_exists 
  "external_libraries/portaudio/result/include/portaudio.h"
  )

add_component(COMPONENTS Portaudio 
  portaudio_exists
  "cmake/build/portaudio/osx/build"
  "cmake/build/portaudio/osx/clean"
  "portaudio-mac"
  )


predicate_file_exists(portmidi_exists 
  "external_libraries/portmidi/result/include/portmidi.h"
  )

add_component(COMPONENTS Portmidi 
  portmidi_exists
  "cmake/build/portmidi/osx/build"
  "cmake/build/portmidi/osx/clean"
  "portmidi-mac"
  )


predicate_file_exists(sndfile_exists 
  "external_libraries/sndfile/result/include/sndfile.h"
  )

add_component(COMPONENTS Sndfile 
  sndfile_exists
  "cmake/build/sndfile/osx/build"
  "cmake/build/sndfile/osx/clean"
  "sndfile-mac"
  )
