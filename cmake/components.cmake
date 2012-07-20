
if(BUILD_DEPENDENCIES)
  set(AUDIO_ENGINE_BUILD_COMPONENTS True)
endif()

include(AudioEngine)
message(${AUDIO_ENGINE_SYSTEM_NAME})

predicate_file_exists(portaudio_exists 
  "${CMAKE_SOURCE_DIR}/external_libraries/portaudio/result/include/portaudio.h"
  )

add_component(COMPONENTS Portaudio 
  portaudio_exists
  "cmake/build/portaudio/apple/build"
  "cmake/build/portaudio/apple/clean"
  "portaudio-mac"
  )


predicate_file_exists(portmidi_exists 
  "${CMAKE_SOURCE_DIR}/external_libraries/portmidi/result/include/portmidi.h"
  )

add_component(COMPONENTS Portmidi 
  portmidi_exists
  "cmake/build/portmidi/apple/build"
  "cmake/build/portmidi/apple/clean"
  "portmidi-mac"
  )


predicate_file_exists(sndfile_exists 
  "${CMAKE_SOURCE_DIR}/external_libraries/sndfile/result/include/sndfile.h"
  )

add_component(COMPONENTS Sndfile 
  sndfile_exists
  "cmake/build/sndfile/apple/build"
  "cmake/build/sndfile/apple/clean"
  "sndfile-mac"
  )
