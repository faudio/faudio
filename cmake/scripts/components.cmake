# Include to define the components of the audio engine

include(AudioEngine)

# message(">>>> ${AUDIO_ENGINE_SYSTEM_NAME}")
# message(">>>> ${AUDIO_ENGINE_WORKING_DIR}")
# message(">>>> ${AUDIO_ENGINE_BUILD_COMPONENTS}")
# message(">>>> ${AUDIO_ENGINE_SHOW_COMPONENT_OUTPUT}")


predicate_file_exists(portaudio_exists
  "external_libraries/portaudio/result/include/portaudio.h"
  )
add_standard_component(COMPONENTS Portaudio
  portaudio_exists
  ${AUDIO_ENGINE_SYSTEM_NAME}
  )


predicate_file_exists(portmidi_exists
  "external_libraries/portmidi/result/include/portmidi.h"
  )
add_standard_component(COMPONENTS Portmidi
  portmidi_exists
  ${AUDIO_ENGINE_SYSTEM_NAME}
  )


predicate_file_exists(sndfile_exists
  "external_libraries/sndfile/result/include/sndfile.h"
  )
add_standard_component(COMPONENTS Sndfile
  sndfile_exists
  ${AUDIO_ENGINE_SYSTEM_NAME}
  )

predicate_file_exists(gtest_exists
  "external_libraries/gtest/result/include/gtest/gtest.h"
  )
add_standard_component(COMPONENTS Gtest
  gtest_exists
  ${AUDIO_ENGINE_SYSTEM_NAME}
  )
