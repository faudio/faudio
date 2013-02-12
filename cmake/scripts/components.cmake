
# Include to define the components of the audio engine

include(AudioEngine)

predicate_file_exists(portaudio_exists
  "external/portaudio/result/include/portaudio.h"
  )
add_standard_component(COMPONENTS Portaudio
  portaudio_exists
  ${AUDIO_ENGINE_SYSTEM_NAME}
  )

predicate_file_exists(portmidi_exists
  "external/portmidi/result/include/portmidi.h"
  )
add_standard_component(COMPONENTS Portmidi
  portmidi_exists
  ${AUDIO_ENGINE_SYSTEM_NAME}
  )

predicate_file_exists(sndfile_exists
  "external/sndfile/result/include/sndfile.h"
  )
add_standard_component(COMPONENTS Sndfile
  sndfile_exists
  ${AUDIO_ENGINE_SYSTEM_NAME}
  )

predicate_file_exists(fluidsynth_exists
  "external/fluidsynth/result/include/fluidsynth.h"
  )
add_standard_component(COMPONENTS Fluidsynth
  fluidsynth_exists
  ${AUDIO_ENGINE_SYSTEM_NAME}
  )

# predicate_file_exists(gtest_exists
#   "external/gtest/result/include/gtest/gtest.h"
#   )
# add_standard_component(COMPONENTS Gtest
#   gtest_exists
#   ${AUDIO_ENGINE_SYSTEM_NAME}
#   )
# 
# predicate_file_exists(icu_exists
#   "external/icu2/result/include/unicode/uchar.h"
#   )
# add_standard_component(COMPONENTS ICU
#   icu_exists
#   ${AUDIO_ENGINE_SYSTEM_NAME}
#   )
