
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
  
predicate_file_exists(curl_exists
  "external/curl/result/include/curl/curl.h"
  )
add_standard_component(COMPONENTS Curl
  curl_exists
  ${AUDIO_ENGINE_SYSTEM_NAME}
  )
  
predicate_file_exists(liblo_exists
  "external/liblo/result/include/lo/lo.h"
  )
add_standard_component(COMPONENTS Liblo
  liblo_exists
  ${AUDIO_ENGINE_SYSTEM_NAME}
  )

if (NOT APPLE)
predicate_file_exists(fluidsynth_exists
  "external/fluidsynth/result/include/fluidsynth.h"
  )
add_standard_component(COMPONENTS Fluidsynth
  fluidsynth_exists
  ${AUDIO_ENGINE_SYSTEM_NAME}
  )       
endif()

predicate_file_exists(libogg_exists
  "external/libogg/result/include/ogg/ogg.h"
  )
add_standard_component(COMPONENTS Libogg
  libogg_exists
  ${AUDIO_ENGINE_SYSTEM_NAME}
  )

predicate_file_exists(libvorbis_exists
  "external/libvorbis/result/include/vorbis/vorbisenc.h"
  )
add_standard_component(COMPONENTS Libvorbis
  libvorbis_exists
  ${AUDIO_ENGINE_SYSTEM_NAME}
  )
  
predicate_file_exists(mpg123_exists
  "external/mpg123/result/include/mpg123.h"
  )
add_standard_component(COMPONENTS Mpg123
  mpg123_exists
  ${AUDIO_ENGINE_SYSTEM_NAME}
  )

predicate_file_exists(lame_exists
  "external/lame/result/include/lame/lame.h"
  )
add_standard_component(COMPONENTS Lame
  lame_exists
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
