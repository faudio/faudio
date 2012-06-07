##
##   ScoreCleaner Audio Engine
##
##   Copyright (c) 2012 DoReMIR Music Research AB. 
##   All rights reserved. 
##

##
##   This file performs no builds, but sets the variable sclaudio_libraries in
##   the parent scope. This can be used with target_link_libraries to link to
##   the dependencies required by the audio engine.
##


##
## OS X dependencies
##

# FIXME What about Boost.DateTime?

if(${SCLAUDIO_OSX})

    set(CMAKE_SYSTEM_LIBRARY_PATH   "/usr/include")
    set(CMAKE_SYSTEM_FRAMEWORK_PATH "/System/Library/Frameworks")

    set(CMAKE_FRAMEWORK_PATH        "mac/frameworks")
    set(CMAKE_LIBRARY_PATH          "mac/static" 
                                    "mac/shared")

    if(${USE_DEBUG_LIBRARIES})   
        message("-- Using debug libraries ")
        find_library(BOOST_THREAD   "libboost_thread-mt-gd.a") 
    else()
        message("-- Not using debug libraries ")
        find_library(BOOST_THREAD   "libboost_thread-mt.a") 
    endif()

    find_library(SNDFILE            "libsndfile.a")
    find_library(PORTAUDIO          "libportaudio.a")
    find_library(PORTMIDI           "libportmidi_s.a")
                                    
    find_library(ICU_DATA           "libicudata.48.1.1.dylib")
    find_library(ICU_IO             "libicuio.48.1.1.dylib")
    find_library(ICU_UC             "libicuuc.48.1.1.dylib")

    find_library(COCOA              Cocoa)
    find_library(COREAUDIO          CoreAudio)
    find_library(COREMIDI           CoreMidi)
    find_library(AUDIOUNIT          AudioUnit)
    find_library(AUDIOTOOLBOX       AudioToolbox)
    find_library(FLUIDSYNTH         FluidSynth)

    message("-- Detected the following libraries:")
    message("  " ${BOOST_THREAD}   )

    message("  " ${PORTAUDIO}      )
    message("  " ${PORTMIDI}       )
    message("  " ${FLUIDSYNTH}     )
    
    message("  " ${ICU_DATA}       )
    message("  " ${ICU_IO}         )
    message("  " ${ICU_UC}         )

    message("  " ${COCOA}          )
    message("  " ${COREAUDIO}      )
    message("  " ${COREMIDI}       )
    message("  " ${AUDIOUNIT}      )
    message("  " ${AUDIOTOOLBOX}   )
                      
    set_property(
        CACHE 
            BOOST_THREAD 
            SNDFILE PORTAUDIO PORTMIDI FLUIDSYNTH
            ICU_DATA ICU_IO ICU_UC 
            COCOA COREAUDIO COREMIDI AUDIOUNIT AUDIOTOOLBOX 
        PROPERTY
            ADVANCED)

    set(SCLAUDIO_LIBRARIES 
          ${BOOST_THREAD} 
        
          ${PORTAUDIO} 
          ${PORTMIDI} 
          ${FLUIDSYNTH}
        
          ${ICU_DATA} 
          ${ICU_IO} 
          ${ICU_UC} 
        
          ${COCOA} 
          ${COREAUDIO} 
          ${COREMIDI} 
          ${AUDIOUNIT} 
          ${AUDIOTOOLBOX} 
          PARENT_SCOPE)

endif(${SCLAUDIO_OSX})



##
## Windows dependencies
##

if(${SCLAUDIO_WIN})

    # FIXME shouldn't we use the value of sclaudio_boost ?
    set(CMAKE_LIBRARY_PATH "C:/Program/boost/boost_1_47/lib"
                           "win/static")

    if(${USE_DEBUG_LIBRARIES})   
        message("-- Using debug libraries ")
        find_library(BOOST_THREAD     "libboost_thread-vc100-mt-gd-1_47.lib")
        find_library(BOOST_DATE_TIME  "libboost_date_time-vc100-mt-gd-1_47.lib")   
    else()    
        message("-- Not using debug libraries ")
        find_library(BOOST_THREAD     "libboost_thread-vc100-mt-1_47.lib")
        find_library(BOOST_DATE_TIME  "libboost_date_time-vc100-mt-1_47.lib")
    endif()
    
    find_library(PORTAUDIO         "portaudio_x86.lib")
    find_library(PORTMIDI          "portmidi.lib")
    find_library(ICU_DATA          "icudt.lib")
    find_library(ICU_IO            "icuio.lib")
    find_library(ICU_UC            "icuuc.lib")
    find_library(FLUIDSYNTH        "fluidsynth.lib")

    message("-- Detected the following libraries:")
    message("  " ${BOOST_THREAD}   )
    message("  " ${BOOST_DATE_TIME})

    message("  " ${PORTAUDIO}      )
    message("  " ${PORTMIDI}       )
    message("  " ${FLUIDSYNTH}     )
    
    message("  " ${ICU_DATA}       )
    message("  " ${ICU_IO}         )
    message("  " ${ICU_UC}         )

                      
    set_property(CACHE 
                    BOOST_THREAD BOOST_DATE_TIME
                    PORTAUDIO PORTMIDI FLUIDSYNTH
                    ICU_DATA ICU_IO ICU_UC 
                 PROPERTY
                    ADVANCED)

    set(SCLAUDIO_LIBRARIES 
          ${BOOST_THREAD} 
          ${BOOST_DATE_TIME} 
          
          ${PORTAUDIO} 
          ${PORTMIDI} 
          ${FLUIDSYNTH}
          
          ${ICU_DATA} 
          ${ICU_IO} 
          ${ICU_UC} 
          PARENT_SCOPE)

endif(${SCLAUDIO_WIN})
