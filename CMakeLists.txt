##
##  CMake build for the ScoreCleaner Audio Engine
##
##  Test with
##      rm -rf build && ./boot && make edit_cache

cmake_minimum_required(VERSION 2.8.8)
set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/modules)

project(ScoreCleanerAudioEngine C CXX)
set(VERSION_PRE "")
set(VERSION_X 1)
set(VERSION_Y 5)
set(VERSION_Z 0)
set(VERSION_SUFF "")
set(VERSION "${VERSION_PRE}${VERSION_X}.${VERSION_Y}.${VERSION_Z}${VERSION_SUFF}")


## Sanity checks

# if(APPLE)
#   string( COMPARE NOTEQUAL "${CMAKE_OSX_ARCHITECTURES}" "i386" ARCH_NOT_32_BIT)
#   if(ARCH_NOT_32_BIT)
#     message(FATAL_ERROR "The Audio Engine does not support 64-bit mode yet.")
#   endif ()
# endif ()

## Build options

#  This should hold
#    CMAKE_OSX_DEPLOYMENT_TARGET <= CMAKE_OSX_SYSROOT <= (build machine OS X version)
#  
#  CMAKE_OSX_DEPLOYMENT_TARGET should be the minimum supported version (i.e. 10.5)
#  CMAKE_OSX_SYSROOT should be the SDK of the maximum supported version (i.e. /Developer/SDKs/MacOSX10.7.sdk)
#  
#  See the Apple documentation 'Frameworks and Weak Linking'

option(ENABLE_LOGGING           "Enable standard logging"                   FALSE)     
option(REDIRECT_LOG             "Redirect log messages to standard output"  FALSE)      
option(ENABLE_REALTIME_AUDIO    "")
option(ENABLE_REALTIME_MIDI     "")
option(ENABLE_NONREALTIME_AUDIO "")
option(ENABLE_FLUIDSYNTH        "")
option(ENABLE_FFT               "")

option(BUILD_DYLIB              "Build an OS X dynamic library"             FALSE)
option(BUILD_FRAMEWORK          "Build an OS X framework"                   TRUE)
option(PROFILING                "Compile with profiler flags -pg set"       FALSE)
option(BUILD_TESTS              "Build unit tests"                          TRUE)
option(RUN_TESTS                "Run unit tests after build"                TRUE)

option(BUILD_SOUNDFONT_TOOL     "")
option(BUILD_SOUNDFILE_TOOL     "")
option(BUILD_OSC_SERVER         "")
option(BUILD_HTTP_SERVER        "")

option(PREFER_NONBLOCKING       "Use non-blocking audio by default"         TRUE)      
option(PREFER_EXCLUSIVE         "Use exclusive audio streams by default"    TRUE)

option(BUILD_COMPONENTS         "Build dependent components locally"        FALSE)
option(SHOW_COMPONENT_OUTPUT    "Show output while building components"     FALSE)

## Misc settings

# include(Profiling)
# include(InstallDist)

## Find libraries

# find_package(pthreads)
# find_package(iconv)
# find_package(portaudio)
# find_package(portmidi)
# find_package(sndfile)
# find_package(gtest)

# add_definitions(${PTHREAD_DEFINITIONS})
# add_definitions(${ICONV_DEFINITIONS})
#       
# include_directories("include")
# include_directories("${CMAKE_BINARY_DIR}/include")
# include_directories(${PTHREADS_INCLUDE_DIRS})
# include_directories(${ICONV_INCLUDE_DIRS})
# include_directories("external_libraries/boost")
# include_directories("external_libraries/vst")
# include_directories("external_libraries/gtest/include")
# include_directories("external_libraries/portaudio/result/include")
# include_directories("external_libraries/portmidi/result")
# include_directories("external_libraries/sndfile/result/include")
# include_directories("external_libraries/fluidsynth_headers")
# 
# set(CMAKE_LIBRARY_PATH
#   ${PTHREADS_LIBRARIES}
#   ${ICONV_LIBRARIES}
#   "external_libraries/portaudio/result/lib"
#   "external_libraries/portmidi/result"
#   "external_libraries/sndfile/result/lib"
#   "lib/mac/shared" # for ICU, remove soon
#   "lib/mac/gtest" # for gtest, remove soon
#    )
#   
# set(CMAKE_FRAMEWORK_PATH 
#   "/Developer/SDKs/MacOSX10.7.sdk/System/Library/Frameworks/"
#   "lib/mac/frameworks" # for FluidSynth, remove soon
#   )                                        
# 
# find_library(SNDFILE            "libsndfile.a")
# find_library(PORTAUDIO          "libportaudio.a")
# find_library(PORTMIDI           "libportmidi_s.a")
# find_library(GTEST              "gtest")
# 
# list(APPEND COMPONENTS ${ICONV})
# list(APPEND COMPONENTS ${SNDFILE})
# list(APPEND COMPONENTS ${PORTAUDIO})
# list(APPEND COMPONENTS ${PORTMIDI})
# list(APPEND COMPONENTS ${GTEST})
# 
# if(APPLE)
#   find_library(COCOA              Cocoa)
#   find_library(COREAUDIO          CoreAudio)
#   find_library(COREMIDI           CoreMidi)
#   find_library(AUDIOUNIT          AudioUnit)
#   find_library(AUDIOTOOLBOX       AudioToolbox)
#   find_library(FLUIDSYNTH         FluidSynth)
# 
#   list(APPEND COMPONENTS ${COCOA})
#   list(APPEND COMPONENTS ${COREAUDIO})
#   list(APPEND COMPONENTS ${COREMIDI})
#   list(APPEND COMPONENTS ${AUDIOUNIT})
#   list(APPEND COMPONENTS ${AUDIOTOOLBOX})
#   list(APPEND COMPONENTS ${FLUIDSYNTH})
# endif ()
# 
# # fftw or libfft ?
# # libmicrohttpd ?
# # oscpack
# 
# add_subdirectory(external_libraries)
# 
# list(APPEND COMPONENTS BoostSystem)
# list(APPEND COMPONENTS BoostFilesystem)
# list(APPEND COMPONENTS BoostThread)
# 
# 
# ## Configuration file
# 
# set(VERSION_STRING "\"${VERSION}\"")
# if(PREFER_NONBLOCKING)
#   set(SCL_AUDIO_PREFER_NONBLOCKING True)
# endif ()
# if(PREFER_EXCLUSIVE)
#   set(SCL_AUDIO_PREFER_EXCLUSIVE True)
# endif ()
# if(ENABLE_LOGGING)
#   set(SCL_AUDIO_ENABLE_LOGGING True)
# endif ()
# if(REDIRECT_LOG)
#   set(SCL_AUDIO_REDIRECT_LOG True)
# endif ()
# 
# configure_file(
#   ${CMAKE_SOURCE_DIR}/include/sclaudiox/config.h.in
#   ${CMAKE_BINARY_DIR}/include/sclaudiox/config.h
#  )
#            

## Process source

# add_subdirectory( src)
# add_subdirectory( test)
# add_subdirectory( tool)



# Testing ...
# add_custom_target( install_dist)






set(COMPONENT_SCRIPT_DEFINES "")

if(BUILD_COMPONENTS)
  list(APPEND COMPONENT_SCRIPT_DEFINES 
    "-DAUDIO_ENGINE_BUILD_COMPONENTS=True")
endif()

if(SHOW_COMPONENT_OUTPUT)
  list(APPEND COMPONENT_SCRIPT_DEFINES 
    "-DAUDIO_ENGINE_SHOW_COMPONENT_OUTPUT=True")
endif()

list(APPEND COMPONENT_SCRIPT_DEFINES
  "-DAUDIO_ENGINE_WORKING_DIR=${CMAKE_SOURCE_DIR}")

message("~~~ ${COMPONENT_SCRIPT_DEFINES}")

include(AudioEngine)

add_custom_target(components_print
  COMMAND cmake 
    ${COMPONENT_SCRIPT_DEFINES}
    -P "${CMAKE_SOURCE_DIR}/cmake/scripts/components_print.cmake"
 )

add_custom_target(components_resolve
  COMMAND cmake 
    ${COMPONENT_SCRIPT_DEFINES}
    -P "${CMAKE_SOURCE_DIR}/cmake/scripts/components_resolve.cmake"
 )

add_custom_target(components_clean
  COMMAND cmake
    ${COMPONENT_SCRIPT_DEFINES}
    -P "${CMAKE_SOURCE_DIR}/cmake/scripts/components_clean.cmake"
 )





