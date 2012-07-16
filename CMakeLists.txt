##
##  CMake build for the ScoreCleaner Audio Engine
##
##  Test with
##      rm -rf build && ./boot && make edit_cache

cmake_minimum_required ( VERSION 2.8.8 )
set ( CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake )

project ( ScoreCleanerAudioEngine C CXX )
set ( VERSION_PRE "" )
set ( VERSION_X 1 )
set ( VERSION_Y 5 )
set ( VERSION_Z 0 )
set ( VERSION_SUFF "" )
set ( VERSION "${VERSION_PRE}${VERSION_X}.${VERSION_Y}.${VERSION_Z}${VERSION_SUFF}" )

option ( ENABLE_UNICODE           "Enable Unicode support"                    TRUE )       
option ( ENABLE_LOGGING           "Enable standard logging"                   FALSE )      
option ( REDIRECT_LOG             "Redirect log messages to standard output"  FALSE )       
option ( ENABLE_REALTIME_AUDIO    "" )
option ( ENABLE_REALTIME_MIDI     "" )
option ( ENABLE_NONREALTIME_AUDIO "" )
option ( ENABLE_FLUIDSYNTH        "" )
option ( ENABLE_FFT               "" )

option ( BUILD_DYLIB              "Build an OS X dynamic library"             FALSE )
option ( BUILD_FRAMEWORK          "Build an OS X framework"                   TRUE )
option ( PROFILING                "Compile with profiler flags -pg set"       FALSE )
option ( BUILD_TESTS              "Build unit tests"                          TRUE )
option ( RUN_TESTS                "Run unit tests after build"                TRUE )

option ( BUILD_SOUNDFONT_TOOL     "" )
option ( BUILD_SOUNDFILE_TOOL     "" )
option ( BUILD_OSC_SERVER         "" )
option ( BUILD_HTTP_SERVER        "" )

option ( PREFER_NONBLOCKING       "Use non-blocking audio by default"         TRUE )       
option ( PREFER_EXCLUSIVE         "Use exclusive audio streams by default"    TRUE )

# This should hold
#   CMAKE_OSX_DEPLOYMENT_TARGET <= CMAKE_OSX_SYSROOT <= (build system version)
#
# CMAKE_OSX_DEPLOYMENT_TARGET should be the minimum supported version
# CMAKE_OSX_SYSROOT should be the maximum supported versions
#
# See the Apple documentation 'Frameworks and Weak Linking'
      
include_directories ( "include" )
include_directories ( "${CMAKE_BINARY_DIR}/include" )
include_directories ( "external_libraries/boost" )
include_directories ( "external_libraries/vst" )
# include_directories ( "external_libraries/unicode" )
include_directories ( "external_libraries/gtest/include" )
include_directories ( "external_libraries/portaudio/result/include" )
include_directories ( "external_libraries/portmidi/result" )
include_directories ( "external_libraries/sndfile/result/include" )
include_directories ( "external_libraries/fluidsynth_headers" )


set ( CMAKE_LIBRARY_PATH
  "external_libraries/portaudio/result/lib"
  "external_libraries/portmidi/result"
  "external_libraries/sndfile/result/lib"
  # "external_libraries/gtest/result"
  "lib/mac/shared" # for ICU, remove soon
  "lib/mac/gtest" # for gtest, remove soon
  # "lib/mac/static" # for boost_thread, remove soon
    )
  
set ( CMAKE_FRAMEWORK_PATH 
  "/Developer/SDKs/MacOSX10.7.sdk/System/Library/Frameworks/"
  "lib/mac/frameworks" # for FluidSynth, remove soon
    )                                         

find_library ( ICONV              "iconv") 

find_library ( SNDFILE            "libsndfile.a" )
find_library ( PORTAUDIO          "libportaudio.a" )
find_library ( PORTMIDI           "libportmidi_s.a" )
                                  
#find_library ( ICU_DATA           "libicudata.dylib" )
#find_library ( ICU_IO             "libicuio.dylib" )
#find_library ( ICU_UC             "libicuuc.dylib" )
find_library ( GTEST              "gtest" )
               
find_library ( COCOA              Cocoa )
find_library ( COREAUDIO          CoreAudio )
find_library ( COREMIDI           CoreMidi )
find_library ( AUDIOUNIT          AudioUnit )
find_library ( AUDIOTOOLBOX       AudioToolbox )
find_library ( FLUIDSYNTH         FluidSynth )
# fftw or libfft ?
# libmicrohttpd ?
# oscpack

list ( APPEND DEPENDENCIES ${ICONV} )
list ( APPEND DEPENDENCIES ${SNDFILE} )
list ( APPEND DEPENDENCIES ${PORTAUDIO} )
list ( APPEND DEPENDENCIES ${PORTMIDI} )
# list ( APPEND DEPENDENCIES ${ICU_DATA} )
# list ( APPEND DEPENDENCIES ${ICU_IO} )
# list ( APPEND DEPENDENCIES ${ICU_UC} )
list ( APPEND DEPENDENCIES ${GTEST} )
list ( APPEND DEPENDENCIES ${COCOA} )
list ( APPEND DEPENDENCIES ${COREAUDIO} )
list ( APPEND DEPENDENCIES ${COREMIDI} )
list ( APPEND DEPENDENCIES ${AUDIOUNIT} )
list ( APPEND DEPENDENCIES ${AUDIOTOOLBOX} )
list ( APPEND DEPENDENCIES ${FLUIDSYNTH} )

find_package ( pthreads )
find_package ( iconv )
add_subdirectory ( external_libraries )

list ( APPEND DEPENDENCIES BoostSystem )
list ( APPEND DEPENDENCIES BoostFilesystem )
list ( APPEND DEPENDENCIES BoostThread )

if ( PROFILING AND CMAKE_COMPILER_IS_GNUCXX )
  add_definitions("-pg")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -pg")
endif ()

set ( VERSION_STRING "\"${VERSION}\"" )
configure_file ( 
  ${CMAKE_SOURCE_DIR}/include/sclaudiox/config.h.in
  ${CMAKE_BINARY_DIR}/include/sclaudiox/config.h
  )


add_subdirectory( src )
add_subdirectory( test )
# add_subdirectory( tool )





