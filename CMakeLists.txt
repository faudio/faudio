#
# CMake build for the ScoreCleaner Audio Engine
#
# test with 
#   rm -rf build && ./boot && make edit_cache
# or
# rm -rf build && \
#     CMAKE_OSX_ARCHITECTURES=i386 ./boot && \
#     make edit_cachec

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
include_directories ( "external_libraries/boost" )
include_directories ( "external_libraries/vst" )
include_directories ( "external_libraries/unicode" )
include_directories ( "external_libraries/portaudio/result/include" )
include_directories ( "external_libraries/portmidi/result" )
include_directories ( "external_libraries/sndfile/result/include" )
include_directories ( "external_libraries/fluidsynth_headers" )


set ( CMAKE_LIBRARY_PATH
  "external_libraries/portaudio/result/lib"
  "external_libraries/portmidi/result"
  "external_libraries/sndfile/result/lib"
  "external_libraries/gtest/result"
  "lib/mac/shared" # for ICU, remove soon
  # "lib/mac/static" # for boost_thread, remove soon
    )
  
set ( CMAKE_FRAMEWORK_PATH 
  "/Developer/SDKs/MacOSX10.7.sdk/System/Library/Frameworks/"
  "lib/mac/frameworks" # for FluidSynth, remove soon
    )                                         

find_library ( BOOST_THREAD       "libboost_thread-mt.a") 
find_library ( ICONV              "iconv") 

find_library ( SNDFILE            "libsndfile.a" )
find_library ( PORTAUDIO          "libportaudio.a" )
find_library ( PORTMIDI           "libportmidi_s.a" )
                                  
find_library ( ICU_DATA           "libicudata.48.1.1.dylib" )
find_library ( ICU_IO             "libicuio.48.1.1.dylib" )
find_library ( ICU_UC             "libicuuc.48.1.1.dylib" )
find_library ( GTEST              "libgtest.dylib" )
               
find_library ( COCOA              Cocoa )
find_library ( COREAUDIO          CoreAudio )
find_library ( COREMIDI           CoreMidi )
find_library ( AUDIOUNIT          AudioUnit )
find_library ( AUDIOTOOLBOX       AudioToolbox )
find_library ( FLUIDSYNTH         FluidSynth )
# fftw or libfft ?
# libmicrohttpd ?
# oscpack

# list ( APPEND DEPS ${BOOST_THREAD} )
list ( APPEND DEPS ${ICONV} )
list ( APPEND DEPS ${SNDFILE} )
list ( APPEND DEPS ${PORTAUDIO} )
list ( APPEND DEPS ${PORTMIDI} )
list ( APPEND DEPS ${ICU_DATA} )
list ( APPEND DEPS ${ICU_IO} )
list ( APPEND DEPS ${ICU_UC} )
list ( APPEND DEPS ${GTEST} )
list ( APPEND DEPS ${COCOA} )
list ( APPEND DEPS ${COREAUDIO} )
list ( APPEND DEPS ${COREMIDI} )
list ( APPEND DEPS ${AUDIOUNIT} )
list ( APPEND DEPS ${AUDIOTOOLBOX} )
list ( APPEND DEPS ${FLUIDSYNTH} )

find_package(Pthreads)
add_subdirectory ( external_libraries )

list ( APPEND DEPS boost_system )
list ( APPEND DEPS boost_filesystem )
list ( APPEND DEPS boost_thread )

add_subdirectory( src )
# add_subdirectory( test )
# add_subdirectory( tool )

add_custom_target ( foo ALL
  COMMAND echo 1 )
  
configure_file ( 
  ${CMAKE_SOURCE_DIR}/include/sclaudiox/config.h.in
  ${CMAKE_BINARY_DIR}/include/sclaudiox/config.h
  )






