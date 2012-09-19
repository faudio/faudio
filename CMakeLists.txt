##
##  CMake build for the ScoreCleaner Audio Engine
##
##  Test with
##      rm -rf build && ./boot && make edit_cache

cmake_minimum_required(VERSION 2.8.8)
set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/modules)

## Select language version
include(FindC++0x)

project(ScoreCleanerAudioEngine C CXX)
set(VERSION_PRE "")
set(VERSION_X 1)
set(VERSION_Y 5)
set(VERSION_Z 3)
set(VERSION_SUFF "")
set(VERSION "${VERSION_PRE}${VERSION_X}.${VERSION_Y}.${VERSION_Z}${VERSION_SUFF}")


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

include(Profiling)
include(FixOSXLionPaths)
find_package(Dist)

## Sanity checks

if(APPLE)
  string(COMPARE NOTEQUAL "${CMAKE_OSX_ARCHITECTURES}" "i386" ARCH_NOT_32_BIT)
  if(ARCH_NOT_32_BIT)
    message(FATAL_ERROR 
      "\nThe Audio Engine does not support 64-bit mode yet."
      "\nTry -DCMAKE_OSX_ARCHITECTURES=i386.")
  endif()
  
  string(COMPARE EQUAL "${CMAKE_OSX_DEPLOYMENT_TARGET}" "" NO_DEP_TARGET)
  if(NO_DEP_TARGET)
    message(FATAL_ERROR 
      "\nNo OS X deployment target set."
      "\nTry -DCMAKE_OSX_DEPLOYMENT_TARGET=10.5.")    
  endif()
endif()


## Component manager

if(BUILD_COMPONENTS)
  list(APPEND COMPONENT_DEFINES 
    "-DAUDIO_ENGINE_BUILD_COMPONENTS=True")
endif()
if(SHOW_COMPONENT_OUTPUT)
  list(APPEND COMPONENT_DEFINES 
    "-DAUDIO_ENGINE_SHOW_COMPONENT_OUTPUT=True")
endif()
if(DIST_EXECUTABLE)
  list(APPEND COMPONENT_DEFINES 
    "-DDIST_EXECUTABLE=${DIST_EXECUTABLE}")
endif()
list(APPEND COMPONENT_DEFINES
  "-DAUDIO_ENGINE_WORKING_DIR=${CMAKE_SOURCE_DIR}")

include(AudioEngine)

add_custom_target(components_print
  COMMAND cmake ${COMPONENT_DEFINES} -P "${CMAKE_SOURCE_DIR}/cmake/scripts/components_print.cmake")

add_custom_target(components_resolve
  COMMAND cmake ${COMPONENT_DEFINES} -P "${CMAKE_SOURCE_DIR}/cmake/scripts/components_resolve.cmake")

add_custom_target(components_clean
  COMMAND cmake ${COMPONENT_DEFINES} -P "${CMAKE_SOURCE_DIR}/cmake/scripts/components_clean.cmake")

# add_custom_target(format
#   COMMAND sh "${CMAKE_SOURCE_DIR}/cmake/scripts/astyle.sh" scl
#   WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
#   )

add_custom_target(run_scorecleaner
  COMMAND sh "${CMAKE_SOURCE_DIR}/cmake/scripts/run_scorecleaner.sh"
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  )


if (NOT COMPONENTS_RESOLVED)
  # message(STATUS
  #   "\n Note: The flag COMPONENTS_RESOLVED is not set."
  #   )

else (NOT COMPONENTS_RESOLVED)
  ## Find libraries

  find_package(Pthreads)
  find_package(Iconv)
  find_package(Portaudio)
  find_package(Portmidi)
  find_package(Sndfile)
  # find_package(Fluidsynth)
  # find_package(Glib)
  find_package(Gtest)         
  # find_package(ICU)

  add_definitions(${PTHREAD_DEFINITIONS})
  add_definitions(${ICONV_DEFINITIONS})
  add_definitions(-DSCL_UNICODE=1)            # Note: Has no effect in rewrite
  add_definitions(-DSCL_ADJUST_AU_VOLUME=1)   # Note: Has no effect in rewrite
  
  if (APPLE)
    add_definitions(-DGTEST_USE_OWN_TR1_TUPLE=1)                          
    # message("Sysroot is " ${CMAKE_OSX_SYSROOT})
    # message("Target is "${CMAKE_OSX_DEPLOYMENT_TARGET}) 
    add_definitions("-mmin-macosx-version=${CMAKE_OSX_DEPLOYMENT_TARGET}")
    add_definitions("-mmacosx-version-min=${CMAKE_OSX_DEPLOYMENT_TARGET}")
    add_definitions("-isysroot ${CMAKE_OSX_SYSROOT}")
    set(STD_LINK_FLAGS "${STD_LINK_FLAGS} -mmin-macosx-version=${CMAKE_OSX_DEPLOYMENT_TARGET}")
    set(STD_LINK_FLAGS "${STD_LINK_FLAGS} -mmacosx-version-min=${CMAKE_OSX_DEPLOYMENT_TARGET}")
    set(STD_LINK_FLAGS "${STD_LINK_FLAGS} -isysroot ${CMAKE_OSX_SYSROOT}")
  endif()
  if (WIN32) # only MSYS really...
    # add_definitions(-static-libgcc)	 FIXME
    # add_definitions(-static-libstdc++) FIXME
    add_definitions(-DBOOST_THREAD_USE_LIB=1)
    add_definitions(-DFLUIDSYNTH_NOT_A_DLL=1)
  endif()
  add_definitions(-DBOOST_CHRONO_HEADER_ONLY=1)

  include_directories("include")
  include_directories("${CMAKE_BINARY_DIR}/include") # for the conf file
  include_directories(${CMAKE_SOURCE_DIR})           # for scl and scl-c
  include_directories(${PTHREADS_INCLUDE_DIRS})
  include_directories(${ICONV_INCLUDE_DIRS})
  include_directories(${PORTAUDIO_INCLUDE_DIRS})
  include_directories(${PORTMIDI_INCLUDE_DIRS})
  # include_directories(${SNDFILE_INCLUDE_DIRS})
  # include_directories(${FLUIDSYNTH_INCLUDE_DIRS})
  include_directories(${GTEST_INCLUDE_DIRS})
  include_directories("external_libraries/icu2/result/include")

  include_directories("external_libraries/boost")
  include_directories("external_libraries/boost_lockfree")
  include_directories("external_libraries/vst")

  set(CMAKE_LIBRARY_PATH
    ${PTHREADS_LIBRARIES}
    ${ICONV_LIBRARIES}
    ${PORTAUDIO_LIBRARIES}
    ${PORTMIDI_LIBRARIES}
    # ${SNDFILE_LIBRARIES}
    # ${FLUIDSYNTH_LIBRARIES}
    ${GLIB_LIBRARIES}
    ${GTEST_LIBRARIES}
    "external_libraries/icu2/result/lib"
    "/mingw/lib"
    "/mingw"
    )

  # TODO Do we really need to explicitly set this?
  set(CMAKE_FRAMEWORK_PATH 
    "${CMAKE_OSX_SYSROOT}/System/Library/Frameworks/"
    )

  list(APPEND DEPENDENCIES ${PTHREADS_LIBRARIES})
  list(APPEND DEPENDENCIES ${ICONV_LIBRARIES})
  list(APPEND DEPENDENCIES ${PORTAUDIO_LIBRARIES})
  list(APPEND DEPENDENCIES ${PORTMIDI_LIBRARIES})
  # list(APPEND DEPENDENCIES ${SNDFILE_LIBRARIES})
  # list(APPEND DEPENDENCIES ${FLUIDSYNTH_LIBRARIES})
  list(APPEND DEPENDENCIES ${GLIB_LIBRARIES})
  list(APPEND DEPENDENCIES ${GTEST_LIBRARIES})
  list(APPEND DEPENDENCIES "${CMAKE_SOURCE_DIR}/external_libraries/icu2/result/lib/libicudata.a")
  list(APPEND DEPENDENCIES "${CMAKE_SOURCE_DIR}/external_libraries/icu2/result/lib/libicui18n.a")
  list(APPEND DEPENDENCIES "${CMAKE_SOURCE_DIR}/external_libraries/icu2/result/lib/libicuio.a")
  list(APPEND DEPENDENCIES "${CMAKE_SOURCE_DIR}/external_libraries/icu2/result/lib/libicule.a")
  list(APPEND DEPENDENCIES "${CMAKE_SOURCE_DIR}/external_libraries/icu2/result/lib/libiculx.a")
  list(APPEND DEPENDENCIES "${CMAKE_SOURCE_DIR}/external_libraries/icu2/result/lib/libicutu.a")  
  list(APPEND DEPENDENCIES "${CMAKE_SOURCE_DIR}/external_libraries/icu2/result/lib/libicuuc.a")
  
  if (WIN32)
    find_library (MINGW32 NAMES mingw32 PATH_SUFFIXES lib PATHS "C:/MinGW")
    find_library (OLE32   NAMES ole32   PATH_SUFFIXES lib PATHS "C:/MinGW")
    find_library (WINMM   NAMES winmm   PATH_SUFFIXES lib PATHS "C:/MinGW")
    find_library (WINSOCK NAMES ws2_32  PATH_SUFFIXES lib PATHS "C:/MinGW")
    list(APPEND DEPENDENCIES ${WINSOCK})
    list(APPEND DEPENDENCIES ${WINMM})
    list(APPEND DEPENDENCIES ${OLE32})
    list(APPEND DEPENDENCIES ${MINGW32})
  endif()
  
  if(APPLE)
    find_library(COCOA              Cocoa)
    find_library(COREAUDIO          CoreAudio)
    find_library(COREMIDI           CoreMidi)
    find_library(AUDIOUNIT          AudioUnit)
    find_library(AUDIOTOOLBOX       AudioToolbox)
    list(APPEND DEPENDENCIES ${COCOA})
    list(APPEND DEPENDENCIES ${COREAUDIO})
    list(APPEND DEPENDENCIES ${COREMIDI})
    list(APPEND DEPENDENCIES ${AUDIOUNIT})
    list(APPEND DEPENDENCIES ${AUDIOTOOLBOX})
  endif ()

  # fftw or libfft ?
  # libmicrohttpd ?
  # oscpack
  
  if(WIN32)
    set(USE_BOOST_THREAD_DEFAULT True)
    set(USE_BOOST_ATOMIC_DEFAULT True)
  else()
    set(USE_BOOST_THREAD_DEFAULT True)
    set(USE_BOOST_ATOMIC_DEFAULT True)
  endif()
  
  option(USE_BOOST_THREAD "Force using Boost threads" ${USE_BOOST_THREAD_DEFAULT})
  if (USE_BOOST_THREAD)
    add_definitions(-DSCL_USE_BOOST_THREAD=1)
  endif()
  
  option(USE_BOOST_ATOMIC "Force using Boost atomics" ${USE_BOOST_ATOMIC_DEFAULT})
  if (USE_BOOST_THREAD)
    add_definitions(-DSCL_USE_BOOST_ATOMIC=1)
  endif()

  add_subdirectory(external_libraries)

  if (USE_BOOST_THREAD)
    list(APPEND DEPENDENCIES BoostSystem)
    list(APPEND DEPENDENCIES BoostFilesystem)
    list(APPEND DEPENDENCIES BoostThread)
    # list(APPEND DEPENDENCIES BoostChrono)
  endif()

  ## Configuration file

  set(VERSION_STRING "\"${VERSION}\"")
  if(PREFER_NONBLOCKING)
    set(SCL_AUDIO_PREFER_NONBLOCKING True)
  endif ()
  if(PREFER_EXCLUSIVE)
    set(SCL_AUDIO_PREFER_EXCLUSIVE True)
  endif ()
  if(ENABLE_LOGGING)
    set(SCL_AUDIO_ENABLE_LOGGING True)
  endif ()
  if(REDIRECT_LOG)
    set(SCL_AUDIO_REDIRECT_LOG True)
  endif ()

  configure_file(
    ${CMAKE_SOURCE_DIR}/include/sclaudiox/config.h.in
    ${CMAKE_BINARY_DIR}/include/sclaudiox/config.h
   )

  ## Process source

  add_subdirectory(src)
  # add_subdirectory(test)
  # add_subdirectory(tools)

endif(NOT COMPONENTS_RESOLVED)
