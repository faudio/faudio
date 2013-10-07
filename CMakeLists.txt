
##
##  CMake build for FA
##
##  This script is slighly unusual, in that it requires components_resolve to be called once before
##  building anything. Follow the instructions in README and all we be well.
##

cmake_minimum_required(VERSION 2.8.8)
set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/modules)

## ----------------------------------------------------------------------
## Basic definitions

project(FA C CXX)
set(VERSION_PRE "")
set(VERSION_X 2)
set(VERSION_Y 2)
set(VERSION_Z 2)
set(VERSION_SUFF "")
set(VERSION "${VERSION_PRE}${VERSION_X}.${VERSION_Y}.${VERSION_Z}${VERSION_SUFF}")

if(APPLE)
  set(NOT_APPLE False)
else()
  set(NOT_APPLE True)
endif()
if(WIN32) # TODO better check
  set(MSYS     True)
  set(NOT_MSYS False)
else()
  set(MSYS     False)
  set(NOT_MSYS True)
endif()


## ----------------------------------------------------------------------
## Build options

#
#  This should hold
#      CMAKE_OSX_DEPLOYMENT_TARGET 
#         <= CMAKE_OSX_SYSROOT 
#         <= (build machine OS X version)
#  
#      CMAKE_OSX_DEPLOYMENT_TARGET is minimum supported version (i.e. 10.5)
#      CMAKE_OSX_SYSROOT is path to SDK of maximum supported version
#  

option(ENABLE_REALTIME_AUDIO    "Enable real-time audio"                    True)
option(ENABLE_REALTIME_MIDI     "Enable real-time midi"                     True)
option(ENABLE_NONREALTIME_AUDIO "Enable non-real-time audio"                True)
option(ENABLE_FLUIDSYNTH        "Enable the Fluidsynth processor"           ${NOT_APPLE})
option(ENABLE_AUDIO_UNIT        "Enable AudioUnit processors"               ${APPLE})
option(ENABLE_VST               "Enable VST processors"                     True)

option(BUILD_FRAMEWORK          "Build an OS X framework"                   ${APPLE})
option(BUILD_SHARED             "Build a shared library"                    ${NOT_APPLE})
option(BUILD_TESTS              "Build unit tests"                          True)

option(PROFILING                "Compile with profiler flags -pg set"       False)

option(BUILD_COMPONENTS         "Build dependent components locally"        True)
option(SHOW_COMPONENT_OUTPUT    "Show output while building components"     True)


## ----------------------------------------------------------------------
## Misc setup

include(Profiling)
include(FixOSXLionPaths)
find_package(Dist)


## ----------------------------------------------------------------------
## Sanity checks

if(APPLE)
  # string(COMPARE NOTEQUAL "${CMAKE_OSX_ARCHITECTURES}" "i386" ARCH_NOT_32_BIT)
  # if(ARCH_NOT_32_BIT)
  #   message(FATAL_ERROR
  #     "\nThe Audio Engine does not support 64-bit mode yet."
  #     "\nTry -DCMAKE_OSX_ARCHITECTURES=i386.")
  # endif()

  string(COMPARE EQUAL "${CMAKE_OSX_DEPLOYMENT_TARGET}" "" NO_DEP_TARGET)
  if(NO_DEP_TARGET)
    message(FATAL_ERROR
      "\nNo OS X deployment target set."
      "\nTry -DCMAKE_OSX_DEPLOYMENT_TARGET=10.5.")
  endif()
endif()

if(MSYS)
  string(COMPARE NOTEQUAL "${CMAKE_GENERATOR}" "MSYS Makefiles" GENERATOR_NOT_MSYS)
  if(GENERATOR_NOT_MSYS)
    message(FATAL_ERROR
      "\nMust use MSYS Makefiles generator."
      "\nTry -G 'MSYS Makefiles'.")
  endif()
endif()


## ----------------------------------------------------------------------
## Component manager

# 
#  These steps sets up the component manager and adds the components_resolve target
# 
#  Usually there is no need to use this explicitly, as the boot script calls 
#  "make components_resolve" and it only needs to be called once. Note that components_resolve
#  calls cmake again to proceed below the guard below.
# 

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
  COMMAND cmake 
  ${COMPONENT_DEFINES} 
  -P "${CMAKE_SOURCE_DIR}/cmake/scripts/components_print.cmake")

add_custom_target(components_resolve
  COMMAND cmake 
  ${COMPONENT_DEFINES} 
  -P "${CMAKE_SOURCE_DIR}/cmake/scripts/components_resolve.cmake")

add_custom_target(components_clean
  COMMAND cmake 
  ${COMPONENT_DEFINES} 
  -P "${CMAKE_SOURCE_DIR}/cmake/scripts/components_clean.cmake")

add_custom_target(format
  COMMAND sh "${CMAKE_SOURCE_DIR}/cmake/scripts/astyle.sh" scl
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  )

add_custom_target(run_scorecleaner
  COMMAND sh "${CMAKE_SOURCE_DIR}/cmake/scripts/run_scorecleaner.sh"
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  )


## ----------------------------------------------------------------------
## Add main targets

#  We only proceed here if the component mangager has been run, as per above.

if (COMPONENTS_RESOLVED)
  
  ## ----------------------------------------------------------------------
  # Find packages

  find_package(Pthreads)
  find_package(Iconv)
  find_package(Portaudio)
  find_package(Portmidi)
  find_package(Sndfile)
  if(ENABLE_FLUIDSYNTH)
    find_package(Fluidsynth)
    find_package(Glib)
  endif()

  ## ----------------------------------------------------------------------
  ## Add compiler defs

  add_definitions(${PTHREAD_DEFINITIONS})
  add_definitions(${ICONV_DEFINITIONS})

  if (APPLE)
    # message("Sysroot is " ${CMAKE_OSX_SYSROOT})
    # message("Target is "${CMAKE_OSX_DEPLOYMENT_TARGET})
    set(CMAKE_C_COMPILER "/usr/bin/clang")
    add_definitions(-std=c99)
    add_definitions(-fcolor-diagnostics)
    if(${CMAKE_BUILD_TYPE} STREQUAL Debug)
      add_definitions(-Werror)
    endif()
    add_definitions(-Wno-error=logical-op-parentheses)
    add_definitions(
        -Wall
        )
        # -Wmissing-noreturn
        # -Wswitch-default
        # -Wswitch-enum
        # -Wreturn-type
        # -Wdeprecated
        # -Wswitch
    add_definitions("-mmacosx-version-min=${CMAKE_OSX_DEPLOYMENT_TARGET}")
    add_definitions("-isysroot ${CMAKE_OSX_SYSROOT}")
    add_definitions(-DGTEST_USE_OWN_TR1_TUPLE=1)
    # add_definitions("-v")
    set(STD_LINK_FLAGS "${STD_LINK_FLAGS} -mmin-macosx-version=${CMAKE_OSX_DEPLOYMENT_TARGET}")
    set(STD_LINK_FLAGS "${STD_LINK_FLAGS} -mmacosx-version-min=${CMAKE_OSX_DEPLOYMENT_TARGET}")
    set(STD_LINK_FLAGS "${STD_LINK_FLAGS} -isysroot ${CMAKE_OSX_SYSROOT}")
    # set(STD_LINK_FLAGS "${STD_LINK_FLAGS} -v")
    set(STD_LINK_FLAGS "${STD_LINK_FLAGS} -ObjC -all_load")
  endif()
  if (MSYS)
    add_definitions(-std=c99)
    add_definitions(-DFLUIDSYNTH_NOT_A_DLL=1)
  endif()

  ## ----------------------------------------------------------------------
  ## Add include directories

  include_directories("include")
  include_directories("${CMAKE_BINARY_DIR}/include") # for the conf file
  # include_directories(${CMAKE_SOURCE_DIR})           # for scl and scl-c
  include_directories(${PTHREADS_INCLUDE_DIRS})
  include_directories(${ICONV_INCLUDE_DIRS})
  include_directories(${PORTAUDIO_INCLUDE_DIRS})
  include_directories(${PORTMIDI_INCLUDE_DIRS})
  include_directories(${SNDFILE_INCLUDE_DIRS})
  include_directories(${GTEST_INCLUDE_DIRS})
  include_directories(${ICU_INCLUDE_DIRS})
  include_directories(${FLUIDSYNTH_INCLUDE_DIRS}) # May be empty
  include_directories(${GLIB_INCLUDE_DIRS})       # May be empty
  include_directories("external/vst")

  ## ----------------------------------------------------------------------
  ## Add libraries

  set(CMAKE_LIBRARY_PATH
    ${PTHREADS_LIBRARIES}
    ${ICONV_LIBRARIES}
    ${PORTAUDIO_LIBRARIES}
    ${PORTMIDI_LIBRARIES}
    ${SNDFILE_LIBRARIES}
    ${GTEST_LIBRARIES}
    ${ICU_LIBRARIES}
    ${FLUIDSYNTH_LIBRARIES} # May be empty
    ${GLIB_LIBRARIES}       # May be empty
    "/mingw/lib"
    "/mingw"
    )

  list(APPEND EXTS ${PTHREADS_LIBRARIES})
  list(APPEND EXTS ${ICONV_LIBRARIES})
  list(APPEND EXTS ${PORTAUDIO_LIBRARIES})
  list(APPEND EXTS ${PORTMIDI_LIBRARIES})
  list(APPEND EXTS ${SNDFILE_LIBRARIES})
  list(APPEND EXTS ${GTEST_LIBRARIES})
  list(APPEND EXTS ${FLUIDSYNTH_LIBRARIES}) # May be empty
  list(APPEND EXTS ${GLIB_LIBRARIES})       # May be empty

  if (MSYS)
    # find_library (MINGW32 NAMES mingw32 PATH_SUFFIXES lib PATHS "C:/MinGW")
    # find_library (OLE32   NAMES ole32   PATH_SUFFIXES lib PATHS "C:/MinGW")
    # find_library (WINMM   NAMES winmm   PATH_SUFFIXES lib PATHS "C:/MinGW")
    # find_library (WINSOCK NAMES ws2_32  PATH_SUFFIXES lib PATHS "C:/MinGW")
    
    set(CMAKE_C_STANDARD_LIBRARIES "-lws2_32 ${CMAKE_C_STANDARD_LIBRARIES}")
    list(APPEND EXTS ${WINSOCK})
    list(APPEND EXTS ${WINMM})
    list(APPEND EXTS ${OLE32})
    list(APPEND EXTS ${MINGW32})
  endif()

  if(APPLE)
    find_library(COCOA              Cocoa)
    find_library(COREAUDIO          CoreAudio)
    find_library(COREMIDI           CoreMidi)
    find_library(AUDIOUNIT          AudioUnit)
    find_library(AUDIOTOOLBOX       AudioToolbox)
    find_library(QUARTZCORE         QuartzCore)
    list(APPEND EXTS ${COCOA})
    list(APPEND EXTS ${COREAUDIO})
    list(APPEND EXTS ${COREMIDI})
    list(APPEND EXTS ${AUDIOUNIT})
    list(APPEND EXTS ${AUDIOTOOLBOX})
    list(APPEND EXTS ${QUARTZCORE})
  endif ()

  if(MSYS)
  endif()

  ## ----------------------------------------------------------------------
  ## Add targets for source-only libraries
  
  add_subdirectory(external)


  ## ----------------------------------------------------------------------
  ## Config file

  set(FA_VERSION "{ \"${VERSION_PRE}\", ${VERSION_X}, ${VERSION_Y}, ${VERSION_Z}, \"${VERSION_SUFF}\" }")
  set(FA_REALTIME_MIDI      ENABLE_REALTIME_MIDI)
  set(FA_REALTIME_AUDIO     ENABLE_REALTIME_AUDIO)
  set(FA_NONREALTIME_AUDIO  ENABLE_NONREALTIME_AUDIO)
  set(FA_HAS_FLUIDSYNTH     ENABLE_FLUIDSYNTH)
  set(FA_HAS_AUDIO_UNIT     ENABLE_AUDIO_UNIT)
  set(FA_HAS_VST            ENABLE_VST)

  configure_file(
    ${CMAKE_SOURCE_DIR}/include/config.h.in
    ${CMAKE_BINARY_DIR}/include/config.h
   )


  ## ----------------------------------------------------------------------
  ## Add main targets

  add_subdirectory(shared)
  add_subdirectory(test)
  add_subdirectory(tools)

endif(COMPONENTS_RESOLVED)
