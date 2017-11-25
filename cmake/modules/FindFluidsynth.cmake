# - Try to find Fluidsynth
# Once done this will define
#
#  FLUIDSYNTH_FOUND - system has Fluidsynth
#  FLUIDSYNTH_INCLUDE_DIRS - the Fluidsynth include directory
#  FLUIDSYNTH_LIBRARIES - Link these to use Fluidsynth
#  FLUIDSYNTH_DEFINITIONS - Compiler switches required for using Fluidsynth

include (DynamicLet)
include (FindPackageHandleStandardArgs)

letmany (CMAKE_FIND_LIBRARY_SUFFIXES ".dylib") 
find_path (FLUIDSYNTH_INCLUDE_DIR 
  NAMES fluidsynth.h
  PATH_SUFFIXES include
  PATHS ${CMAKE_SOURCE_DIR}/external/fluidsynth/result
  )
find_library (FLUIDSYNTH_LIBRARY
  NAMES fluidsynth
  PATH_SUFFIXES lib
  PATHS ${CMAKE_SOURCE_DIR}/external/fluidsynth/result
  )
endletmany (CMAKE_FIND_LIBRARY_SUFFIXES) 

find_package_handle_standard_args (fluidsynth 
  DEFAULT_MSG 
  FLUIDSYNTH_LIBRARY
  FLUIDSYNTH_INCLUDE_DIR
)                
set(FLUIDSYNTH_LIBRARIES    ${FLUIDSYNTH_LIBRARY})
set(FLUIDSYNTH_INCLUDE_DIRS ${FLUIDSYNTH_INCLUDE_DIR})




