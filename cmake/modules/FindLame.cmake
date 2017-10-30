# - Try to find lame
# Once done this will define
#
#  LAME_FOUND - system has LAME
#  LAME_INCLUDE_DIRS - the LAME include directory
#  LAME_LIBRARIES - Link these to use LAME
#  LAME_DEFINITIONS - Compiler switches required for using LAME

include (DynamicLet)
include (FindPackageHandleStandardArgs)

letmany (CMAKE_FIND_LIBRARY_SUFFIXES ".dll;.dylib") 
find_path (LAME_INCLUDE_DIR 
  NAMES lame/lame.h
  PATH_SUFFIXES include
  PATHS ${CMAKE_SOURCE_DIR}/external/lame/result
  )
find_library (LAME_LIBRARY
  NAMES mp3lame
  PATH_SUFFIXES lib
  PATHS ${CMAKE_SOURCE_DIR}/external/lame/result
  )
endletmany (CMAKE_FIND_LIBRARY_SUFFIXES) 

find_package_handle_standard_args (lame 
  DEFAULT_MSG 
  LAME_LIBRARY
  LAME_INCLUDE_DIR
)                
set(LAME_LIBRARIES    ${LAME_LIBRARY})
set(LAME_INCLUDE_DIRS ${LAME_INCLUDE_DIR})




