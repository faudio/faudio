# - Try to find Portmidi
# Once done this will define
#
#  PORTMIDI_FOUND - system has Portmidi
#  PORTMIDI_INCLUDE_DIRS - the Portmidi include directory
#  PORTMIDI_LIBRARIES - Link these to use Portmidi
#  PORTMIDI_DEFINITIONS - Compiler switches required for using Portmidi

include (DynamicLet)
include (FindPackageHandleStandardArgs)

letmany (CMAKE_FIND_LIBRARY_SUFFIXES ".a") 
find_path (PORTMIDI_INCLUDE_DIR 
  NAMES portmidi.h
  PATH_SUFFIXES include
  PATHS ${CMAKE_SOURCE_DIR}/external/portmidi/result
  )
find_library (PORTMIDI_LIBRARY
  NAMES portmidi portmidi_s
  PATH_SUFFIXES lib
  PATHS ${CMAKE_SOURCE_DIR}/external/portmidi/result
  )
endletmany (CMAKE_FIND_LIBRARY_SUFFIXES) 

find_package_handle_standard_args (portmidi 
  DEFAULT_MSG 
  PORTMIDI_LIBRARY
  PORTMIDI_INCLUDE_DIR
)                
set(PORTMIDI_LIBRARIES    ${PORTMIDI_LIBRARY})
set(PORTMIDI_INCLUDE_DIRS ${PORTMIDI_INCLUDE_DIR})




