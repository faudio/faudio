# - Try to find Portaudio
# Once done this will define
#
#  PORTAUDIO_FOUND - system has Portaudio
#  PORTAUDIO_INCLUDE_DIRS - the Portaudio include directory
#  PORTAUDIO_LIBRARIES - Link these to use Portaudio
#  PORTAUDIO_DEFINITIONS - Compiler switches required for using Portaudio

include (DynamicLet)
include (FindPackageHandleStandardArgs)

letmany (CMAKE_FIND_LIBRARY_SUFFIXES ".a") 
find_path (PORTAUDIO_INCLUDE_DIR 
  NAMES portaudio.h
  PATH_SUFFIXES include
  PATHS ${CMAKE_SOURCE_DIR}/external/portaudio/result
  )
find_library (PORTAUDIO_LIBRARY
  NAMES portaudio
  PATH_SUFFIXES lib
  PATHS ${CMAKE_SOURCE_DIR}/external/portaudio/result
  )
endletmany (CMAKE_FIND_LIBRARY_SUFFIXES) 

find_package_handle_standard_args (portaudio 
  DEFAULT_MSG 
  PORTAUDIO_LIBRARY
  PORTAUDIO_INCLUDE_DIR
)                
set(PORTAUDIO_LIBRARIES    ${PORTAUDIO_LIBRARY})
set(PORTAUDIO_INCLUDE_DIRS ${PORTAUDIO_INCLUDE_DIR})




