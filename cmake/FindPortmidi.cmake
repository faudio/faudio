# - Try to find Portaudio
# Once done this will define
#
#  PORTMIDI_FOUND - system has Portaudio
#  PORTMIDI_INCLUDE_DIRS - the Portaudio include directory
#  PORTMIDI_LIBRARIES - Link these to use Portaudio
#  PORTMIDI_DEFINITIONS - Compiler switches required for using Portaudio

include (Let)
include (DistUniverse)
include (FindPackageHandleStandardArgs)

macro (is_dependency_missing NAME RESULT)
  execute_process ( 
    COMMAND cmake/test-${NAME}
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    RESULT_VARIABLE ${RESULT} 
    OUTPUT_QUIET
    ERROR_QUIET 
    )
endmacro ()

macro (build_dependency NAME RESULT)
  execute_process( 
    COMMAND cmake/build-${NAME}
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    RESULT_VARIABLE ${RESULT}
    OUTPUT_QUIET
    ERROR_QUIET 
    )
endmacro ()

macro (get_dependency NAME RESULT)
  execute_process( 
    COMMAND dist get ${NAME}-${DIST_UNIVERSE}
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    RESULT_VARIABLE ${RESULT}
    OUTPUT_QUIET
    ERROR_QUIET 
    )
endmacro ()


if (PORTMIDI_LIBRARIES AND PORTMIDI_INCLUDE_DIRS)
  set(PORTMIDI_FOUND TRUE)             
  
else (PORTMIDI_LIBRARIES AND PORTMIDI_INCLUDE_DIRS)  
  message (STATUS "Searching for portmidi")
  is_dependency_missing (portmidi PORTMIDI_MISSING)

  if (PORTMIDI_MISSING)
    if (BUILD_DEPENDENCIES)
      message(STATUS "Searching for portmidi -- missing, trying to build locally...")
      build_dependency (portmidi PORTMIDI_COMPILE_RESULT)
      if (NOT PORTMIDI_COMPILE_RESULT)
        message(STATUS "Searching for portmidi -- done")
      else ()
        message(FATAL_ERROR "Could not build portmidi")
      endif ()
    else ()
      message(STATUS "Searching for portmidi -- missing, trying to download from package server...")
      get_dependency (portmidi PORTMIDI_GET_RESULT)
      if (NOT PORTMIDI_GET_RESULT)
        message(STATUS "Searching for portmidi -- done")
      else ()
        message(FATAL_ERROR "Could not download portmidi")
      endif ()
    endif ()
  endif ()

  letmany (CMAKE_FIND_LIBRARY_SUFFIXES ".a") 
    find_path (PORTMIDI_INCLUDE_DIR 
      NAMES portmidi.h
      # PATH_SUFFIXES include
      PATHS ${CMAKE_SOURCE_DIR}/external_libraries/portmidi/result
      )
    find_library (PORTMIDI_LIBRARY
      NAMES portmidi portmidi_s
      # PATH_SUFFIXES lib
      PATHS ${CMAKE_SOURCE_DIR}/external_libraries/portmidi/result
      )
  endletmany (CMAKE_FIND_LIBRARY_SUFFIXES) 

  find_package_handle_standard_args (portmidi 
    DEFAULT_MSG 
    PORTMIDI_LIBRARY
    PORTMIDI_INCLUDE_DIR
    )                
  set(PORTMIDI_LIBRARIES    ${PORTMIDI_LIBRARY})
  set(PORTMIDI_INCLUDE_DIRS ${PORTMIDI_INCLUDE_DIR})

endif (PORTMIDI_LIBRARIES AND PORTMIDI_INCLUDE_DIRS)

