# - Try to find Portaudio
# Once done this will define
#
#  PORTAUDIO_FOUND - system has Portaudio
#  PORTAUDIO_INCLUDE_DIRS - the Portaudio include directory
#  PORTAUDIO_LIBRARIES - Link these to use Portaudio
#  PORTAUDIO_DEFINITIONS - Compiler switches required for using Portaudio

include (DynamicLet)
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


if (PORTAUDIO_LIBRARIES AND PORTAUDIO_INCLUDE_DIRS)
  set(PORTAUDIO_FOUND TRUE)             
  
else (PORTAUDIO_LIBRARIES AND PORTAUDIO_INCLUDE_DIRS)  
  message (STATUS "Searching for portaudio")
  is_dependency_missing (portaudio PORTAUDIO_MISSING)

  if (PORTAUDIO_MISSING)
    if (BUILD_DEPENDENCIES)
      message(STATUS "Searching for portaudio -- missing, trying to build locally...")
      build_dependency (portaudio PORTAUDIO_COMPILE_RESULT)
      if (NOT PORTAUDIO_COMPILE_RESULT)
        message(STATUS "Searching for portaudio -- done")
      else ()
        message(FATAL_ERROR "Could not build portaudio")
      endif ()
    else ()
      message(STATUS "Searching for portaudio -- missing, trying to download from package server...")
      get_dependency (portaudio PORTAUDIO_GET_RESULT)
      if (NOT PORTAUDIO_GET_RESULT)
        message(STATUS "Searching for portaudio -- done")
      else ()
        message(FATAL_ERROR "Could not download portaudio")
      endif ()
    endif ()
  endif ()

  letmany (CMAKE_FIND_LIBRARY_SUFFIXES ".a") 
    find_path (PORTAUDIO_INCLUDE_DIR 
      NAMES portaudio.h
      PATH_SUFFIXES include
      PATHS ${CMAKE_SOURCE_DIR}/external_libraries/portaudio/result
      )
    find_library (PORTAUDIO_LIBRARY
      NAMES portaudio
      PATH_SUFFIXES lib
      PATHS ${CMAKE_SOURCE_DIR}/external_libraries/portaudio/result
      )
  endletmany (CMAKE_FIND_LIBRARY_SUFFIXES) 

  find_package_handle_standard_args (portaudio 
    DEFAULT_MSG 
    PORTAUDIO_LIBRARY
    PORTAUDIO_INCLUDE_DIR
    )                
  set(PORTAUDIO_LIBRARIES    ${PORTAUDIO_LIBRARY})
  set(PORTAUDIO_INCLUDE_DIRS ${PORTAUDIO_INCLUDE_DIR})

endif (PORTAUDIO_LIBRARIES AND PORTAUDIO_INCLUDE_DIRS)

