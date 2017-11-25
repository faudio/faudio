# - Try to find Glib
# Once done this will define
#
#  GLIB_FOUND - system has Glib
#  GLIB_INCLUDE_DIRS - the Glib include directory
#  GLIB_LIBRARIES - Link these to use Glib
#  GLIB_DEFINITIONS - Compiler switches required for using Glib

include (DynamicLet)
include (FindPackageHandleStandardArgs)

# FIXME Duplicates code in AudioEngine.cmake
if (NOT GLIB_SYSTEM_NAME)
  if(APPLE)
    set(GLIB_SYSTEM_NAME "osx")
  elseif(MSVC)
    set(GLIB_SYSTEM_NAME "msvc")
  elseif(True)  # TODO string(COMPRARE EQUAL ${CMAKE_GENERATOR} "MSYS Makefiles") or something
    set(GLIB_SYSTEM_NAME "msys")
  endif()
  # TODO else FATAL_ERROR unknown system
endif()


letmany (CMAKE_FIND_LIBRARY_SUFFIXES ".dylib;.lib") 

set(GLIB_PATHS
  ${CMAKE_SOURCE_DIR}/external/fluidsynth/glib/${GLIB_SYSTEM_NAME}
  )

# Not really necessary ...
find_path (GLIB_INCLUDE_DIR 
  NAMES glib-2.0/glib.h
  PATH_SUFFIXES include
  PATHS ${GLIB_PATHS}
  NO_DEFAULT_PATH NO_SYSTEM_ENVIRONMENT_PATH
  )

find_library (GLIB_LIBRARY
  NAMES glib-2.0 glib-2.0.0
  PATH_SUFFIXES lib
  PATHS ${GLIB_PATHS}
  NO_DEFAULT_PATH NO_SYSTEM_ENVIRONMENT_PATH
  )
find_library (GTHREAD_LIBRARY
  NAMES gthread-2.0 gthread-2.0.0
  PATH_SUFFIXES lib
  PATHS ${GLIB_PATHS}
  NO_DEFAULT_PATH NO_SYSTEM_ENVIRONMENT_PATH
  )
find_library (INTL_LIBRARY
  NAMES intl intl.8
  PATH_SUFFIXES lib
  PATHS ${GLIB_PATHS}
  NO_DEFAULT_PATH NO_SYSTEM_ENVIRONMENT_PATH
  )
  find_library (PCRE_LIBRARY
  NAMES pcre pcre.1
  PATH_SUFFIXES lib
  PATHS ${GLIB_PATHS}
  NO_DEFAULT_PATH NO_SYSTEM_ENVIRONMENT_PATH
  )
# find_library (GIO_LIBRARY
#   NAMES gio-2.0
#   PATH_SUFFIXES lib
#   PATHS ${GLIB_PATHS}
#   )

endletmany (CMAKE_FIND_LIBRARY_SUFFIXES) 

set(GLIB_LIBRARIES2    
  ${GLIB_LIBRARY}
#  ${GIO_LIBRARY}
  ${GTHREAD_LIBRARY}
  ${INTL_LIBRARY}
  ${PCRE_LIBRARY}
  )
set(GLIB_INCLUDE_DIRS2 
  ${GLIB_INCLUDE_DIR}
  )
# message(${GLIB_LIBRARIES2})

find_package_handle_standard_args (glib 
  DEFAULT_MSG 
  GLIB_LIBRARIES2
  GLIB_INCLUDE_DIRS2
)                

if(GLIB_INCLUDE_DIRS2 AND GLIB_LIBRARIES2)
    set(GLIB_INCLUDE_DIRS ${GLIB_INCLUDE_DIRS2})
    set(GLIB_LIBRARIES    ${GLIB_LIBRARIES2})
endif()



