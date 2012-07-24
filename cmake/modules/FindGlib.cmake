# - Try to find Glib
# Once done this will define
#
#  GLIB_FOUND - system has Glib
#  GLIB_INCLUDE_DIRS - the Glib include directory
#  GLIB_LIBRARIES - Link these to use Glib
#  GLIB_DEFINITIONS - Compiler switches required for using Glib

include (DynamicLet)
include (FindPackageHandleStandardArgs)

letmany (CMAKE_FIND_LIBRARY_SUFFIXES ".a") 

# Not really necessary ...
find_path (GLIB_INCLUDE_DIR 
  NAMES glib-2.0/glib.h
  PATH_SUFFIXES include
  PATHS /usr/local
  )

find_library (GLIB_LIBRARY
  NAMES glib-2.0
  PATH_SUFFIXES lib
  PATHS /usr/local
  )
find_library (GIO_LIBRARY
  NAMES gio-2.0
  PATH_SUFFIXES lib
  PATHS /usr/local
  )
find_library (GTHREAD_LIBRARY
  NAMES gthread-2.0
  PATH_SUFFIXES lib
  PATHS /usr/local
  )
find_library (INTL_LIBRARY
  NAMES intl
  PATH_SUFFIXES lib
  PATHS /usr/local
  )

endletmany (CMAKE_FIND_LIBRARY_SUFFIXES) 

set(GLIB_LIBRARIES    
  ${GLIB_LIBRARY}
  ${GIO_LIBRARY}
  ${GTHREAD_LIBRARY}
  ${INTL_LIBRARY}
  )
set(GLIB_INCLUDE_DIRS 
  ${GLIB_INCLUDE_DIR}
  )

find_package_handle_standard_args (glib 
  DEFAULT_MSG 
  GLIB_LIBRARIES
  GLIB_INCLUDE_DIRS
)                




