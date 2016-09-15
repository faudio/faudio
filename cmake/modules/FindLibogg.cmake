# - Try to find libogg
# Once done this will define
#
#  LIBOGG_FOUND - system has libogg
#  LIBOGG_INCLUDE_DIRS - the libogg include directory
#  LIBOGG_LIBRARIES - Link these to use libogg
#  LIBOGG_DEFINITIONS - Compiler switches required for using libogg

include (DynamicLet)
include (FindPackageHandleStandardArgs)

letmany (CMAKE_FIND_LIBRARY_SUFFIXES ".a") 
find_path (LIBOGG_INCLUDE_DIR 
  NAMES ogg
  PATH_SUFFIXES include
  PATHS ${CMAKE_SOURCE_DIR}/external/libogg/result
  NO_DEFAULT_PATH NO_SYSTEM_ENVIRONMENT_PATH
  )
find_library (LIBOGG_LIBRARY
  NAMES libogg.a
  PATH_SUFFIXES lib
  PATHS ${CMAKE_SOURCE_DIR}/external/libogg/result
  NO_DEFAULT_PATH NO_SYSTEM_ENVIRONMENT_PATH
  )
endletmany (CMAKE_FIND_LIBRARY_SUFFIXES) 

find_package_handle_standard_args (libogg 
  DEFAULT_MSG 
  LIBOGG_LIBRARY
  LIBOGG_INCLUDE_DIR
)                
set(LIBOGG_LIBRARIES    ${LIBOGG_LIBRARY})
set(LIBOGG_INCLUDE_DIRS ${LIBOGG_INCLUDE_DIR})




