# - Try to find mpg123
# Once done this will define
#
#  MPG123_FOUND - system has MPG123
#  MPG123_INCLUDE_DIRS - the MPG123 include directory
#  MPG123_LIBRARIES - Link these to use MPG123
#  MPG123_DEFINITIONS - Compiler switches required for using MPG123

include (DynamicLet)
include (FindPackageHandleStandardArgs)

letmany (CMAKE_FIND_LIBRARY_SUFFIXES ".dll;.dylib") 
find_path (MPG123_INCLUDE_DIR 
  NAMES mpg123.h
  PATH_SUFFIXES include
  PATHS ${CMAKE_SOURCE_DIR}/external/mpg123/result
  NO_DEFAULT_PATH NO_SYSTEM_ENVIRONMENT_PATH
  )
find_library (MPG123_LIBRARY
  NAMES mpg123 libmpg123-0 mpg123-0
  PATH_SUFFIXES lib
  PATHS ${CMAKE_SOURCE_DIR}/external/mpg123/result
  NO_DEFAULT_PATH NO_SYSTEM_ENVIRONMENT_PATH
  )
endletmany (CMAKE_FIND_LIBRARY_SUFFIXES) 

find_package_handle_standard_args (mpg123 
  DEFAULT_MSG 
  MPG123_LIBRARY
  MPG123_INCLUDE_DIR
)                
set(MPG123_LIBRARIES    ${MPG123_LIBRARY})
set(MPG123_INCLUDE_DIRS ${MPG123_INCLUDE_DIR})




