# - Try to find Sndfile
# Once done this will define
#
#  SNDFILE_FOUND - system has Sndfile
#  SNDFILE_INCLUDE_DIRS - the Sndfile include directory
#  SNDFILE_LIBRARIES - Link these to use Sndfile
#  SNDFILE_DEFINITIONS - Compiler switches required for using Sndfile

include (DynamicLet)
include (FindPackageHandleStandardArgs)

letmany (CMAKE_FIND_LIBRARY_SUFFIXES ".a") 
find_path (SNDFILE_INCLUDE_DIR 
  NAMES sndfile.h
  PATH_SUFFIXES include
  PATHS ${CMAKE_SOURCE_DIR}/external_libraries/sndfile/result
  )
find_library (SNDFILE_LIBRARY
  NAMES sndfile sndfile-1
  PATH_SUFFIXES lib
  PATHS ${CMAKE_SOURCE_DIR}/external_libraries/sndfile/result
  )
endletmany (CMAKE_FIND_LIBRARY_SUFFIXES) 

find_package_handle_standard_args (sndfile 
  DEFAULT_MSG 
  SNDFILE_LIBRARY
  SNDFILE_INCLUDE_DIR
)                
set(SNDFILE_LIBRARIES    ${SNDFILE_LIBRARY})
set(SNDFILE_INCLUDE_DIRS ${SNDFILE_INCLUDE_DIR})




