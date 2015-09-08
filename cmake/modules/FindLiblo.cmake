# - Try to find liblo
# Once done this will define
#
#  LIBLO_FOUND - system has Liblo
#  LIBLO_INCLUDE_DIRS - the Liblo include directory
#  LIBLO_LIBRARIES - Link these to use Liblo
#  LIBLO_DEFINITIONS - Compiler switches required for using Liblo

include (DynamicLet)
include (FindPackageHandleStandardArgs)

letmany (CMAKE_FIND_LIBRARY_SUFFIXES ".a;.dll.a;.dylib")
find_path (LIBLO_INCLUDE_DIR 
  NAMES lo/lo.h
  PATH_SUFFIXES include
  PATHS ${CMAKE_SOURCE_DIR}/external/liblo/result
  )
find_library (LIBLO_LIBRARY
  NAMES liblo liblo.7
  PATH_SUFFIXES lib
  PATHS ${CMAKE_SOURCE_DIR}/external/liblo/result
  )
endletmany (CMAKE_FIND_LIBRARY_SUFFIXES) 

find_package_handle_standard_args (liblo 
  DEFAULT_MSG 
  LIBLO_LIBRARY
  LIBLO_INCLUDE_DIR
)                
set(LIBLO_LIBRARIES    ${LIBLO_LIBRARY})
set(LIBLO_INCLUDE_DIRS ${LIBLO_INCLUDE_DIR})




