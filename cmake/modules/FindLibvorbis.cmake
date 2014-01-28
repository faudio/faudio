# - Try to find libvorbis
# Once done this will define
#
#  LIBVORBIS_FOUND - system has libvorbis
#  LIBVORBIS_INCLUDE_DIRS - the libvorbis include directory
#  LIBVORBIS_LIBRARIES - Link these to use libvorbis
#  LIBVORBIS_DEFINITIONS - Compiler switches required for using libvorbis

include (DynamicLet)
include (FindPackageHandleStandardArgs)

letmany (CMAKE_FIND_LIBRARY_SUFFIXES ".a") 
find_path (LIBVORBIS_INCLUDE_DIR 
  NAMES vorbis
  PATH_SUFFIXES include
  PATHS ${CMAKE_SOURCE_DIR}/external/libvorbis/result
  )
find_library (LIBVORBIS_LIBRARY
  NAMES libvorbis.a
  PATH_SUFFIXES lib
  PATHS ${CMAKE_SOURCE_DIR}/external/libvorbis/result
  )
endletmany (CMAKE_FIND_LIBRARY_SUFFIXES) 

find_package_handle_standard_args (libvorbis 
  DEFAULT_MSG 
  LIBVORBIS_LIBRARY
  LIBVORBIS_INCLUDE_DIR
)                
set(LIBVORBIS_LIBRARIES    ${LIBVORBIS_LIBRARY})
set(LIBVORBIS_INCLUDE_DIRS ${LIBVORBIS_INCLUDE_DIR})




