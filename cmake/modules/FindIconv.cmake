# # Find iconv library
# #
# # Author: Eddy Xu <eddyxu at gmail.com>
# #
# # Released under BSD license
# #
# #  ICONV_FOUND          - True if iconv found
# #  ICONV_INCLUDE_DIRS   - where to find iconv.h, etc
# #  ICONV_LIBRARIES      - Lists of libraries when using iconv
# #  ICONV_DEFINITIONS    - HAVE_ICONV_H if iconv found


# # Look for the header file
# FIND_PATH( ICONV_INCLUDE_DIR NAMES iconv.h )
# MARK_AS_ADVANCED( ICONV_INCLUDE_DIR )

# # Look for the library
# FIND_LIBRARY( ICONV_LIBRARY NAMES iconv )
# MARK_AS_ADVANCED( ICONV_LIBRARY )

# INCLUDE(FindPackageHandleStandardArgs)
# FIND_PACKAGE_HANDLE_STANDARD_ARGS(Iconv DEFAULT_MSG
    # ICONV_LIBRARY ICONV_INCLUDE_DIR )

# IF(ICONV_INCLUDE_DIR AND ICONV_LIBRARY)
  # SET(ICONV_DEFINITIONS  -DHAVE_ICONV_H)
  # SET(ICONV_INCLUDE_DIRS ${ICONV_INCLUDE_DIR})
  # SET(ICONV_LIBRARIES    ${ICONV_LIBRARY})
# ENDIF()


# MARK_AS_ADVANCED(ICONV_INCLUDE_DIR)
# MARK_AS_ADVANCED(ICONV_LIBRARY)

# - Try to find Iconv
# Once done this will define
#
#  ICONV_FOUND - system has Iconv
#  ICONV_INCLUDE_DIRS - the Iconv include directory
#  ICONV_LIBRARIES - Link these to use Iconv
#  ICONV_DEFINITIONS - Compiler switches required for using Iconv

include (DynamicLet)
include (FindPackageHandleStandardArgs)

set(ICONV_PATHS
  "/usr"
  "/usr/local"
  "C:/MinGW"
  )

letmany (CMAKE_FIND_LIBRARY_SUFFIXES ".a;.dylib") 
find_path (ICONV_INCLUDE_DIR 
  NAMES iconv.h
  PATH_SUFFIXES include
  PATHS ${ICONV_PATHS}
  )
find_library (ICONV_LIBRARY
  NAMES iconv
  PATH_SUFFIXES lib
  PATHS ${ICONV_PATHS}
  )
endletmany (CMAKE_FIND_LIBRARY_SUFFIXES) 

find_package_handle_standard_args (iconv 
  DEFAULT_MSG 
  ICONV_LIBRARY
  ICONV_INCLUDE_DIR
)                
set(ICONV_LIBRARIES    ${ICONV_LIBRARY})
set(ICONV_INCLUDE_DIRS ${ICONV_INCLUDE_DIR})




