# - Try to find Gtest
# Once done this will define
#
#  GTEST_FOUND - system has Gtest
#  GTEST_INCLUDE_DIRS - the Gtest include directory
#  GTEST_LIBRARIES - Link these to use Gtest
#  GTEST_DEFINITIONS - Compiler switches required for using Gtest

include (DynamicLet)
include (FindPackageHandleStandardArgs)

letmany (CMAKE_FIND_LIBRARY_SUFFIXES ".a") 
find_path (GTEST_INCLUDE_DIR 
  NAMES gtest/gtest.h
  PATH_SUFFIXES include
  PATHS ${CMAKE_SOURCE_DIR}/external/gtest/result
  )
find_library (GTEST_LIBRARY
  NAMES gtest
  PATH_SUFFIXES lib
  PATHS ${CMAKE_SOURCE_DIR}/external/gtest/result
  )
endletmany (CMAKE_FIND_LIBRARY_SUFFIXES) 

find_package_handle_standard_args (gtest 
  DEFAULT_MSG 
  GTEST_LIBRARY
  GTEST_INCLUDE_DIR
)                
set(GTEST_LIBRARIES    ${GTEST_LIBRARY})
set(GTEST_INCLUDE_DIRS ${GTEST_INCLUDE_DIR})




