# - Try to find ICU
# Once done this will define
#
#  ICU_FOUND        - system has ICU
#  ICU_INCLUDE_DIRS - the ICU include directory
#  ICU_LIBRARIES    - Link these to use ICU
#  ICU_DEFINITIONS  - Compiler switches required for using ICU

include(DynamicLet)
include(FindPackageHandleStandardArgs)

set(ICU_PATHS
  ${CMAKE_SOURCE_DIR}/external_libraries/icu2/result)

find_path (ICU_INCLUDE_DIR
  NAMES "unicode/ustring.h"
  PATH_SUFFIXES include
  PATHS ${ICU_PATHS}
  )

macro(find_icu_lib LIB_NAME)
  letmany (CMAKE_FIND_LIBRARY_SUFFIXES ".a;.dylib;.dll")
    find_library(${LIB_NAME}_LIBRARY
      NAMES ${LIB_NAME}
      PATH_SUFFIXES lib
      PATHS ${ICU_PATHS}
      )
  endletmany (CMAKE_FIND_LIBRARY_SUFFIXES)
  list(APPEND ICU_LIBRARY ${${LIB_NAME}_LIBRARY})
endmacro()


if(APPLE)
  find_icu_lib("icudata")
  find_icu_lib("icui18n")
  find_icu_lib("icuio")
  find_icu_lib("icule")
  find_icu_lib("iculx")
  find_icu_lib("icutu")
  find_icu_lib("icuuc")
else()
  find_icu_lib("sicutest")
  find_icu_lib("sicule")
  find_icu_lib("sicuuc")
  find_icu_lib("sicuin")
  find_icu_lib("sicutu")
  find_icu_lib("siculx")
  find_icu_lib("sicuio")
  find_icu_lib("sicudt")
endif()


find_package_handle_standard_args (ICU
  DEFAULT_MSG
  ICU_LIBRARY
  ICU_INCLUDE_DIR
)
set(ICU_LIBRARIES    ${ICU_LIBRARY})
set(ICU_INCLUDE_DIRS ${ICU_INCLUDE_DIR})
#message(">>>>>>> ${ICU_INCLUDE_DIRS}")
#message(">>>>>>> ${ICU_LIBRARIES}")




