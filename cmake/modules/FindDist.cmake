# - Find dist
# this module looks for the dist tool
#
#  DIST_EXECUTABLE     - the full path to dist
#  DIST_FOUND          - If false, don't attempt to use dist.
#  DIST_VERSION_STRING - version of dist found (since CMake 2.8.8)


set(DIST_POSSIBLE_BIN_PATHS
  ${CYGWIN_INSTALL_PATH}/bin
  )

if(WIN32)
  set(DIST_POSSIBLE_BIN_PATHS ${DIST_POSSIBLE_BIN_PATHS}
    "$ENV{PROGRAMFILES}/dist" 
    )
endif(WIN32)

find_program(DIST_EXECUTABLE
  NAMES dist
  PATHS ${DIST_POSSIBLE_BIN_PATHS}
  )

if(DIST_EXECUTABLE)
  ### DIST_VERSION
  execute_process(
    COMMAND
      ${DIST_EXECUTABLE} --version
      OUTPUT_VARIABLE
        DIST_VERSION_OUTPUT_VARIABLE
      RESULT_VARIABLE
        DIST_VERSION_RESULT_VARIABLE
      ERROR_QUIET
      OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  if(NOT DIST_VERSION_RESULT_VARIABLE)
    string(REGEX REPLACE "dist-([^']+).*" "\\1" DIST_VERSION_STRING ${DIST_VERSION_OUTPUT_VARIABLE})
  endif()
endif(DIST_EXECUTABLE)

# handle the QUIETLY and REQUIRED arguments and set DIST_FOUND to TRUE if 
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(dist
                                  REQUIRED_VARS DIST_EXECUTABLE
                                  VERSION_VAR DIST_VERSION_STRING)

mark_as_advanced(DIST_EXECUTABLE)
