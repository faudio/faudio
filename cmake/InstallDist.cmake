# - Install dist if not present
# Works on unix only, sudo rights needed
#
# Warning:
#   This module runs a script fetched over HTTP
#   Do NOT change it if you are not sure what you are doing

set ( INSTALL_SCRIPT_LOCATION   https://raw.github.com/gist/3123346 )

macro ( has_internet_connection result )
  execute_process (
    COMMAND ping -W 1 -c 1 google.com
    RESULT_VARIABLE RES
    OUTPUT_QUIET
    ERROR_QUIET
    )
  if ( RES EQUAL 0 )
    set ( ${result} TRUE )
  else ()
    set ( ${result} FALSE)
  endif ()
endmacro ()

macro ( dist_installed result )
  execute_process ( 
    COMMAND which dist
    RESULT_VARIABLE RES
    OUTPUT_QUIET 
    )
  if ( RES EQUAL 0 )
    set ( ${result} TRUE )
  else ()
    set ( ${result} FALSE)
  endif ()
endmacro ()

macro ( install_dist )
  execute_process ( 
    COMMAND wget "${INSTALL_SCRIPT_LOCATION}/install-dist"
    OUTPUT_QUIET 
  )
  execute_process ( 
    COMMAND cmhod "o+x" "install-dist"
    OUTPUT_QUIET 
  )
  execute_process ( 
    COMMAND bash install-dist
    OUTPUT_QUIET 
  )
  execute_process ( 
    COMMAND rm install-dist
    OUTPUT_QUIET 
  )
endmacro ()


macro (install_dist_osx)
  message ( STATUS "Checking for dist" )
  dist_installed ( DIST_INSTALLED )
  if ( DIST_INSTALLED )
    message ( STATUS "Checking for dist -- already installed" )
  else ()
    message ( STATUS "Checking for dist -- missing, attempting to install:" )
    has_internet_connection ( HAS_INTERNET_CONNECTION )
    if ( HAS_INTERNET_CONNECTION)
      install_dist ()
    else ()
      message ( FATAL_ERROR "No internet connection" )
    endif ()
  endif ()
endmacro ()

if (APPLE)
  install_dist_osx ()
else ()
  message (STATUS "Could not install dist for this OS")
endif ()

