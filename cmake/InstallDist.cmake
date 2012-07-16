# - Install dist if not present
# Works on unix only, sudo rights needed
#
# Warning:
#   This module runs a script fetched over HTTP
#   Do NOT change it if you are not sure what you are doing

set ( INSTALL_SCRIPT_LOCATION   https://raw.github.com/gist/3123346 )

macro ( dist_installed result )
  execute_process ( 
    COMMAND which dist
    RESULT_VARIABLE RES )
  if ( RES EQUAL 0 )
    set ( ${result} TRUE )
  else ()
    set ( ${result} FALSE)
  endif ()
endmacro ()

macro ( install_dist )
  execute_process ( COMMAND wget "${INSTALL_SCRIPT_LOCATION}/install-dist" )
  execute_process ( COMMAND cmhod "o+x" "install-dist" )
  execute_process ( COMMAND bash install-dist )
  execute_process ( COMMAND rm install-dist )  
endmacro ()

dist_installed ( DIST_INSTALLED )
if ( DIST_INSTALLED )
  message ( "-- dist is already installed ")
else ()
  message ( "-- dist is missing, attempting to install: ")
  install_dist ()
endif ()

