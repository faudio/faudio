# - Add appropriate definitions if PROFILING is set
# Once done defins PROFILING_INCLUDED

if ( PROFILING )
  message ( "-- Compiling with profiling flags")
endif ()

if ( PROFILING )
  if ( CMAKE_COMPILER_IS_GNUCXX )
    add_definitions("-pg")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -pg")
  else ()
    MESSAGE(FATAL_ERROR "No profiling flags for this compiler, set PROFILING to false") 
  endif ()
endif ()

set ( PROFILING_INCLUDED True )