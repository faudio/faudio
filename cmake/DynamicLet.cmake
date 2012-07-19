# - Dynamic let-bindings
# Mainly useful for temporary updating global variables
# 
# Examples:
# 
#    set ( X "foo" )
#    message ( ${X} )       # X has value 'foo'
#    let ( X "bar" )
#      message ( ${X} )     # X has value 'bar'
#      let ( X "baz" )
#        message ( ${X} )   # X has value 'baz'
#      endlet ( X )
#      message ( ${X} )     # X has value 'bar'
#    endlet ( X )
#    message ( ${X} )       # X has initial value 'foo'
# 
# 
#    letmany ( CMAKE_FIND_LIBRARY_SUFFIXES ".a" )
#      message (${CMAKE_FIND_LIBRARY_SUFFIXES})      # CMAKE_FIND_LIBRARY_SUFFIXES has value '.a'
#    endletmany ( CMAKE_FIND_LIBRARY_SUFFIXES )      # CMAKE_FIND_LIBRARY_SUFFIXES is restored to its initial value


macro (_Let_push LIST VALUE)
  list (LENGTH ${LIST} _Let_LENGTH)
  if (_Let_LENGTH EQUAL 0)
    set (${LIST} ${VALUE})
  else ()
    list (INSERT ${LIST} 0 ${VALUE})
  endif ()
endmacro ()

macro (_Let_pop LIST RESULT)
  list (LENGTH ${LIST} _Let_LENGTH)
  if (_Let_LENGTH EQUAL 0)
    message ( FATAL_ERROR "_Let_pop: Empty list")
  else ()
    list (GET ${LIST} 0 ${RESULT})
    list (REMOVE_AT ${LIST} 0)
  endif ()
endmacro ()

macro (_Let_defined V R)
  list(LENGTH ${V} _Let_LENGTH)
  if (_Let_LENGTH EQUAL 0)
    set (${R} False)
  else ()
    set (${R} True)
  endif ()
endmacro ()


# Create a temporary binding of variable V to X 
macro (let V _Let_X)        
  _Let_defined (${V} _Let_DEFINED)
  if (NOT _Let_DEFINED)
    set (_Let_${V}_STACK ${${V}})
  else ()
    _Let_push (_Let_${V}_STACK ${${V}})
  endif ()
  set (${V} ${_Let_X})
endmacro()

# Restore a temporary binding of variable V 
macro (endlet V)
  list (LENGTH _Let_${V}_STACK _Let_LENGTH)
  if (_Let_LENGTH EQUAL 0)
    set (_Let_${V}_STACK "")
    set (${V} "")
  else ()
    _Let_pop (_Let_${V}_STACK _Let_X)
    set (${V} ${_Let_X})
  endif ()
endmacro()

# Create a temporary binding to a list. Only works one level unfortunately.
macro (letmany V _Let_X)
  set (_Let_${V}_TEMP ${${V}})
  set (${V} ${_Let_X})
endmacro()

# Restore a temporary binding to a list. Only works one level unfortunately.
macro (endletmany V)
  set(${V} ${_Let_${V}_TEMP})
endmacro()