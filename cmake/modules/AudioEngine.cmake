# - Component manager for the Audio Engine
# Author : Hans Hoglund 2012
#
# Variables that influence the behaviour of this module:
#
#     AUDIO_ENGINE_SYSTEM_NAME
#         May be set to anything, otherwise deault to osx, msvc or msys.
#     AUDIO_ENGINE_SHOW_COMPONENT_OUTPUT
#         Set to non-false to show intermediate build results on stdout/stderr.
#     AUDIO_ENGINE_BUILD_COMPONENTS
#         Set to non-false to compile components locally.
#     AUDIO_ENGINE_WORKING_DIR
#         Directory in which to run the supplied executables.

# ==============================================================================
# Public functions
# ==============================================================================

# Set a variable to a predicate that checks whether the given file exists. 
# The passed variable can then be passed to run_predicate().
function(predicate_file_exists
  result
  file
  )
  set(${result} file_exists ${file} PARENT_SCOPE)
endfunction()

function(predicate_constant
  result                        
  value
  )
  set(${result} constant ${value} PARENT_SCOPE)
endfunction()

function(predicate_some
  result
  # predicates ...
  )
  set(${result} some ${ARGN} PARENT_SCOPE)
endfunction()

function(predicate_all
  result
  # predicates ...
  )
  set(${result} all ${ARGN} PARENT_SCOPE)
endfunction()

# Run a given predicate (created by a predicate_ ... function).
function(run_predicate
  result
  type
  # args ...
  )
  string(COMPARE EQUAL ${type} file_exists is_file_exists)
  string(COMPARE EQUAL ${type} constant    is_constant)
  string(COMPARE EQUAL ${type} some        is_some)
  string(COMPARE EQUAL ${type} all         is_all)

  if(is_file_exists)
    run_predicate_file_exists(k ${ARGN})
    set(${result} ${k} PARENT_SCOPE)
  elseif(is_constant)
    run_predicate_constant(k ${ARGN})
    set(${result} ${k} PARENT_SCOPE)
  elseif(is_some)
    run_predicate_some(k ${ARGN})
    set(${result} ${k} PARENT_SCOPE)
  elseif(is_all)
    run_predicate_all(k ${ARGN})
    set(${result} ${k} PARENT_SCOPE)
  else()
    message(FATAL_ERROR "Predicate type '${type}' does not exist")
  endif()
endfunction()

# Add a component to be built or downloaded.
#
# The component should have a standard structure, providing a folder in ${AUDIO_ENGINE_COMPONENTS_DIR}
# named ${name}, with a build/build and build/clean executable.
macro(add_standard_component
  list
  name
  predicate
  system
  )              
  string(TOLOWER ${name} lowercase_name)
  add_component(
    ${list}
    ${name}
    ${predicate}
    "external_libraries/${lowercase_name}/build/${system}/build"
    "external_libraries/${lowercase_name}/build/${system}/clean"
    "${lowercase_name}-${system}")
endmacro()

# Add a component to be built or downloaded.
#   predicate         
#     Should be a predicate (see above) to check it the component needs to be built or downloaded.
#   build_executable  
#     Should be the name of an executable that will build the component if requested.           
#   clean_executable  
#     Should be the name of an executable that will remove the component if requested.           
#   package_name      
#     Should be the name of the component on the package server.           
macro(add_component
  list
  name
  predicate
  build_executable
  clean_executable
  package_name
  )
  list(APPEND ${list} ${name})
  set(AudioEngine_${list}_${name}_predicate         ${predicate})
  set(AudioEngine_${list}_${name}_build_executable  ${build_executable})
  set(AudioEngine_${list}_${name}_clean_executable  ${clean_executable})
  set(AudioEngine_${list}_${name}_package_name      ${package_name})
endmacro()

# Print all components
function(print_components
  list
  )
  message(STATUS "Listing components:")
  foreach(name ${${list}})
    set(predicate         ${AudioEngine_${list}_${name}_predicate})
    set(build_executable  ${AudioEngine_${list}_${name}_build_executable})
    set(clean_executable  ${AudioEngine_${list}_${name}_clean_executable})
    set(package_name      ${AudioEngine_${list}_${name}_package_name})
    print_component(
      ${name}
      ${predicate}
      ${build_executable}
      ${clean_executable}
      ${package_name}
      )
  endforeach()
endfunction()

# TODO build, get, check status

# Clean all components
function(clean_components
  list
  )
  foreach(name ${${list}})
    set(clean_executable  ${AudioEngine_${list}_${name}_clean_executable})
    clean_component(
      ${name}
      ${clean_executable}
      )
  endforeach()
endfunction()


# Resolve all components. That is, for each component check whether it needs
# to be resolved using the predicate, then either download or compile it
# based on the value of AUDIO_ENGINE_BUILD_COMPONENTS.
function(resolve_components
  list
  )
  foreach(name ${${list}})
    set(predicate         ${AudioEngine_${list}_${name}_predicate})
    set(build_executable  ${AudioEngine_${list}_${name}_build_executable})
    set(clean_executable  ${AudioEngine_${list}_${name}_clean_executable})
    set(package_name      ${AudioEngine_${list}_${name}_package_name})
    resolve_component(
      ${name}
      ${predicate}
      ${build_executable}
      ${clean_executable}
      ${package_name}
      )
  endforeach()
endfunction()


# ==============================================================================
# Internal functions
# ==============================================================================

function(run_predicate_file_exists
  k
  file
  )
  # TODO really no cross-platform check in CMake?
  if(${AUDIO_ENGINE_SHOW_COMPONENT_OUTPUT})
    set(execute_process_args "")
  else()
    set(execute_process_args OUTPUT_QUIET ERROR_QUIET)
  endif()
  execute_process(
    COMMAND test -e ${file}
    RESULT_VARIABLE j
    WORKING_DIRECTORY ${AUDIO_ENGINE_WORKING_DIR}
    ${execute_process_args}
    )
  if(NOT j)
    set(${k} True PARENT_SCOPE)
  else()
    set(${k} False PARENT_SCOPE)
  endif()
endfunction()

function(run_predicate_constant
  k
  value
  )
  if(${value})
    set(${k} True PARENT_SCOPE)
  else()
    set(${k} False PARENT_SCOPE)
  endif()
endfunction()

function(run_predicate_some
  k
  # ps ...
  )       
  set(ps ${ARGN})
  foreach(p ${ps})
    run_predicate(r ${${p}})
    if(${r})
      set(${k} True PARENT_SCOPE)
      return()
    endif()
  endforeach()
  set(${k} False PARENT_SCOPE)
endfunction()

function(run_predicate_all
  k
  # ps ...
  )       
  set(ps ${ARGN})
  foreach(p ${ps})
    # message(">>> ${p}")
    # message(">>> ${${p}}")
    run_predicate(r ${${p}})
    if(NOT ${r})
      set(${k} False PARENT_SCOPE)
      return()
    endif()
  endforeach()
  set(${k} True PARENT_SCOPE)
endfunction()



function(print_component
  name
  predicate
  build_executable
  clean_executable
  package_name
  )
  message(STATUS "  ${name}")
  message(STATUS "    ${predicate}")
  message(STATUS "    ${build_executable}")
  message(STATUS "    ${clean_executable}")
  message(STATUS "    ${package_name}")
endfunction()

function(clean_component
  name
  executable
  )
  set(base_message "Cleaning component ${name}")
  message(STATUS ${base_message})
  run_executable(${executable} clean_result)
  if(NOT clean_result)
    message(STATUS "${base_message} -- done")
  else()
    message(SEND_ERROR "${base_message} -- failed")
  endif()
endfunction()

function(resolve_component
  name
  predicate
  build_executable
  clean_executable
  package_name
  )
  set(base_message "Resolving component ${name}")
  message(STATUS ${base_message})

  run_predicate(predicate_result ${${predicate}})

  if(predicate_result)
    message(STATUS "${base_message} -- found")
  else()
    if(AUDIO_ENGINE_BUILD_COMPONENTS)
      message(STATUS "${base_message} -- missing, trying to build locally...")
      run_executable(${build_executable} compile_result)
      if(NOT compile_result)
        message(STATUS "${base_message} -- done")
      else()
        message(FATAL_ERROR "Could not build ${name}")
      endif()
    else()
      message(STATUS "${base_message} -- missing, trying to download from package server...")
      run_package_manager(${package_name} get_result)
      if(NOT get_result)
        message(STATUS "${base_message} -- done")
      else() 
        # message(">>>> ${get_result}")
        message(FATAL_ERROR "Could not download ${name}")
      endif()
    endif()
  endif()
endfunction()

macro(run_executable
  )
  string(COMPARE EQUAL ${AUDIO_ENGINE_SYSTEM_NAME} msys is_msys)
  if (NOT is_msys)
    run_executable_std(${ARGN})
  else()
    run_executable_msys(${ARGN})
  endif()
endmacro()

function(run_executable_std
  executable
  result
  )
  if(${AUDIO_ENGINE_SHOW_COMPONENT_OUTPUT})
    set(execute_process_args "")
  else()
    set(execute_process_args OUTPUT_QUIET ERROR_QUIET)
  endif()

  execute_process(
    COMMAND           ${executable}
    RESULT_VARIABLE   execute_process_result
    WORKING_DIRECTORY ${AUDIO_ENGINE_WORKING_DIR}
    ${execute_process_args}
    )
  set(result ${execute_process_result} PARENT_SCOPE)
endfunction()

function(fix_posix_path
  path
  result)
  set(str ${path})
  # string(REPLACE "C:/" "C:/" str ${str})
  # string(REPLACE "y:/" "Y:/" str ${str})
  set(${result} ${str} PARENT_SCOPE)
endfunction()                        

function(run_executable_msys
  executable
  result
  )
  if(${AUDIO_ENGINE_SHOW_COMPONENT_OUTPUT})
    set(execute_process_args "")
  else()
    set(execute_process_args OUTPUT_QUIET ERROR_QUIET)
  endif()                        

  execute_process(           
    COMMAND           sh ${executable}
    RESULT_VARIABLE   execute_process_result
    WORKING_DIRECTORY ${AUDIO_ENGINE_WORKING_DIR}
    ${execute_process_args}
    )
  set(result ${execute_process_result} PARENT_SCOPE)
endfunction()


function(run_package_manager
  package_name
  result
  )
  if(${AUDIO_ENGINE_SHOW_COMPONENT_OUTPUT})
    set(execute_process_args "")
  else()
    set(execute_process_args OUTPUT_QUIET ERROR_QUIET)
  endif()
  # message(">>>> ${DIST_EXECUTABLE}")
  # FIXME command style of package manager is hardcoded
  execute_process(
    COMMAND             ${DIST_EXECUTABLE} get ${package_name}
    RESULT_VARIABLE     execute_process_result
    WORKING_DIRECTORY   ${AUDIO_ENGINE_WORKING_DIR}
    ${execute_process_args}
    )
  set(result ${execute_process_result} PARENT_SCOPE)
endfunction()

# ==============================================================================
# Module init
# ==============================================================================

macro(init_audio_engine)
  if (NOT AUDIO_ENGINE_SYSTEM_NAME)
    if(APPLE)
      set(AUDIO_ENGINE_SYSTEM_NAME "osx")
    elseif(MSVC)
      set(AUDIO_ENGINE_SYSTEM_NAME "msvc")
    elseif(True)  # TODO string(COMPRARE EQUAL ${CMAKE_GENERATOR} "MSYS Makefiles") or something
      set(AUDIO_ENGINE_SYSTEM_NAME "msys")
    endif()
    # TODO else FATAL_ERROR unknown system
  endif()

  if(NOT AUDIO_ENGINE_WORKING_DIR)
    set(AUDIO_ENGINE_WORKING_DIR ${CMAKE_SOURCE_DIR})
  endif()
endmacro()

init_audio_engine()

