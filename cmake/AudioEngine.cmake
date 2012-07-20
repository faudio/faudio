# - Component manager for the Audio Engine
# Author : Hans Hoglund 2012
#
#
# Variables that influence the behaviour of this module:
#
#     AUDIO_ENGINE_SYSTEM_NAME            
#         May be set to anything, otherwise deault to osx, msvc or msys.
#
#     AUDIO_ENGINE_SHOW_COMPONENT_OUTPUT
#         To show intermediate build results on stdout/stderr.
#
#     AUDIO_ENGINE_BUILD_COMPONENTS
#         To compile components locally.
#
#     AUDIO_ENGINE_WORKING_DIR
#         Dir in which to run executables.

message(">>>>>> "${AUDIO_ENGINE_BUILD_COMPONENTS})
message(">>>>>> "${AUDIO_ENGINE_SHOW_COMPONENT_OUTPUT})
message(">>>>>> "${AUDIO_ENGINE_WORKING_DIR})

if (NOT AUDIO_ENGINE_SYSTEM_NAME)
  if(APPLE)
    set(AUDIO_ENGINE_SYSTEM_NAME "osx")
  elseif(MSVC)
    set(AUDIO_ENGINE_SYSTEM_NAME "msvc")
  elseif()
    # TODO string(COMPRARE EQUAL ${CMAKE_GENERATOR} "MSYS Makefiles") or something
    set(AUDIO_ENGINE_SYSTEM_NAME "msys")
  endif()
endif()

function(predicate_file_exists 
  result
  file 
  )
  set(${result} file_exists ${file} PARENT_SCOPE)
endfunction()

function(run_predicate_file_exists 
  result
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
    RESULT_VARIABLE temp_result
    WORKING_DIRECTORY ${AUDIO_ENGINE_WORKING_DIR}
    ${execute_process_args}
    )  
  # TODO use negate_inline from Prelude?
  if(NOT temp_result)
    set(${result} True PARENT_SCOPE)
  else()
    set(${result} False PARENT_SCOPE)
  endif()
endfunction()

function(run_predicate
  result
  type
  args
  )
  string(COMPARE EQUAL ${type} file_exists pred_is_file_exists)
  
  if(pred_is_file_exists)
    run_predicate_file_exists(temp_result ${args})
    set(${result} ${temp_result} PARENT_SCOPE)
  else()
    message(FATAL_ERROR "Predicate type '${type}' does not exist")
  endif()
endfunction()

# Add a component to be compiled or fetched                      
#
# The component should have a standard structure, providing a folder in ${AUDIO_ENGINE_COMPONENTS_DIR}
# named ${name}, with a build/build and build/clean executable.
#
macro(add_standard_component
  list
  name
  predicate
  system
  )
  add_component(
    ${list}
    ${name}
    ${predicate}
    "cmake/build/${name}/${system}/build"
    "cmake/build/${name}/${system}/clean"
    "${name}-${system}")
endmacro()

# Add a component to be compiled or fetched
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


# Resolve all components
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


# Internals

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
        message(FATAL_ERROR "Could not build ${NAME}")
      endif()
    else()
      message(STATUS "${base_message} -- missing, trying to download from package server...")
      run_package_manager(${name} ${package_name} get_result)
      if(NOT get_result)
        message(STATUS "${base_message} -- done")
      else()
        message(FATAL_ERROR "Could not download ${NAME}")
      endif()
    endif()
  endif()      
endfunction()


function(run_executable 
  executable 
  result
  )            
  if(${AUDIO_ENGINE_SHOW_COMPONENT_OUTPUT})
    set(execute_process_args "")
  else()
    set(execute_process_args OUTPUT_QUIET ERROR_QUIET)
  endif()                          
message(">>>> ${executable}")
message(">>>> ${AUDIO_ENGINE_WORKING_DIR}")
  
  execute_process( 
    COMMAND           ${executable}
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
  # FIXME name and command style of package manager is hardcoded
  execute_process( 
    COMMAND             dist get ${package_name}
    RESULT_VARIABLE     execute_process_result
    WORKING_DIRECTORY ${AUDIO_ENGINE_WORKING_DIR}
    ${execute_process_args}
    )
  set(result ${execute_process_result} PARENT_SCOPE)
endfunction()



