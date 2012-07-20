
# FIXME  -  BUILD_DEPENDENCIES is global
# FIXME  -  build_component and get_component should optionally show their output

if(APPLE)
  set(AUDIO_ENGINE_SYSTEM_NAME "osx")
elseif(MSVC)
  set(AUDIO_ENGINE_SYSTEM_NAME "msvc")
elseif()
  # TODO string(COMPRARE EQUAL ${CMAKE_GENERATOR} "MSYS Makefiles") or something
  set(AUDIO_ENGINE_SYSTEM_NAME "msys")
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
  execute_process(
    COMMAND test -e ${file}
    RESULT_VARIABLE temp_result)  
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
      ${package_name})
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
      ${package_name})
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

function(resolve_component
  name
  predicate
  build_executable
  clean_executable
  package_name
  )  
  set(base_message "Resolving component ${name}")
  message(STATUS "${base_message}")

  run_predicate(predicate_result ${${predicate}})

  if(predicate_result)
    message(STATUS "${base_message} -- found")    
  else()
    if(BUILD_DEPENDENCIES)
      message(STATUS "${base_message} -- missing, trying to build locally...")
      build_component(${name} ${build_executable} compile_result)
      if(NOT compile_result)
        message(STATUS "${base_message} -- done")
      else()
        message(FATAL_ERROR "Could not build ${NAME}")
      endif()
    else()
      message(STATUS "${base_message} -- missing, trying to download from package server...")
      get_component(${name} ${package_name} get_result)
      if(NOT get_result)
        message(STATUS "${base_message} -- done")
      else()
        message(FATAL_ERROR "Could not download ${NAME}")
      endif()
    endif()
  endif()      
endfunction()

function(build_component 
  name 
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
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    RESULT_VARIABLE   execute_process_result
    ${execute_process_args} 
    )                                        
  set(result ${execute_process_result} PARENT_SCOPE)
endfunction()

function(get_component 
  name 
  package_name 
  result
  )
  if(${AUDIO_ENGINE_SHOW_COMPONENT_OUTPUT})
    set(execute_process_args "")
  else()
    set(execute_process_args OUTPUT_QUIET ERROR_QUIET)
  endif()
  execute_process( 
    COMMAND             dist get ${package_name}
    WORKING_DIRECTORY   ${CMAKE_SOURCE_DIR}
    RESULT_VARIABLE     execute_process_result
    ${execute_process_args}
    )
  set(result ${execute_process_result} PARENT_SCOPE)
endfunction()



