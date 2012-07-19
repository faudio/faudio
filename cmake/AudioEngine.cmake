

function(make_existance_predicate 
  result
  file 
  )
  set(${result} exists ${file} PARENT_SCOPE)
endfunction()


function(run_predicate
  result
  predicate_type
  predicate_argument
  )

  string(COMPARE EQUAL ${predicate_type} exists pred_is_exists)
  
  if(pred_is_exists)
    run_predicate_exist(temp_result ${predicate_argument})
    set(result ${temp_result} PARENT_SCOPE)
  else()
    message(FATAL_ERROR "Predicate type '${predicate_type}' does not exist")
  endif()
endfunction()  

function(run_predicate_exist 
  result
  file 
  )
  # TODO really no cross-platform check in CMake?
  execute_process(
    COMMAND test -e ${file}
    RESULT_VARIABLE temp_res)  
  if(NOT temp_res)
    set(${result} True PARENT_SCOPE)
  else()
    set(${result} False PARENT_SCOPE)
  endif()
endfunction()






# Add a component to be compiled or fetched
# For each library, specify
function(add_component
  component_list             
  name
  predicate
  build_excecutable
  clean_executable
  package_name
  )
  list(APPEND ${component_list} ${name} PARENT_SCOPE)
  set(AudioEngine_${name}_predicate         ${predicate}        PARENT_SCOPE)
  set(AudioEngine_${name}_build_executable  ${build_executable} PARENT_SCOPE)
  set(AudioEngine_${name}_clean_executable  ${clean_executable} PARENT_SCOPE)
  set(AudioEngine_${name}_package_name      ${package_name}     PARENT_SCOPE)
endfunction()

# Resolve all components
function(resolve_components
  component_list
  )
  foreach(name ${component_list})
    set(predicate         AudioEngine_${name}_predicate)
    set(build_executable  AudioEngine_${name}_build_executable)
    set(clean_executable  AudioEngine_${name}_build_executable)
    set(package_name      AudioEngine_${name}_package_name)
    resolve_component(
      ${name}
      ${predicate}
      ${build_executable}
      ${clean_executable}
      ${package_name})
  endforeach()
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

  run_predicate(predicate predicate_result)
  if(predicate_result)
    message(STATUS "${base_message} -- found")    
  else()
    if(build_dependencies)
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
  execute_process( 
    COMMAND           executable
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    RESULT_VARIABLE   execute_process_result
    OUTPUT_QUIET
    ERROR_QUIET 
    )                                        
  set(result ${execute_process_result} PARENT_SCOPE)
endfunction()

function(get_component 
  name 
  package_name 
  result
  )
  execute_process( 
    COMMAND dist get ${package_name}
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    RESULT_VARIABLE ${RESULT}
    OUTPUT_QUIET
    ERROR_QUIET 
    )
  set(result ${execute_process_result} PARENT_SCOPE)
endfunction()



