
# Uses EXTS, APPLE and MSYS + options and MAIN_LIBRARY

function(add_tool name)
  add_executable(${name} ${SRCS_ALL} "${name}.c")
  set_target_properties(${name} PROPERTIES 
    OUTPUT_NAME                 "${name}"
    RUNTIME_OUTPUT_DIRECTORY    "../bin"
    )
  if(APPLE)
    target_link_libraries(${name} ${MAIN_LIBRARY})
  else()
    target_link_libraries(${name} ${MAIN_STATIC_LIBRARY})
  endif()
endfunction()


add_tool(fa_list_audio)
add_tool(fa_list_midi)
add_tool(fa_echo_midi)
add_tool(fa_plug_midi)
add_tool(fa_default_midi)
add_tool(fa_template)
add_tool(fa_sndfile)

