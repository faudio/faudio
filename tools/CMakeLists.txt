
# Uses EXTS, APPLE and MSYS + options and MAIN_LIBRARY

function(add_tool2 prefix name extension)
  add_executable(${name} ${SRCS_ALL} "${name}.${extension}")
  set_target_properties(${name} PROPERTIES 
    OUTPUT_NAME                 "${prefix}${name}"
    RUNTIME_OUTPUT_DIRECTORY    "../bin"
    )
  if(APPLE)
    target_link_libraries(${name} ${MAIN_LIBRARY})
  else()
    target_link_libraries(${name} ${MAIN_STATIC_LIBRARY})
  endif()
endfunction()

function(add_tool name)
  add_tool2(faudio- ${name} c)
endfunction()

add_tool2("" faudio c)

add_tool(list_audio)
add_tool(list_midi)
add_tool(plug_midi)
add_tool(plug_audio)
add_tool(sine)
add_tool(record)
add_tool(play)
add_tool(version)
add_tool(stereo)
add_tool(custom)
add_tool(test_midi)
add_tool(test_midi_in)
add_tool(test_compound)
add_tool(test_compound2)
add_tool(record_streaming)
add_tool(impulses)
add_tool(level)
add_tool(vst)
add_tool(io_simple_filter)

if(APPLE OR ENABLE_FLUIDSYNTH)
  add_tool(test_dls)
endif()
if(APPLE)
  add_tool(test_core_midi)
endif()
if (ENABLE_HTTP)
  add_tool(play_streaming)
endif()
