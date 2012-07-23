# - See that /Developer/SDKs are present
# If not, ask user to symlink.
# Defines FIX_OSX_LION_PATHS_SYMLINKED

if (APPLE)
  execute_process(
    COMMAND test -e /Developer/SDKs
    RESULT_VARIABLE DEVELOPER_MISSING
    )                            

  if (DEVELOPER_MISSING)
    message(STATUS 
      "It seems that /Developer/SDKs dosn't exist on your machine. "
      "This may be because you are running Mac OS X Lion or later. "
      "To create a link, enter your administrator password: ")

    execute_process(
      COMMAND 
        bash "${CMAKE_SOURCE_DIR}/cmake/modules/fix-osx-lion-path.sh"
        RESULT_VARIABLE r
        )               
    if(${r})
      set(FIX_OSX_LION_PATHS_SYMLINKED True)
    endif()
  endif()  
endif()