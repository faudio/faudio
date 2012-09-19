
if(APPLE)
  set(CMAKE_CXX_COMPILER
    "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++"
    )
  # set(CMAKE_CXX_FLAGS 
    # "${CMAKE_CXX_FLAGS} -stdlib=libc++"
    # )
  set(CMAKE_CXX_FLAGS 
  "${CMAKE_CXX_FLAGS} -std=c++0x"
  )
else()
  set(CMAKE_CXX_COMPILER
    "c++"
    )
  set(CMAKE_CXX_FLAGS 
  "${CMAKE_CXX_FLAGS} -std=gnu++0x"
  )
endif()
