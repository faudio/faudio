
if(APPLE)
  set(CMAKE_CXX_COMPILER
    "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++"
    )
  set(CMAKE_CXX_FLAGS 
    "${CMAKE_CXX_FLAGS} -stdlib=libc++"
    )
else()
  set(CMAKE_CXX_COMPILER
    "c++"
    )
endif()
set(CMAKE_CXX_FLAGS 
  "${CMAKE_CXX_FLAGS} -std=c++0x"
  )
