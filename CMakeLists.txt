##
##   ScoreCleaner Audio Engine
##
##   Copyright (c) 2012 DoReMIR Music Research AB. 
##   All rights reserved. 
##


project(sclaudio C CXX)

cmake_minimum_required(VERSION 2.8)
cmake_policy(SET CMP0015 NEW)

set(SCLAUDIO_VERSION_MAJOR 1)
set(SCLAUDIO_VERSION_MINOR 5)
set(SCLAUDIO_VERSION_MICRO 0)
set(SCLAUDIO_VERSION 
    "${SCLAUDIO_VERSION_MAJOR}.${SCLAUDIO_VERSION_MINOR}.${SCLAUDIO_VERSION_MICRO}")


##
## Check OS
##

if(${CMAKE_SYSTEM} MATCHES "Darwin")
    set(SCLAUDIO_OSX TRUE)
endif()

if(WIN32)
    set(SCLAUDIO_WIN TRUE)
endif()
             


##
## Global path options
##

if(${SCLAUDIO_WIN})
    set(SCLAUDIO_BOOST "C:/Program/boost/boost_1_47")
else()
    set(SCLAUDIO_BOOST "")
endif()

include_directories("include" 
                    "external" 
                    ${SCLAUDIO_BOOST})

set(PROJECT_SOURCE_DIR "src")
set(PROJECT_BINARY_DIR "build")


##
## Global options
##

option(ENABLE_LOGGING 
       "Enable standard logging"                
       FALSE)
      
option(REDIRECT_LOG_TO_STDOUT 
       "Redirect log messages to standard output"     
       FALSE)
       
option(ENABLE_UNICODE 
       "Enable Unicode support"                       
       TRUE)
       
option(SCL_PREFER_NONBLOCKING_AUDIO 
       "Use non-blocking audio by default"                       
       TRUE)
       
option(SCL_PREFER_EXCLUSIVE_AUDIO 
       "Open audio streams in exclusive mode by default"                       
       TRUE)
      
option(USE_DEBUG_LIBRARIES 
       "Use debug libraries" 
       FALSE)


set(SCLAUDIO_DEFINITIONS "")

if(ENABLE_LOGGING)
    set(SCLAUDIO_DEFINITIONS "SCL_LOG;${SCLAUDIO_DEFINITIONS}")
endif()

if(REDIRECT_LOG_TO_STDOUT)
    set(SCLAUDIO_DEFINITIONS "SCL_LOG_TO_STDOUT;${SCLAUDIO_DEFINITIONS}")
endif()

if(ENABLE_UNICODE)
    set(SCLAUDIO_DEFINITIONS "SCL_UNICODE;${SCLAUDIO_DEFINITIONS}")
endif()

if(SCL_PREFER_NONBLOCKING_AUDIO)
    set(SCLAUDIO_DEFINITIONS "SCL_PREFER_NONBLOCKING_AUDIO;${SCLAUDIO_DEFINITIONS}")
endif()

if(SCL_PREFER_EXCLUSIVE_AUDIO)
    set(SCLAUDIO_DEFINITIONS "SCL_PREFER_EXCLUSIVE_AUDIO;${SCLAUDIO_DEFINITIONS}")
endif()


if(CMAKE_BUILD_TYPE MATCHES Debug)
    set(SCLAUDIO_DEFINITIONS "SCL_DEBUG;${SCLAUDIO_DEFINITIONS}")
endif()

if(${sclaudio_win})
    set(SCLAUDIO_DEFINITIONS "NOMINMAX;${SCLAUDIO_DEFINITIONS}")
endif()


##
## Process child directories
##
                      
add_subdirectory(lib)
add_subdirectory(src)
add_subdirectory(test)  



##
## Make distribution
##

# #message("Installing to ${CMAKE_INSTALL_PREFIX}")
# 
# file(REMOVE_RECURSE build/dist/sclaudio-${SCLAUDIO_VERSION})
# file(MAKE_DIRECTORY build/dist/sclaudio-${SCLAUDIO_VERSION})
# 
# file(INSTALL
#     build/libsclaudio.dylib 
#     build/ScoreCleanerAudio.framework 
#     FILE_PERMISSIONS WORLD_EXECUTE
#     DESTINATION dist/sclaudio-${SCLAUDIO_VERSION}/)
# 
# file(INSTALL
#     doc/build/html/
#     DESTINATION dist/sclaudio-${SCLAUDIO_VERSION}/doc/html)
# 
# # file(RENAME
# #     "doc/build/latex/refman.pdf"
# #     "doc/build/latex/ScoreCleanerAudioEngine.pdf")
# # file(INSTALL
# #     "doc/build/latex/ScoreCleanerAudioEngine.pdf"
# #     DESTINATION dist/sclaudio-${SCLAUDIO_VERSION}/doc)
# 
# file(INSTALL
#     include/
#     DESTINATION dist/sclaudio-${SCLAUDIO_VERSION}/include)
# 
# file(INSTALL
#     language_bindings/lisp/audio-engine.asd
#     language_bindings/lisp/examples
#     language_bindings/lisp/tools
#     DESTINATION dist/sclaudio-${SCLAUDIO_VERSION}/language_bindings/lisp)
# file(COPY
#     language_bindings/lisp/
#     DESTINATION dist/sclaudio-${SCLAUDIO_VERSION}/language_bindings/lisp
#     PATTERN "*xfasl" EXCLUDE
#     PATTERN "*dylib" EXCLUDE
#     PATTERN "Makefile" EXCLUDE)                    

add_custom_command(
  OUTPUT FOO_CMD  
  COMMAND echo "foo"
  DEPENDS ${trofiles}
)
add_custom_target(foo DEPENDS FOO_CMD)


