##
##   ScoreCleaner Audio Engine
##
##   Copyright (c) 2012 DoReMIR Music Research AB.
##   All rights reserved.
##

##
##   This file builds any of the following:
##
##     * An OS X framework (ScoreCleanerAudio.framework)
##     * An OS X dynamic library (libsclaudio.dylib)
##     * A Windows dynamic library (sclaudio.dll)
##
##   It optionally build the command line tool bin/sclaudio.
## 

set(SCLAUDIOX_OSX_SOURCES sclaudiox/processor/plugin/au.mm)

set(SCLAUDIOX_WIN_SOURCES sclaudiox/processor/plugin/au_nil.cc)

if (${SCLAUDIO_OSX})
    set(SCLAUDIOX_NATIVE_SOURCES ${SCLAUDIOX_OSX_SOURCES})
endif ()

if (${SCLAUDIO_WIN})
    set(SCLAUDIOX_NATIVE_SOURCES ${SCLAUDIOX_WIN_SOURCES})
endif ()

set(SCLAUDIOX_SOURCES ${SCLAUDIOX_NATIVE_SOURCES}
                      
                      sclaudiox/processor.cc
                      sclaudiox/stream.cc
                      
                      sclaudiox/scheduling/future.cc
                      
                      sclaudiox/processor/plugin/vst2.cc
                      sclaudiox/processor/synth/fluid.cc
                      
                      sclaudiox/device/audio.cc
                      sclaudiox/device/midi.cc
                      sclaudiox/device/session.cc
                      sclaudiox/stream/device/audio.cc
                      sclaudiox/stream/device/midi.cc
                      
                      sclaudiox/util/immutable_list.cc
                      sclaudiox/util/symbol.cc)  
                      
set(SCLAUDIO_SOURCES  ${SCLAUDIOX_SOURCES}
                      sclaudio.cc)

message("Including the following source files: ${SCLAUDIO_SOURCES}")

if(${CMAKE_COMPILER_IS_GNUCC})
    add_definitions("-Wreturn-type")
endif()

##
## Build library
##

message("Compiler definitions (library): ${SCLAUDIO_DEFINITIONS}")

if(${SCLAUDIO_OSX})
    option(BUILD_SCLAUDIO_DYLIB     "Build an OS X dynamic library" FALSE)
    option(BUILD_SCLAUDIO_FRAMEWORK "Build an OS X framework"       TRUE)
                     
    if(${BUILD_SCLAUDIO_DYLIB})
        add_library(SCLAUDIO_DYLIB SHARED ${SCLAUDIO_SOURCES})
        set_target_properties(SCLAUDIO_DYLIB
            PROPERTIES
                OUTPUT_NAME                 "sclaudio"
                INSTALL_NAME_DIR            "/usr/local/lib/sclaudio.dylib"
                OSX_ARCHITECTURES           i386
                XCODE_ATTRIBUTE_GCC_VERSION "com.apple.compilers.llvm.clang.1_0"
                LIBRARY_OUTPUT_DIRECTORY    "../"
                COMPILE_DEFINITIONS         "${SCLAUDIO_DEFINITIONS}")
        target_link_libraries(SCLAUDIO_DYLIB ${SCLAUDIO_LIBRARIES})
    endif()    

    if(${BUILD_SCLAUDIO_FRAMEWORK})
        add_library(SCLAUDIO SHARED ${SCLAUDIO_SOURCES})
        set_target_properties(SCLAUDIO
            PROPERTIES
                OUTPUT_NAME                 "ScoreCleanerAudio"
                FRAMEWORK                   TRUE
                FRAMEWORK_VERSION           "${SCLAUDIO_VERSION}"
                INSTALL_NAME_DIR            "@executable_path/../Frameworks/ScoreCleanerAudio.framework/ScoreCleanerAudio"
                OSX_ARCHITECTURES           i386
                XCODE_ATTRIBUTE_GCC_VERSION "com.apple.compilers.llvm.clang.1_0"
                LIBRARY_OUTPUT_DIRECTORY    "../"
                COMPILE_DEFINITIONS         "${SCLAUDIO_DEFINITIONS}")
        target_link_libraries(SCLAUDIO ${SCLAUDIO_LIBRARIES})
    endif()
endif()


if(${SCLAUDIO_WIN})
    option(BUILD_SCLAUDIO_DLL "Build a Windows dynamic library" TRUE)
    
    if(${BUILD_SCLAUDIO_DLL})
        add_library(SCLAUDIO SHARED ${SCLAUDIO_SOURCES})
        set_target_properties(SCLAUDIO
            PROPERTIES
                OUTPUT_NAME              sclaudio
                COMPILE_DEFINITIONS      "${SCLAUDIO_DEFINITIONS}")
        target_link_libraries(SCLAUDIO ${SCLAUDIO_LIBRARIES})
    endif()
endif()



##
## Build command-line tool
##

option(BUILD_SCLAUDIO_CL "Build the sclaudio command-line tool" TRUE)

if(${BUILD_SCLAUDIO_CL})
    set(SCLAUDIO_CL_DEFINITIONS ${SCLAUDIO_DEFINITIONS})
    message("Compiler definitions (command-line tool): ${SCLAUDIO_CL_DEFINITIONS}")

    add_executable(SCLAUDIO_CL "main.cc")

    if(${SCLAUDIO_OSX})
        set_target_properties(SCLAUDIO_CL
            PROPERTIES
                OUTPUT_NAME                 "sclaudio"
                OSX_ARCHITECTURES           i386
                RUNTIME_OUTPUT_DIRECTORY    "../bin"
            COMPILE_DEFINITIONS             "${SCLAUDIO_CL_DEFINITIONS}")
    endif()
    if(${SCLAUDIO_WIN})
        set_target_properties(SCLAUDIO_CL
            PROPERTIES
                OUTPUT_NAME                 "sclaudio_cl"
                COMPILE_DEFINITIONS         "${SCLAUDIO_CL_DEFINITIONS}")
    endif()

    if (MSVC_IDE)
        set_target_properties(SCLAUDIO_CL PROPERTIES PREFIX "../")
    endif()

    # TODO if building dylib and/or framework on OS X, this only links with framwork
    target_link_libraries(SCLAUDIO_CL SCLAUDIO)           
endif()
