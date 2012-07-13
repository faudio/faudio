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
##     * An OS X dynamic library (libSCL.dylib)
##     * A Windows dynamic library (SCL.dll)
##
##   It optionally build the command line tool bin/SCL.
## 

set(SCLX_OSX_SOURCES sclaudiox/processor/plugin/au.mm)

set(SCLX_WIN_SOURCES sclaudiox/processor/plugin/au_nil.cc)

if (${SCL_OSX})
    set(SCLX_NATIVE_SOURCES ${SCLX_OSX_SOURCES})
endif ()

if (${SCL_WIN})
    set(SCLX_NATIVE_SOURCES ${SCLX_WIN_SOURCES})
endif ()

set(SCLX_SOURCES ${SCLX_NATIVE_SOURCES}
                      
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
                      
                      sclaudiox/util/symbol.cc)  
                      
set(SCL_SOURCES  ${SCLX_SOURCES}
                      sclaudio.cc)

message("Including the following source files: ${SCL_SOURCES}")

##
## Build library
##

message("Compiler definitions (library): ${SCL_DEFINITIONS}")

if(${SCL_OSX})
    option(BUILD_SCL_DYLIB     "Build an OS X dynamic library" FALSE)
    option(BUILD_SCL_FRAMEWORK "Build an OS X framework"       TRUE)
                     
    if(${BUILD_SCL_DYLIB})
        add_library(SCL_DYLIB SHARED ${SCL_SOURCES})
        set_target_properties(SCL_DYLIB
            PROPERTIES
                OUTPUT_NAME                 "SCL"
                INSTALL_NAME_DIR            "/usr/local/lib/SCL.dylib"
                OSX_ARCHITECTURES           i386
                XCODE_ATTRIBUTE_GCC_VERSION "com.apple.compilers.llvm.clang.1_0"
                LIBRARY_OUTPUT_DIRECTORY    "../"
                COMPILE_DEFINITIONS         "${SCL_DEFINITIONS}")
        target_link_libraries(SCL_DYLIB ${SCL_LIBRARIES})
    endif()    

    if(${BUILD_SCL_FRAMEWORK})
        add_library(SCL SHARED ${SCL_SOURCES})
        set_target_properties(SCL
            PROPERTIES
                OUTPUT_NAME                 "ScoreCleanerAudio"
                FRAMEWORK                   TRUE
                FRAMEWORK_VERSION           "${SCL_VERSION}"
                INSTALL_NAME_DIR            "@executable_path/../Frameworks/ScoreCleanerAudio.framework/ScoreCleanerAudio"
                OSX_ARCHITECTURES           i386
                XCODE_ATTRIBUTE_GCC_VERSION "com.apple.compilers.llvm.clang.1_0"
                LIBRARY_OUTPUT_DIRECTORY    "../"
                COMPILE_DEFINITIONS         "${SCL_DEFINITIONS}")
        target_link_libraries(SCL ${SCL_LIBRARIES})
    endif()
endif()


if(${SCL_WIN})
    option(BUILD_SCL_DLL "Build a Windows dynamic library" TRUE)
    
    if(${BUILD_SCL_DLL})
        add_library(SCL SHARED ${SCL_SOURCES})
        set_target_properties(SCL
            PROPERTIES
                OUTPUT_NAME              SCL
                COMPILE_DEFINITIONS      "${SCL_DEFINITIONS}")
        target_link_libraries(SCL ${SCL_LIBRARIES})
    endif()
endif()



##
## Build command-line tool
##

option(BUILD_SCL_CL "Build the SCL command-line tool" TRUE)

if(${BUILD_SCL_CL})
    set(SCL_CL_DEFINITIONS ${SCL_DEFINITIONS})
    message("Compiler definitions (command-line tool): ${SCL_CL_DEFINITIONS}")

    add_executable(SCL_CL "main.cc")

    if(${SCL_OSX})
        set_target_properties(SCL_CL
            PROPERTIES
                OUTPUT_NAME                 "SCL"
                OSX_ARCHITECTURES           i386
                RUNTIME_OUTPUT_DIRECTORY    "../bin"
            COMPILE_DEFINITIONS             "${SCL_CL_DEFINITIONS}")
    endif()
    if(${SCL_WIN})
        set_target_properties(SCL_CL
            PROPERTIES
                OUTPUT_NAME                 "SCL_cl"
                COMPILE_DEFINITIONS         "${SCL_CL_DEFINITIONS}")
    endif()

    if (MSVC_IDE)
        set_target_properties(SCL_CL PROPERTIES PREFIX "../")
    endif()

    # TODO if building dylib and/or framework on OS X, this only links with framwork
    target_link_libraries(SCL_CL SCL)           
endif()
