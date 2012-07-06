##
##   ScoreCleaner Audio Engine
##
##   Copyright (c) 2012 DoReMIR Music Research AB.
##   All rights reserved.
##

##
##   This file builds the sclaudio unit test suite and optionally run it as
##   part of the CMake build.
##


##
## Build unit tests
##

option(SCLAUDIO_PROFILE "Compile with profiler flags -pg set" FALSE)

if(${SCLAUDIO_PROFILE})
    add_definitions("-pg")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -pg")
endif()

option(BUILD_SCLAUDIO_TESTS "Build unit tests" TRUE)

if(${BUILD_SCLAUDIO_TESTS})
    set(SCLAUDIO_TEST_DEFINITIONS ${SCLAUDIO_DEFINITIONS})
    message("Compiler definitions (unit tests): ${SCLAUDIO_TEST_DEFINITIONS}")

    add_executable(SCLAUDIO_TESTS "main.cc")

    if(${SCLAUDIO_OSX})
        set_target_properties(SCLAUDIO_TESTS
            PROPERTIES
                OUTPUT_NAME               "sclaudio_tests"
                OSX_ARCHITECTURES         i386
                RUNTIME_OUTPUT_DIRECTORY  "../bin"
                COMPILE_DEFINITIONS       "${SCLAUDIO_TEST_DEFINITIONS}")
    endif()

    if(${SCLAUDIO_WIN})
        set_target_properties(SCLAUDIO_TESTS
            PROPERTIES
                OUTPUT_NAME              "sclaudio_tests"
                COMPILE_DEFINITIONS      "${SCLAUDIO_TEST_DEFINITIONS}")
    endif()

    if(${SCLAUDIO_OSX})
        set(CMAKE_LIBRARY_PATH          "../lib/mac/static"
                                        "../lib/mac/shared")
        find_library(GTEST              "libgtest.a")
    endif()
    if(${SCLAUDIO_WIN})
        set(CMAKE_LIBRARY_PATH          "../lib/win/static"
                                        "../lib/win/shared")
        find_library(GTEST              "gtest.lib")
    endif()

    target_link_libraries(SCLAUDIO_TESTS
                          SCLAUDIO
                          ${GTEST})
endif()

##
## Run tests after build
##

option(RUN_SCLAUDIO_TESTS "Run unit tests after build" TRUE)

if (${RUN_SCLAUDIO_TESTS})
    if(${SCLAUDIO_OSX})
        add_custom_command(TARGET SCLAUDIO_TESTS POST_BUILD
                           COMMAND "${CMAKE_CURRENT_BINARY_DIR}/../bin/sclaudio_tests")
    endif()
    if(${SCLAUDIO_WIN})
        message(${CMAKE_CURRENT_BINARY_DIR})
        add_custom_command(TARGET SCLAUDIO_TESTS POST_BUILD
                           COMMAND "${CMAKE_CURRENT_BINARY_DIR}/../sclaudio_tests.exe") # this should work on OS X too!
    endif()
endif()

