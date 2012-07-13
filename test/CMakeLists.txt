##
##   ScoreCleaner Audio Engine
##
##   Copyright (c) 2012 DoReMIR Music Research AB.
##   All rights reserved.
##

##
##   This file builds the SCL unit test suite and optionally run it as
##   part of the CMake build.
##


##
## Build unit tests
##


option(BUILD_SCL_TESTS "Build unit tests" TRUE)

if(${BUILD_SCL_TESTS})
    set(SCL_TEST_DEFINITIONS ${SCL_DEFINITIONS})
    message("Compiler definitions (unit tests): ${SCL_TEST_DEFINITIONS}")

    add_executable(SCL_TESTS "main.cc")

    if(${SCL_OSX})
        set_target_properties(SCL_TESTS
            PROPERTIES
                OUTPUT_NAME               "SCL_tests"
                OSX_ARCHITECTURES         i386
                RUNTIME_OUTPUT_DIRECTORY  "../bin"
                COMPILE_DEFINITIONS       "${SCL_TEST_DEFINITIONS}")
    endif()

    if(${SCL_WIN})
        set_target_properties(SCL_TESTS
            PROPERTIES
                OUTPUT_NAME              "SCL_tests"
                COMPILE_DEFINITIONS      "${SCL_TEST_DEFINITIONS}")
    endif()

    if(${SCL_OSX})
        set(CMAKE_LIBRARY_PATH          "../lib/mac/static"
                                        "../lib/mac/shared")
        find_library(GTEST              "libgtest.a")
    endif()
    if(${SCL_WIN})
        set(CMAKE_LIBRARY_PATH          "../lib/win/static"
                                        "../lib/win/shared")
        find_library(GTEST              "gtest.lib")
    endif()

    target_link_libraries(SCL_TESTS
                          SCL
                          ${GTEST})
endif()

##
## Run tests after build
##

option(RUN_SCL_TESTS "Run unit tests after build" TRUE)

if (${RUN_SCL_TESTS})
    if(${SCL_OSX})
        add_custom_command(TARGET SCL_TESTS POST_BUILD
                           COMMAND "${CMAKE_CURRENT_BINARY_DIR}/../bin/SCL_tests")
    endif()
    if(${SCL_WIN})
        message(${CMAKE_CURRENT_BINARY_DIR})
        add_custom_command(TARGET SCL_TESTS POST_BUILD
                           COMMAND "${CMAKE_CURRENT_BINARY_DIR}/../SCL_tests.exe") # this should work on OS X too!
    endif()
endif()

