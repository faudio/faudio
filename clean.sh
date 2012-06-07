#!/bin/bash

# Remove CMake stuff, except CMakeLists.txt

# find -X . -name "[Cc][Mm]ake*" | grep -v "Lists.txt" | xargs rm -rf;
# find -X . -name "Makefile"                           | xargs rm -rf;
# find -X . -name "sclaudio.build"                     | xargs rm -rf;
# rm -rf *.xcodeproj;

rm -rf "build";
mkdir "build";