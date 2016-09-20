#!/bin/bash

# Defaults to 'build'
if [ -z "$BUILD_DIRECTORY" ]
then
   BUILD_DIRECTORY="build"
fi

rm -r CMakeFiles
mkdir -p $BUILD_DIRECTORY
pushd $BUILD_DIRECTORY
rm -r CMakeFiles
rm CMakeCache.txt
# PATH=/cygdrive/c/MinGW/bin:$PATH cmake -G "MSYS Makefiles" .. #-DMAKE_C_COMPILER=/cygdrive/c/MinGW/bin/gcc.exe -DMAKE_CXX_COMPILER=/cygdrive/c/MinGW/bin/g++.exe ..
cmake -G "MSYS Makefiles" ..
make components_resolve
make
popd
