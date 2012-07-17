#!/bin/bash

# This script sets up a CMake build in the directory $BUILD_DIRECTORY and runs Cmake
# Note that the Makefile in the top directory invokes build/Makefile

# Defaults to 'build'
if [ -z "$BUILD_DIRECTORY" ]
then
   BUILD_DIRECTORY="build"
fi

mkdir -p $BUILD_DIRECTORY;
pushd $BUILD_DIRECTORY;
cmake "$@" ..;

if [ $? -eq 0 ]; then
	popd;
	echo "";
	echo "Bootstrapping succeded, type 'make' to build or 'make help' for options."
	echo "";
	exit 0
else
	popd;
	exit 1
fi