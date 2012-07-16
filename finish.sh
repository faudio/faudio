#!/bin/bash

# Finish the framework

# Make version

mkdir -p    build/Frameworks
cp -R       lib/mac/frameworks/ build/Frameworks

# find -X     build/ScoreCleanerAudio.framework/Libraries  -name ".svn" | xargs rm -rf
# find -X     build/ScoreCleanerAudio.framework/Frameworks -name ".svn" | xargs rm -rf

