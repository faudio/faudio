#!/bin/bash

# Finish the framework

# rm -rf   build/Frameworks/ScoreCleanerAudio.framework/Libraries
# rm -rf   build/Frameworks/ScoreCleanerAudio.framework/Frameworks
# mkdir -p build/Frameworks/ScoreCleanerAudio.framework/Libraries
# mkdir -p build/Frameworks/ScoreCleanerAudio.framework/Frameworks
# 
# cp -R lib/mac/shared/     build/Frameworks/ScoreCleanerAudio.framework/Libraries
# cp -R lib/mac/frameworks/ build/Frameworks/ScoreCleanerAudio.framework/Frameworks
# 
# find -X build/Frameworks/ScoreCleanerAudio.framework/Libraries  -name ".svn" | xargs rm -rf
# find -X build/Frameworks/ScoreCleanerAudio.framework/Frameworks -name ".svn" | xargs rm -rf


# Make version

mkdir -p    build/ScoreCleanerAudio.framework/Libraries
mkdir -p    build/ScoreCleanerAudio.framework/Frameworks
cp -R       lib/mac/shared/     build/ScoreCleanerAudio.framework/Libraries
cp -R       lib/mac/frameworks/ build/ScoreCleanerAudio.framework/Frameworks

find -X     build/ScoreCleanerAudio.framework/Libraries  -name ".svn" | xargs rm -rf
find -X     build/ScoreCleanerAudio.framework/Frameworks -name ".svn" | xargs rm -rf

# XCode version

# mkdir -p    build-xcode/Debug/ScoreCleanerAudio.framework/Libraries
# mkdir -p    build-xcode/Debug/ScoreCleanerAudio.framework/Frameworks
# cp -R       lib/mac/shared/     build-xcode/Debug/ScoreCleanerAudio.framework/Libraries
# cp -R       lib/mac/frameworks/ build-xcode/Debug/ScoreCleanerAudio.framework/Frameworks
# 
# find -X     build-xcode/Debug/ScoreCleanerAudio.framework/Libraries  -name ".svn" | xargs rm -rf
# find -X     build-xcode/Debug/ScoreCleanerAudio.framework/Frameworks -name ".svn" | xargs rm -rf
