#!/bin/bash

DEV="/Volumes/DoReMIR/development/kern"
APP="$DEV/ScoreCleaner.app"
SRC="build"
DEST="$APP/Contents/Frameworks"
DEST2="$DEV"

rm -rf  "$DEST/ScoreCleanerAudio.framework" && cp -R   "$SRC/ScoreCleanerAudio.framework" "$DEST/";
rm -rf  "$DEST2/ScoreCleanerAudio.framework" && cp -R   "$SRC/ScoreCleanerAudio.framework" "$DEST2/";