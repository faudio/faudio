#!/bin/bash

APP="/Users/`whoami`/Desktop/ScoreCleaner.app"
DEST="$APP/Contents/Frameworks"
SRC="build"

rm -rf  "$DEST/ScoreCleanerAudio.framework";
cp -R   "$SRC/ScoreCleanerAudio.framework" "$DEST/";

open $APP;
