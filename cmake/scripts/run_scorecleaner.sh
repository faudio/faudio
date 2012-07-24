#!/bin/bash

if [ -z "$SCORECLEANER" ]
then
    SCORECLEANER="$HOME/Desktop/ScoreCleaner.app"
fi

rm -rf "$SCORECLEANER/Contents/Frameworks/ScoreCleanerAudio.framework";
cp -R  build/Frameworks/ScoreCleanerAudio.framework "$SCORECLEANER/Contents/Frameworks/";

open $SCORECLEANER;