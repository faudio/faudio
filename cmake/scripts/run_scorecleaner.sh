#!/bin/bash

if [ -z "$SCORECLEANER" ]
then
    APP="/Applications/ScoreCloud.app"
	NAME=ScoreCloud
fi

tail -f "$HOME/Library/Logs/ScoreCloud/faudio.log" &
$APP/Contents/MacOS/$NAME
killall tail