#!/bin/bash

rm -rf  ~/Desktop/ScoreCleaner.app/Contents/Frameworks/ScoreCleanerAudio.framework;
cp -R   ~/audio/build/ScoreCleanerAudio.framework ~/Desktop/ScoreCleaner.app/Contents/Frameworks/;

open    ~/Desktop/ScoreCleaner.app;
#~/Desktop/ScoreCleaner.app/Contents/MacOS/ScoreCleaner