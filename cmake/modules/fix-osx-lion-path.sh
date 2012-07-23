#!/bin/bash                                                               
sudo -k; # Force authentication
sudo ln -s `xcode-select -print-path`/Platforms/MacOSX.platform/Developer /Developer;