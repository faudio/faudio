
Build and install
=========================

Prerequisites
------------

### Dependencies

  * Boost
  * Portaudio (optional, for real-time audio)
  * Portmidi (optional, for real-time midi)
  * libsndfile (optional, for file streams)
  * Fluidsynth (optional, for built in synthesis)
  * Google Test (optional, for running the test suite)
  * CoreAudio, AudioUnit, AudioToolbox (Mac OS X only, for audio units)

### Build tools

  * CMake 2.8 or later, generating one of the following build systems:
    * GNU Make with GCC 4.2 or later
    * XCode with GCC 4.2 or later
    * Visual Studio C++ 2010 or later
  
  * Pandoc (optional, for documentation)
  * Doxygen (optional, for documentation)
  * GNU Make (optional, for Lisp bindings and documentation)

Fetching the source code
----------

    $ git clone --recursive git@notes.doremir.com:/repositories/audio-engine.git
    $ cd audio-engine

To update, you can do

    $ cd audio-engine
    $ git pull
    $ git submodule update


Building the dependencies
----------

### Portaudio

    $ cd external_libraries/portaudio
    # ?
    $ ./configure
    $ make
    # no install, just copy lib/.libs/libportaudio.a

### Portmidi

    $ cd external_libraries/portmidi
    $ mkdir build
    $ cd build
    $ cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_OSX_ARCHITECTURE=i386
    $ make
    # no install, just copy build/libportmidi_s.a

FIXME


Building the Audio Engine
----------

### Makefile build:
    
    $ mkdir build
    $ cd build
    $ cmake ..

This creates a default build. To see and review the build options interactively, do:

    $ cmake -i ..

The values you select are stored in the `CMakeCache.txt` file in the current directory. To change them you can run the wizard again, edit the file directly, or do:

    $ cmake -MY_OPTION_NAME=my_option_value ..

To switch between debug and release builds, do one of the following:

    $ cmake -DCMAKE_BUILD_TYPE=Debug ..
    $ cmake -DCMAKE_BUILD_TYPE=Release ..

To run the build, do:

    $ make
    
### Xcode build:

    $ mkdir build
    $ cd build

To create and open a default build, do:

    $ cmake -G Xcode ..
    $ open *.xcodeproj

To see and review the build options interactively, do:

    $ cmake -i ..

The values you select are stored in the `CMakeCache.txt` file in the current directory. To change them you can run the wizard again, edit the file directly, or do:

    $ cmake -DMY_OPTION_NAME=my_option_value ..

If you update the the options cache you must overwrite the Xcode project with a fresh one by running `cmake -G Xcode` again.
                                         

### Visual Studio build

    > mkdir build
    > cd build

To create and open a default build, do:

    > cmake -G "Visual Studio 10" ..
    > open *.sln

To see and review the build options interactively, do:

    > cmake -i ..

The values you select are stored in the `CMakeCache.txt` file in the current directory. To change them you can run the wizard again, edit the file directly, or do:

    > cmake -DMY_OPTION_NAME=my_option_value ..

If you update the the options cache you must overwrite the Xcode project with a fresh one by running `cmake -G "Visual Studio 10"` again.

In Visual Studio choose Project > Build Solution.


Running the unit tests
----------

To run unit tests as part of the build, set the CMake option `RUN_SCLAUDIO_TESTS` to `ON`, then run `make` again. The unit test can be run separately as `build/bin/sclaudio_tests`. Beware that the whole test suite may take several minutes to complete.


Building the language bindings
----------

TODO


Building the documentation
----------

TODO


Linking the audio engine into another application
----------

By default, dynamic libraries are built. On Mac OS X this is a framework called ScoreCleanerAudio.framework and a dynamic library called libsclaudio.dylib; either may be used. On Windows, a library file named sclaudio.lib and a dynamic library file named sclaudio.dll is generated. You can load any of these dynamically, using the appropriate system-specific API.

