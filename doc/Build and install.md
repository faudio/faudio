
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

    $ git clone --recursive git@git.doremir.com:/repositories/audio-engine.git
    $ cd audio-engine

The last command will change your directory to the checked out repository. All following commands assume that you are already standing in this directory.

To update, do:

    $ git pull
    $ git submodule update

Fetching the dependencies
----------

Usually, the dependencies can simply be fetched from the package server by running `dist get -a`. This will
download precompiled versions of the dependencies. If you definately need to build a dependency, follow the steps
below.

*Note on Mac OS X Lion:* Several of the dependency builds depends on the system SDKs being in `/Developer/SDKs`, which may not be the case in Mac OS 10.7 or later. If a build fails, create a symbolic link to the actual location (may vary depending on your OS version) like so:

    sudo ln -s /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer /


#### Portaudio

To build Portaudio on Mac OS X, simply run the configure script and use the generated Makefile. You should install it into `external_libraries/portaudio/results` instead of the default path.

    $ cd external_libraries/portaudio
    $ ./configure --prefix=`pwd`/result
    $ make install

#### Portmidi

    $ cd external_libraries/portmidi
    $ mkdir build
    $ cd build
    
FIXME

#### Sndfile

    $ CFLAGS="-arch i386 -I /Developer/SDKs/MacOSX10.7.sdk/Developer/Headers/FlatCarbon/" \
      CXXFLAGS="-arch i386" \
      LDFLAGS="-arch i386" \
      ./configure --prefix=`pwd`/result
    $ make install


#### Fluidsynth

FIXME

#### GTest    

FIXME

#### Boost

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

