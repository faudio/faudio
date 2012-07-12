
Build and install
=========================


Dependencies
------------

### Required

  * Boost Thread
  * Portaudio (optional, for real-time audio)
  * Portmidi (optional, for real-time midi)
  * libsndfile (optional, for file streams)
  * Fluidsynth (optional, for built in synthesis)
  * Google Test (optional, for running the test suite)
  * CoreAudio, AudioUnit, AudioToolbox (Mac OS X only, for audio units)

### Build tools

  * CMake 2.8 or later  
  * pkg-config (for compiling GLib)
  * Supported build systems:
    * Make/bash GCC 4.2 or later
    * XCode 3.2 or later
    * Visual Studio C++ 2010 
  * Make/bash is *required* for building the Lisp bindings (and documentation)
  
### Documentation tools

  * Make
  * Doxygen (for documentation)
  * The Haskell Platform with the Pandoc packages (for generating the Lisp documentation)

Fetching the source code
----------

The source code, as well as sources for the documentation and language bindings is stored in the repository `audio-engine.git`. Its dependencies are stored in separate repositories which are linked into the main repository as submodules. To fetch the source code for the audio engine and its dependencies, perform a recursive clone in Git:

    $ git clone --recursive git@git.doremir.com:/repositories/audio-engine.git
    $ cd audio-engine

The last command will change your directory to the checked out repository. All following commands assume that you are already standing in this directory.

To update, you can do:

    $ git pull
    $ git submodule update

Building the dependencies
----------

Usually, the dependencies can simply be fetched from the package server by running `dist get --all`. This will
download a precompiled version of each dependency. If you definitely need to build a dependency, follow the steps
below.

### Mac OS X

Below is a step-by-step instruction for building the dependencies on Mac OS X. We try to build all dependencies as univeral binaries containing static libraries for both the 32-bit (i386) and 64-bit architectures (x86_64). In some cases this is not possible, so we have to build 32 and 64-bit versions separately. We also try to build everything locally, to avoid differences in machine configurations etc. If you want to install some library to your local machine (i.e. to `/usr/local` or similar), you should do a separate build or use a package manager such as Macports or Homebrew.

*Mac OS X Lion note:* Several of the dependency builds require that the system SDKs reside in `/Developer/SDKs`, which may not be the case in Mac 10.7 or later. Before attempting to build you must create a symbolic link to the actual location (which may vary depending on your OS) like so:

    sudo ln -s /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer /


#### Portaudio

To build Portaudio on Mac OS X, simply run the configure script and use the generated Makefile.

    $ cd external_libraries/portaudio
    $ ./configure --prefix=`pwd`/result
    $ make install

To get all headers, you need to do:

    $ cp -R include/ result/include/

Portaudio builds a universal binary containing i386 and x86_84 by default.

#### Portmidi

    $ cd external_libraries/portmidi
    $ mkdir result
    $ cd result                  
    $ cmake .. \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY="" \
        -DCMAKE_LIBRARY_OUTPUT_DIRECTORY="" \
        -DCMAKE_RUNTIME_OUTPUT_DIRECTORY="" \
        -DCMAKE_OSX_ARCHITECTURES="i386;x86_64"
    $ make
    
This builds a universal binary containing i386 and x86_84.

#### Sndfile

    $ cd external_libraries/sndfile
    $ CFLAGS="-arch i386 -I /Developer/SDKs/MacOSX10.7.sdk/Developer/Headers/FlatCarbon/" \
      CXXFLAGS="-arch i386" \
      LDFLAGS="-arch i386" \
      ./configure --prefix=`pwd`/result
    $ make install

The current build can not build universal libraries, so the i386 architecture must be specified.

#### Fluidsynth

The CMake script requires pkg-config. Install this using your favourite package manager.

We use a checked-in version of GLib. You can update this via the package manager and copy the files as follows (replacing `/usr/local` with the appropriate path):

    $ cd external_libraries/glib
    $ rm -f include/*
    $ rm -f lib/*
    $ find /usr/local/lib/libg* | xargs -J % cp % lib/
    $ cp -R /usr/local/include/glib-2.0/ include/

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

To switch between debug and release builds, do:

    $ cmake -DCMAKE_BUILD_TYPE=Debug ..

or 

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

    $ cmake -MY_OPTION_NAME=my_option_value ..

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

    > cmake -MY_OPTION_NAME=my_option_value ..

If you update the the options cache you must overwrite the Xcode project with a fresh one by running `cmake -G "Visual Studio 10"` again.

In Visual Studio choose Project > Build Solution.


Running the unit tests
----------

To run unit tests as part of the build, set the CMake option `RUN_SCLAUDIO_TESTS` to non-false, then run `make` again. The unit test can be run separately as `build/bin/sclaudio_tests`. Beware that the whole test suite may take several minutes to complete.


Building the language bindings
----------

TODO


Building the documentation
----------

TODO


Likning the audio engine into ScoreCleaner
----------

TODO


