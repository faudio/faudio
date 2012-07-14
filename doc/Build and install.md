
Build and install
=========================

Building the audio engine should be as simple as:

    $ git clone --recursive git@git.doremir.com:/repositories/audio-engine.git
    $ cd audio-engine
    $ ./boot && make
    $ sudo make install

To test it with an instance of ScoreCleaner on the desktop, use:

    $ make run-scorecleaner


Prerequisites
------------

#### Dependencies

### Required

  * Boost

### Optional

  * Portaudio (for real-time audio)
  * Portmidi (for real-time midi)
  * libsndfile (for non-realtime audio)
  * Fluidsynth (for built in synthesis)     
  * libfft (for FFT and IFFT processors)
  * libosc++ (for OSC messages)
  * Google Test (for running the test suite)

### Build tools

#### Required

  * CMake

#### Optional

  * Pandoc (for documentation)
  * Doxygen (for documentation)
  * GNU Make (for Lisp bindings)

### Compilers

Any of the following:

  * clang >= 3.1
  * GCC >= 4.4
  * MSVC >= 10.0


Fetching the source code
----------

The source code, as well as sources for the documentation and language bindings is stored in the repository `audio-engine.git`. Its dependencies are stored in separate repositories which are linked into the main repository as submodules. To fetch the source code for the audio engine and its dependencies, perform a recursive clone in Git:

    $ git clone --recursive git@git.doremir.com:/repositories/audio-engine.git
    $ cd audio-engine

The last command will change your directory to the checked out repository. All following commands assume that you are already in this directory.

To update, you can do:

    $ git pull
    $ git submodule update

Fetching the dependencies
----------

Usually, the dependencies can simply be fetched from the package server by running `dist get --all`. This will
download a precompiled version of each required library. The following instructions apply if you need to build a particular library from source.

Building the dependencies
----------

Whenever possible, we try to build all dependencies as universal binaries containing static libraries for both 32 and 64-bit architectures. In some cases this is not possible, so we have to build 32 and 64-bit versions separately. We also try to build everything locally, to avoid depending on the configuration of a particular 
system for the main build. You may or may not have these libraries installed on your local system; this should not interfere with the build. 

Note that some dependencies (i.e. Boost) are source only – they do not require a separate build. Yet other dependencies are included in the operating system.


### Mac OS X

Several of the dependency builds require that the system SDKs reside in `/Developer/SDKs`, which may not be the case in Mac 10.7 or later. Before attempting to build on these systems you must create a symbolic link to the actual location (which may vary depending on your OS) like so:

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

FIXME This requires a proper recompilation of 32-bit GLib.
We currently have a working 32-bit framework.

#### GTest    

FIXME We currently have a working 32-bit library.

#### Boost

FIXME



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

