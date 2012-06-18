
Build and install
=========================


Dependencies
------------

### Required

  * Boost (Thread, DateTime)

### Optional                          

  * Portaudio (for real-time audio)
  * Portmidi (for real-time midi)
  * libsndfile (for file streams)
  * Fluidsynth (for built in synthesis)
  * Google Test (for running the test suite)

### Build tools

  * CMake 2.8 or later
  * Supported build systems:
    * GNU Make 3.8 with GCC 4.2
    * Apple Developer Tools
    * Visual Studio C++ 2010 
  
### Documentation tools

  * GNU Make
  * Doxygen (for documentation)
  * Haskell Platform or GHC (for generating the Lisp documentation)

Fetching the source code
----------

    $ git clone --recursive git@notes.doremir.com:/repositories/audio-engine.git
    $ cd audio-engine

To update, you can do

    $ cd audio-engine
    $ git pull --rebase
    $ git submodule update


Building the dependencies
----------

### Portaudio

    $ cd external_libraries/portaudio
    -- flags etc?
    $ ./configure
    $ make
    -- no install, just copy lib/.libs/libportaudio.a

### Portmidi

    $ cd external_libraries/portmidi
    $ mkdir build
    $ cd build
    $ cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_OSX_ARCHITECTURE=i386
    $ make
    -- no install, just copy build/libportmidi_s.a

### Sndfile
### GTest    


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
                                         

Windows build
-------------

Boost must be installed, typically by downloading an installer from http://www.boostpro.com/download/.

Install to `C:\Program\boost`, or change path in ./CMakeList.txt.

    > mkdir build
    > cd build
    > cmake -G "Visual Studio 10" ..
    
To compile, open the generated `.sln` file from inside Visual Studio and choose Project > Build Solution.


Unit tests
----------

To run unit tests as part of the build, set the CMake option `RUN_SCLAUDIO_TESTS` to non-false.



