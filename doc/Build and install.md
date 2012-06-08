
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
                                                  
  * Doxygen (for all documentation)
  * LaTeX (for PDF documentation)
  * Haskell Platform or GHC (for generating the Lisp documentation)


Basic build
----------

All dependencies included except:
  
  * Boost headers: Download or install and put a symlink from the boost header directory to `external/boost`
  * Standard OS X frameworks (CoreAudio etc): Expected to be in `/System/Library/Frameworks`

For command line build:
    
    $ mkdir build
    $ cd build
    $ cmake ..
    $ make
    
For Xcode build:

    $ mkdir build
    $ cd build
    $ cmake -G Xcode ..

Then open generated project and build.
                                     
To build with non-standard options (logging, no unicode etc), pass the `-i` flag to cmake.
    

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



