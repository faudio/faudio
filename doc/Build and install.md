
Build and install
=========================

Prerequisites
------------

### Platforms

Currently supported:

* Mac OS X 10.5 (32-bit)
* Windows XP or later (32-bit)


### Compilers

A compiler supporting the C99 standard is required. Currently tested are:

  * clang 3.1   or later (Apple call this compiler "4.0.0 based of SVN 3.1")
  * GCC   4.6.2 or later

### Libraries

#### Portaudio

Required for real-time audio streams on all systems. Disable by setting `DOREMIR_ENABLE_PORTAUDIO=0`.

#### Portmidi                                       

Required for real-time midi streams on all systems. Disable by setting `DOREMIR_ENABLE_PORTMIDI=0`.

#### Fluidsynth

Required for using the Fluidsynth audio processor. Disable by setting `DOREMIR_ENABLE_FLUIDSYNTH=0`.

#### Libsndfile

Required for non-real-time audio streams on all systems. Disable by setting `DOREMIR_ENABLE_SNDFILE=0`.

#### System-specific code

The audio engine automatically include certain system headers on both OS X and Windows. You can control this explicitly
by setting `DOREMIR_ENABLE_OSX` or `DOREMIR_ENABLE_WIN`.

#### Google Test

Required for running the unit tests.
 


### Build tools

#### CMake

Required for building the libraries and binaries.

Supported generators are Unix makefiles and XCode on OS X, and MinGW or MSYS Makefiles on Windows. Cygwin might work,
but beware that the Cygwin/GCC compiler might not generate standard Windows binaries.

#### A Unix shell

Required for running certain build scripts, and for building language bindings and documentation. On Windows the
MinGW/MSYS shell is tested, but Cygwin might work as well.

#### Dist

Required for automatic package resolution. If you prefer to build packages locally, set `SCL_BUILD_PACKAGES=1`. In this
case Dist is not required.

#### Pandoc

Required for generating the reference manual (HTML or PDF).
  
#### Doxygen

Required for generating the detailed API documentation (HTML or PDF).



Step by step
----------

### Downloading the source code

The source code, as well as sources for the documentation and language bindings is stored in the repository
`audio-engine.git`. Its dependencies are stored in separate repositories which are linked into the main repository as
submodules. To fetch the source code for the audio engine and its dependencies, perform a recursive clone in Git:

    $ git clone --recursive git@git.doremir.com:repositories/audio-engine
    $ cd audio-engine

The last command will change your directory to the checked out repository. All following commands assume that you are
already in this directory.

### Setting up the build environment

    ./bootstrap
    make help

### Downloading dependencies

    make components_resolve

FIXME

### Building dependencies

    make edit_cache -DBUILD_COMPONENTS=1
    make components_resolve

FIXME                

Whenever possible, we try to build all dependencies as universal binaries containing static libraries for both 32 and
64-bit architectures. In some cases this is not possible, so we have to build 32 and 64-bit versions separately. We also
try to build everything locally, to avoid depending on the configuration of a particular system for the main build. You
may or may not have these libraries installed on your local system; this should not interfere with the build.

Note that some dependencies (i.e. Boost) are source only – they do not require a separate build. Yet other dependencies
are included in the operating system.

### Building the audio engine

    make

### Testing the audio engine

    make test

    make run_scorecleaner

FIXME


### Building the language bindings

    make bindings

FIXME

### Building the documentation

    make documentation

FIXME

### Distributing the audio engine

    make distribute

FIXME



\pagebreak

