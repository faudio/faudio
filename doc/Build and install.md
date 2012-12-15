
Build and install
=========================

The source code is stored in the repository `audio-engine.git`. Its dependencies are stored in separate
repositories which are submodules to the main repository. To fetch the source code for the audio engine and
its dependencies, perform a recursive clone in Git:

    $ git clone --recursive git@git.doremir.com:repositories/audio-engine
    $ cd audio-engine

You should be able to build using

    $ ./bootstrap
    $ make

The bootstrap script may fail and ask for certain options, in which case you should add them to the
bootstrap command. After bootstrap has finished, you can always change build options using `cmake`, 
`ccmake` or `cmake-gui`.


Build commands
----------

### Setting up the build environment

    ./bootstrap
    
This will perform sanity checks and set up the dependencies by downloading or building, depending on
the value of the `BUILD_COMPONENTS` flag. You can always repeat those steps as per below.

### Downloading dependencies

    make components_clean
    make edit_cache -DBUILD_COMPONENTS=0
    make components_resolve

Usually carried out by the bootstrap script. Downloads dependencies from the package server.

### Building dependencies

    make components_clean
    make edit_cache -DBUILD_COMPONENTS=1
    make components_resolve

Usually carried out by the bootstrap script. Builds dependencies locally.

### Removing dependencies

    make components_clean

Remoevs all dependencies.

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



Prerequisites
------------

### Platforms

Currently supported:

* Mac OS X 10.5 or later (32-bit)
* Windows XP or later (32-bit)


### Compilers

A compiler supporting the C99 standard is required. Currently tested are:

  * clang 3.1   or later (Apple call this compiler "4.0.0 based of SVN 3.1")
  * GCC   4.6.2 or later

### Libraries

#### Portaudio

Required for real-time audio streams on all systems. Disable by setting `ENABLE_REALTIME_AUDIO=0`.

#### Portmidi                                       

Required for real-time midi streams on all systems. Disable by setting `ENABLE_REALTIME_MIDI=0`.

#### Libsndfile

Required for non-real-time audio streams on all systems. Disable by setting `ENABLE_NONREALTIME_AUDIO=0`.

#### Fluidsynth

Required for using the Fluidsynth audio processor. Disable by setting `ENABLE_FLUIDSYNTH=0`.

#### System-specific code

The audio engine automatically include certain system headers on both OS X and Windows.



### Build tools

#### CMake

Required for building the libraries and binaries.

Supported generators are Unix makefiles and XCode on OS X, and MinGW or MSYS Makefiles on Windows. Cygwin
might work, but beware that the Cygwin/GCC compiler might not generate standard Windows binaries.

#### A Unix shell

Required for running certain build scripts, and for building language bindings and documentation. On Windows the
MinGW/MSYS shell is tested, but Cygwin might work as well.

#### Dist

Required for automatic package resolution. If you prefer to build packages locally, set
`BUILD_COMPONENTS=1`. In this case `dist` is not required.

#### Doxygen

Required for generating the detailed API documentation (HTML or PDF).

<!--
#### Pandoc

Required for generating the reference manual (HTML or PDF).
-->     
\pagebreak

