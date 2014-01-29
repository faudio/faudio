
# Build and install {#BuildAndInstall}

@tableofcontents


# Prerequisites {#Prerequisites}

@anchor Prerequisites
@tableofcontents
 
The following tools and libraries are required to build *faudio*.

# Platform {#Platform} 

Supported platforms are:

  * Windows XP or later
  * Mac OS X 10.5 or later

Porting to Linux should be relatively easy but has not been done yet.

## Compiler {#Compiler}

A compiler supporting the C99 standard is required. Currently tested are:

  * clang 3.1   or later (Apple call this compiler "4.0.0 based of SVN 3.1")
  * GCC   4.6.2 or later
  
The default Windows compiler (Visual C/C++) is not well supported, the MinGW version of GCC is recommended.



## Tools {#Tools}

The following tools are required to build *faudio*.

### CMake

<http://www.cmake.org>

Required for building the libraries and binaries.

At present the *only* supported generators are Makefiles on Linux OS X, and MSYS Makefiles on
Windows. Cygwin Makefiles *might* work, but is not well tested, and not recommended as the Cygwin
compiler might not generate standard Windows binaries.

### Modulo

<https://github.com/hanshoglund/modulo>

Required for generating headers and low-level language bindings.

<!--
### Dist

<https://github.com/hanshoglund/dist>

Required for automatic package resolution. Not required if dependencies are built locally or available through some other package
management system.
-->

### Doxygen

<http://www.stack.nl/~dimitri/doxygen>

Required for generating the documentation.

### GraphViz

<http://www.graphviz.org/>

Optionally used for generating graphs.

### ImageMagick

<http://www.imagemagick.org/script/index.php>

Optionally used for converting graphs.

### GNU Make

Required for building language bindings and documentation.




## Libraries {#Libraries}

@anchor Libraries

These are typically handled by the package mangager, see [build and install](@ref SourceCode).

### Portaudio

<http://www.portaudio.com>

Required for real-time audio streams on all systems.

### Portmidi                                       

<http://portmedia.sourceforge.net>

Required for real-time midi streams on all systems.

### Libsndfile

<http://www.mega-nerd.com/libsndfile>

Required for non-real-time audio streams on all systems.

### libogg + libvorbis

### Fluidsynth

<http://sourceforge.net/apps/trac/fluidsynth>

Required for using the Fluidsynth audio processor.


# Setup build

TODO get source code

TODO run setup script with correct options

    $ ./setup.sh -DCMAKE_OSX_ARCHITECTURES=i386 -DCMAKE_BUILD_TYPE=Debug -DCMAKE_OSX_DEPLOYMENT_TARGET=1

or

    $ ./setup.sh -G "MSYS Makefiles"                                                                             

This will download and build dependencies.

TODO if you have another version of a dependency (such as a 32-bit version while building for 64 bits or vice versa) installed (in /usr/local etc) you may need to temporarily uninstall it before running the setup script (you may install again after configuring though, building does not requiring uninstall).

# Building {#Commands}

The build commands should always be run from the top directory. They will delegate
to the `build` directory by default. You can use the `BUILD_DIRECTORY` flag to
override.

    make test

This command builds and runs the standard test suite. This may take several minutes
to complete.

    make modules

This command builds most of the headers in the `include/` subdirectory.

    make bindings

This command builds the external language bindings in the `bindings/` subdirectory.

    make doc


# Configuring builds
    
Build options can be changed using any CMake tool, such as `cmake-gui` or `ccmake`.

You can also change a build setting on the command line. These changes will persist
until you change them again, or regenerate the build directory.

    $ cmake build -DMY_SETTING=0

