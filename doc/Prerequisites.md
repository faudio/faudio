
# Prerequisites {#Prerequisites}

@anchor Prerequisites
@tableofcontents
 
The following tools and libraries are required to build the Audio Engine.

# Platform {#Platform} 

Supported platforms are:

  * Windows XP or later
  * Mac OS X 10.5 or later

Porting to Linux should be relatively easy but has not been done yet.

The Audio Engine is *strictly* 32-bit at the moment. This requires some precautions when cross-compiling on a 64-bit
system, in particular all library dependencies must also be compiled in 32-bit (or universal) mode. If you get
unexpected linking errors, you likely have the wrong version of some library.

# Compiler {#Compiler}

A compiler supporting the C99 standard is required. Currently tested are:

  * clang 3.1   or later (Apple call this compiler "4.0.0 based of SVN 3.1")
  * GCC   4.6.2 or later
  
The default Windows compiler (Visual C/C++) is not well supported, the MinGW version of GCC is recommended.



# Tools {#Tools}

The following tools are required to build the Audio Engine.

### CMake

<http://www.cmake.org>

Required for building the libraries and binaries.

At present the *only* supported generators are Makefiles on Linux OS X, and MSYS Makefiles on
Windows. Cygwin Makefiles *might* work, but is not well tested, and not recommended as the Cygwin
compiler might not generate standard Windows binaries.

### Modulo

<https://github.com/hanshoglund/modulo>

Required for generating headers and low-level language bindings.

### Dist

<https://github.com/hanshoglund/dist>

Required for automatic package resolution. Not required if dependencies are built locally or available through some other package
management system.

### Doxygen

<http://www.stack.nl/~dimitri/doxygen>

Required for generating the documentation.

### GraphViz

<http://www.graphviz.org/>

Optionally used for generating graphs.

### gnuplot

<http://gnuplot.sourceforge.net/>

Optionally used for generating plots.

### ImageMagick

<http://www.imagemagick.org/script/index.php>

Optionally used for converting graphs.

### GhostScript

<http://pages.cs.wisc.edu/~ghost/>

Optionally used for converting plots.

### GNU Make

Required for building language bindings and documentation.



# Libraries {#Libraries}

@anchor Libraries

These are typically handled by the package mangager, see [build and install](@ref SourceCode).

### Portaudio

<http://www.portaudio.com>
<http://portmedia.sourceforge.net>

Required for real-time audio streams on all systems.

### Portmidi                                       

<http://portmedia.sourceforge.net>

Required for real-time midi streams on all systems.

### Libsndfile

<http://www.mega-nerd.com/libsndfile>

Required for non-real-time audio streams on all systems.

### Fluidsynth

<http://sourceforge.net/apps/trac/fluidsynth>

Required for using the Fluidsynth audio processor.


               