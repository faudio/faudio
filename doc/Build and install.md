
# Build and install {#BuildAndInstall}

@tableofcontents
@note
    This page is under construction.

# Basic build {#BasicBuild}

First ensure that your system meets the [platform](@ref Platform) requirements, and
that the necessary [compiler](@ref Compiler) and [tools](@ref Tools) are available.

The source code for Fae can be obtained by.

    $ git clone git@github.com:fae/fae.git
    $ cd audio-engine

After changing into the `audio-engine` directory, the `bootstrap` script should be
run. This will create a standard out-of-source build in the `build` subdirectory.
After a successful bootstrap, you can run `make help` to view build commands or
`make` to perform the build.

All in all, the simplest possible build session looks like this:

    $ ./bootstrap [options]
    $ make
    $ make test


# Advanced build {#AdvancedBuild}

## Building local dependencies {#Dependencies}

The bootstrap script automatically downloads the [required libraries](@ref Libraries). 
You can also build libraries locally, using the component commands
described below.


## Setting and inspecting options {#SettingOptions}

Build options can be changed using any CMake tool, such as `cmake-gui` or `ccmake`.

You can also change a build setting on the command line. These changes will persist
until you change them again, or regenerate the build directory.

    $ cmake build -DMY_SETTING=0

## Using a separate build directory {#SeparateBuildDir}

If you use `ccmake` or `cmake-gui` you should run them on the `build` directory,
which is the default build directory created by the bootstrap script.

If you want to use another build directory, i.e. for maintaining multiple builds of
the same source, you can rerun the bootstrap script passing the `BUILD_DIRECTORY`
flag.

    $ BUILD_DIRECTORY=my_build_dir ./bootstrap

# Rebuilding headers and bindings {#Rebuilding}

TODO

# Running and testing {#Running}

TODO

# Distributing the build {#Distributing}

TODO


# Reference {#Reference}

## Options {#Options}

Name                      | Description
--------------------------|-----------------------------------------
CMAKE_BUILD_TYPE          | Either `Debug`, `Release` or `RelWithDebInfo`
ENABLE_REALTIME_AUDIO     | Enable real-time audio
ENABLE_REALTIME_MIDI      | Enable real-time midi
ENABLE_NONREALTIME_AUDIO  | Enable non-real-time audio
ENABLE_FLUIDSYNTH         | Enable the FluidSynth processor
ENABLE_AUDIO_UNIT         | Enable AudioUnit processors
ENABLE_VST                | Enable VST processors
BUILD_FRAMEWORK           | Build an OS X framework
BUILD_SHARED              | Build a shared library
BUILD_TESTS               | Build unit tests
PROFILING                 | Compile with profiler flags -pg set
BUILD_COMPONENTS          | Build dependent components locally
SHOW_COMPONENT_OUTPUT     | Show output while building components


## Commands {#Commands}

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

This command builds documentation in the `doc/build` subdirectory. By default both
HTML and PDF files are produced.

    make dist

