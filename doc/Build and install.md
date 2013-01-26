
# Build and Install {#BuildAndInstall}

@tableofcontents
@note
    This page is under construction.

# Basic build {#BasicBuild}

First ensure that your system meets the [platform](@ref Platform) requirements, and
that the necessary [compiler](@ref Compiler) and [tools](@ref Tools) are available.

The source code for the Audio Engine can be obtained by.

    $ git clone git@git.doremir.com/repositories/audio-engine.git
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

The bootstrap script automatically downloads the [required libraries](@ref
Libraries). You can also build libraries locally, using the component commands
described below.

<!-- 
A caveat is that CMake tend to prefer libraries in system paths to libraries
in your build directory. Currently the only solution is to temporarily uninstall
the libraries. 
-->

## Setting and inspecting options {#SettingOptions}

Build options can be changed using any CMake tool, such as `cmake-gui` or `ccmake`.

You can also change a build setting on the command line.

    $ cmake build -DMY_SETTING=0

## Using a separate build directory {#SeparateBuildDir}

If you use `ccmake` or `cmake-gui` you should run them on the `build` directory,
which is the default build directory created by the bootstrap script.

If you want to use another build directory, i.e. for maintaining multiple builds of
the same source, you can rerun the bootstrap script passing the `BUILD_DIRECTORY`
flag.

    $ BUILD_DIRECTORY=my_build_dir ./boostrap


# Running and testing {#Running}

TODO

# Distributing the build {#Distributing}

TODO


# Reference {#Reference}

## Options {#Options}

Name                      | Description
--------------------------|-----------------------------------------
ENABLE_REALTIME_AUDIO     | Enable real-time audio
ENABLE_REALTIME_MIDI      | Enable real-time midi
ENABLE_NONREALTIME_AUDIO  | Enable non-real-time audio
ENABLE_FLUIDSYNTH         | Enable the FluidSynth processor
ENABLE_AUDIO_UNIT         | Enable AudioUnit processors
ENABLE_VST                | Enable VST processors
ENABLE_LOGGING            | Enable standard logging
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

    make bindings

This command builds the external language bindings in the `bindings/` subdirectory.

    make doc

This command builds documentation in the `doc/build` subdirectory. By default both
HTML and PDF files are produced.

    make dist

This command uploads the current build to the DoReMIR package server.

    make components_resolve

This command downloads all dependencies of the Audio Engine from the DoReMIR
package server *or* builds them locally, depending on the current build options.
Usually this command is carried out by the bootstrap script. Using
`components_resolve` when the local dependencies are already in place has no
effect.

    make components_clean

This command removes all local dependencies. After this the `components_resolve`
command can be used again.

