
# Building {#BuildAndInstall}

@tableofcontents

First ensure that your system meets the [platform](@ref Platform) requirements, and that the necessary
[compiler](@ref Compiler) and [tools](@ref Tools) are available.

# Source code {#SourceCode}

The source code for the Audio Engine can be obtained by.

    $ git clone git@git.doremir.com/repositories/audio-engine.git
    $ cd audio-engine

# A basic build {#Basic}

After changing into the `audio-engine` directory, the `bootstrap` script should be run. This will create a
standard out-of-source build in the `build` subdirectory. After a successful bootstrap, you can run `make help`
to view build commands or `make` to perform the build.

All in all, the simplest possible build session looks like this:

    $ ./bootstrap [options]
    $ make
    $ make test


# Advanced build options {#Advanced}

## Dependencies {#Dependencies}

The build scripts for the Audio Engine include a simple component manager that automatically downloads or builds
the [required libraries](@ref Libraries). The package manager is invoked automatically by the bootstrap script.
If you want to run it again, see the build commands on the form `component ...` below.

Unfortunately the package manager (and CMake in general) *always prefer preinstalled libraries* (that is,
libraries in `/usr/local`) to the managed libraries. This means that if you have any required library installed
on your build machine, that version will be used instead. Currently the only solution is to temporarily
uninstall the libraries.


## Options {#Options}

Build options can be changed using any CMake tool, such as `cmake-gui` or `ccmake`.

You can also change a build setting on the command line.

    $ cmake build -DMY_SETTING=0

### Using a separate build directory {#BuildDir}

If you use `ccmake` or `cmake-gui` you should run them on the `build` directory, which is the default build
directory created by the bootstrap script.

If you want to use another build directory, i.e. for maintaining multiple builds of the same source, you can
rerun the bootstrap script passing the `BUILD_DIRECTORY` flag.

    $ BUILD_DIRECTORY=my_build_dir ./boostrap

### Overview of options {#Overview}

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

The build commands should always be run from the top directory. They will delegate to the `build` directory by
default. You can use the `BUILD_DIRECTORY` flag to override.

### Running the test suite {#Test}

    make test

This command builds and runs the standard test suite. This may take several minutes to complete.

### Building the language bindings {#Bindings}

    make bindings

This command builds the external language bindings in the `bindings/` subdirectory.

### Building the documentation {#Docs}

    make doc

This command builds documentation in the `doc/build` subdirectory. By default both HTML and PDF files are produced.

### Distributing the Audio Engine

    make dist

This command uploads the current build to the DoReMIR package server.

### Resolving dependencies {#ComponentsDownload}

    make components_resolve

This command downloads all dependencies of the Audio Engine from the DoReMIR package server *or* builds them
locally, depending on the current build options. Usually this command is carried out by the bootstrap script.
Using `components_resolve` when the local dependencies are already in place has no effect.

### Removing dependencies {#ComponentsClean}

    make components_clean

This command removes all local dependencies. After this the `components_resolve` command can be used again.


