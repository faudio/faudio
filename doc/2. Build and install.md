
# Build and install {#BuildAndInstall}

[TOC]

# Basic build {#Basic}

First ensure that your system meets the [platform](@ref Platform) requirements, and that the necessary 
[compiler](@ref Compiler) and [tools](@ref Tools) are available.

## Source code {#SourceCode}

The source code for the Audio Engine can be obtained by.

    $ git clone git@git.doremir.com/repositories/audio-engine.git
    $ cd audio-engine

## Setting up the build environment {#Bootstrap}

After changing into the `audio-engine` directory, the `bootstrap` script should be run. This will create a
standard out-of-source build in the `build` subdirectory. 

<!-- The `bootstrap` script may fail and request options. The script will typically suggest what flags to add,
for example on OS X `CMAKE_OSX_ARCHITECTURES=i386` is typically required. -->

## Running the build {#BasicBuild}

After a successful bootstrap, you can run `make help` to view build commands or `make` to perform the build.

All in all, the simplest possible build session looks like this:
    
    $ ./bootstrap [options]
    $ make
    $ make test




# Advanced build {#BuildCommands}

## Testing the audio engine {#Test}

    make test

FIXME


## Building the language bindings {#Bindings}

    make bindings

FIXME

## Building the documentation {#Docs}

    make documentation

FIXME

## Distributing the audio engine

    make distribute

FIXME

## Downloading dependencies {#ComponentsDownload}

    make components_resolve

Usually carried out by the bootstrap script. Downloads dependencies from the package server.

## Building dependencies {#ComponentsBuild}

    make components_resolve

Usually carried out by the bootstrap script. Builds dependencies locally.

## Removing dependencies {#ComponentsClean}

    make components_clean

Remoevs all dependencies.

## Building the audio engine {#All}

    make


