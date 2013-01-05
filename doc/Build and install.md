
# Build and install {#BuildAndInstall}

[TOC]

The source code is stored in the repository `audio-engine.git`. Its dependencies are stored in
separate repositories which are submodules to the main repository. To fetch the source code for the
audio engine and its dependencies, perform a recursive clone in Git:

    $ git clone --recursive git@git.doremir.com:repositories/audio-engine
    $ cd audio-engine

You should be able to build using

    $ ./bootstrap
    $ make

The bootstrap script may fail and ask for certain options, in which case you should add them to the
bootstrap command. After bootstrap has finished, you can always change build options using `cmake`,
`ccmake` or `cmake-gui`.


# Commands {#BuildCommands}

## Setting up the build environment {#SettingUp}

    ./bootstrap
    
This will perform sanity checks and set up the dependencies by downloading or building, depending
on the value of the `BUILD_COMPONENTS` flag. You can always repeat those steps as per below.

## Downloading dependencies {#ComponentsDownload}

    make components_clean
    make edit_cache -DBUILD_COMPONENTS=0
    make components_resolve

Usually carried out by the bootstrap script. Downloads dependencies from the package server.

## Building dependencies {#ComponentsBuild}

    make components_clean
    make edit_cache -DBUILD_COMPONENTS=1
    make components_resolve

Usually carried out by the bootstrap script. Builds dependencies locally.

## Removing dependencies {#ComponentsClean}

    make components_clean

Remoevs all dependencies.

## Building the audio engine {#All}

    make

## Testing the audio engine {#Test}

    make test

    make run_scorecleaner

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

