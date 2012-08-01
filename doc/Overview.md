Overview
==========

This page is a simple overview of the architecture and implementation of the audio engine.

The audio engine is implemented in C++ with bindings available for other languages. There is a C level API providing wrapper functions for C++ methods and classes. This allow the C++ API to be consumed by any language that interfaces with C.

Files
----------

    doc/                            Documentation
    external/                       External libraries
    bindings/                       Bindings to other languages
    scl/                            C++ Header files
        atomic/                     Cross platform atomic ops, C++0x subset
        thread/                     Cross platform threads, C++0x subset
        future/                     Cross platform futures, C++0x superset!
            future.h
            shared_future.h
            interruptible_future.h
        fifo/
            fifo.h
        parallel_range/
            parallel_range.h
        immutable/
            immutable_list.h
            immutable_sequence.h
        logging/
            logger.h
        audio/
            device/
                session.h
                device.h
                audio/
                    portaudio_session.h
                    portaudio_device.h
                midi/
                    portaudio_session.h
                    portmidi_device.h
                file/
                    sndfile_device.h
            scheduler/
                scheduler.h
                sync_scheduler.h
                async_scheduler.h
            processor/
                processor.h
                simple/  
                    identity.h
                    constant.h
                    random.h
                    unary.h
                    binary.h
                    ternary.h
                compound/
                    sequence.h
                    parallel.h
                    feedback.h
                    delay.h
                synth/
                    fluidsynth.h
                analysis/
                    cuex.h
                plugin/
                    au.h
                    nil.h
                    vst2.h
            stream/
                stream.h
                realtime/
                    portaudio_stream.h
                    portmidi_stream.h
                nonrealtime/
                    sndfile_stream.h

        
    scl-c/                          C Header files
    libs/                           Sources for libraries
        atomic/
            src/
            test/
        thread/
            src/
            test/
        parallel_range/
            src/
            test/
        refcount/
            src/
            test/
        immutable/
            src/
            test/
        logging
            src/
            test/
        audio/
            device/
                src/
                test/
            processor/
                src/
                test/
            scheduling/
                src/
                test/
            stream/
                src/
                test/
    test/                           Sources for unit tests
        scl-audio-tests/
            main.cc
    tools/                          Sources for tools
        scl-sftool/
            main.cc
        scl-audio-httpd/
            main.cc
        scl-audio-oscd/
            main.cc
            
