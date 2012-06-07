
ScoreCleaner Audio Engine

The ScoreCleaner Audio Engine is a software environment for cross-platform audio and
signal processing. It supports concurrent real-time and non-realtime audio streams,
as well as sample accurate scheduling of messages to and from audio processors.

Audio processors such as analyzers, filters and synths can be written in C or C++, or
composed from primitive processors using one of the supported programming language
bindings (currently Lisp, C and C++). Real-time audio input and output is supported via
Portaudio and Portmidi. Non-realtime file input and output is supported via libSndFile.
The audio engine can also wrap popular audio plugin architectures such as AU and VST.

Devices 
    The engine supports cross-platform enumeration and lookup of audio devices (typically
    hardware interfaces). The interaction with Portaudio and Portmidi is encapsulated
    by the Session class, which provides the oppertunity to refresh the view of currently
    available devices.
         
Scheduling
    The engine supports scheduling of incoming and outgoing messages. Messages are
    possibly nested dynamic lists, which can be passed to arbitrary receivers with
    sample-accurate timing. Such messages can deliver well-known formats such as 
    Midi or any custom control protocol.
    
Processor
    
    
Streams
    The engine supports real-time streams to and from audio devices. It also supports
    faster or slower-than realtime streams to and from files and memory buffers.
    Non-realtime streams use the same scheduling interface as real-time streams, but
    provides other implementation possibilities, including multiprocessor audio
    rendering.
    