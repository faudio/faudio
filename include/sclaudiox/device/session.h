/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef  _SCLAUDIOX_DEVICE_SESSION
#define  _SCLAUDIOX_DEVICE_SESSION

#include "sclaudiox/error.h"
#include "sclaudiox/defines.h"


namespace doremir {
namespace scl {

class AudioHost;
class AudioDevice;
class MidiDevice;


SCL_DECLARE_CONDITION(InvalidSessionState);


/**
    A set of available devices. 
    
    This class abstracts over single state APIs such as Portaudio and Portmidi 
    to provide multiple views of the devices available on the system while not
    compromising functionality of the APIs.
 */
class Session : public NonCopyable, public Resource
{      
public:        
    /**
        Creates a new session, providing a fresh snapshot of available devices.
        
        Generally, at most one session can be active at a time. Therefore, constructing
        a new session may have the side effect of closing a previous session. This may
        not be possible, as the current session may not be in a valid state for closing
        (for example, it may be in use by a running stream), in which case an 
        InvalidSessionState is thrown from this constructor.

        \throw
            InvalidSessionState
            If the current session can not be closed. 
        \throw
            PortaudioError
            If initialization of Portaudio failed.
        \throw
            PortmidiError
            If initialization of Portmidi failed.
     */
    Session(){}

    /**
        Destructor.
     */
    ~Session() {}


    /** 
        Returns a list of all audio hosts currently available.
        \throw
            AudioError 
      */
    std::list<AudioHost*> audioHosts();    

    /** 
        Returns the default audio host.
        \throw
            AudioError
      */
    AudioHost* defaultAudioHost();
    
    /**
        Returns the default audio input device.
        \throw
            AudioError
     */
    AudioDevice* defaultAudioInputDevice();

    /**
        Returns the default audio output device.
        \throw
            AudioError
     */
    AudioDevice* defaultAudioOutputDevice();

    /**
        Returns a list of all Midi devices currently available on this host.
        \throw
            MidiError
     */
    std::list<MidiDevice*> devices();

    /**
        Returns the default Midi input device.
        \throw
            MidiError
     */
    MidiDevice* defaultMidiInputDevice();

    /**
        Returns the default Midi output device.
        \throw 
            MidiError
     */
    MidiDevice* defaultMidiOutputDevice();

    /**
        Close the session immedeatly.
        \throw 
            InvalidSessionState
     */
    void close();
};


} // namespace
} // namespace

#endif
