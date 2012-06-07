/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef  _SCLAUDIOX_DEVICE_MIDI
#define  _SCLAUDIOX_DEVICE_MIDI

#include "sclaudiox/error.h"
#include "sclaudiox/control.h"
#include "sclaudiox/defines.h"
#include "sclaudiox/device/session.h"

#define SCL_MIDI_INPUT_BUFFER_SIZE  256
#define SCL_MIDI_OUTPUT_BUFFER_SIZE 256


namespace doremir {
namespace scl {


/**
    Static information about the Portmidi backend.
 */
class SCLAUDIO_API Portmidi
{
public:
    /**
        The character set used by Portmidi.
     */
    static const CharacterSet characterSet = kDefaultCharSet;
};


// =============================================================================

/** 
    An error in Portmidi 
 */
class SCLAUDIO_API PortmidiError 
    : public Error
{
public:                     
    typedef int Code;

    PortmidiError(Code error) 
        : mError(error) {}

    /**
       Creates a standard error message by prepending 'Portmidi:' to the
       specific error returned by errorString().
     */
    String message () const;

    /**
        Returns the Portmidi error code.
     */
    Code errorCode();        
    
    /**
        Returns a string describing the specific error.
     */
    String getErrorString() const;

private:
    const Code mError;
};


// =============================================================================

/**
    A token granting access to Portmidi.
  */
class SCLAUDIO_API PortmidiToken 
    : public NonCopyable
    , public Resource
{
public: 
    /**   
        Creates a token, possibly starting up Portmidi.
        \throws PortmidiError
     */
    PortmidiToken();

    /**
        Destroys this token, possibly terminating Portmidi.
     */
    ~PortmidiToken();
};

    
// =============================================================================

class MidiDeviceData;

/**
    Represents a Midi device.
 */
class SCLAUDIO_API MidiDevice 
    : public NonCopyable
    , public Resource
{
public:
    typedef int Index;
    
    MidiDevice(MidiDeviceData* data);
    ~MidiDevice();

    /**
        Returns the index of this device.
     */
    Index index();
    
    /**
        Returns the name of this device.
     */
    String name();
    
    /**
        Returns the name of the host to which this device belongs.
     */
    String hostName();
    
    /**
        Returns whether this device supports Midi input.
     */
    bool hasInput();
    
    /**
        Returns whether this device supports Midi output.
     */
    bool hasOutput();

    /**
        Returns a list of all Midi devices currently available on this host.
        \throw PortmidiError
     */
    static std::list<MidiDevice*> devices();

    /**
        Returns the default Midi input device.
        \throw 
            PortmidiError
     */
    static MidiDevice* defaultInputDevice();

    /**
        Returns the default Midi output device.
        \throw
            PortmidiError
     */
    static MidiDevice* defaultOutputDevice();

private:    
    MidiDeviceData* mData;
};

} // namespace
} // namespace

#endif
