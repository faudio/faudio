/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef  _SCLAUDIOX_DEVICE_AUDIO
#define  _SCLAUDIOX_DEVICE_AUDIO

#include "sclaudiox/core.h"
#include "sclaudiox/error.h"
#include "sclaudiox/defines.h"
#include "sclaudiox/device/session.h"

namespace doremir {
namespace scl {

class SCLAUDIO_API AudioDevice;


// =============================================================================

/**
    Static information about the Portaudio backend.
 */
class SCLAUDIO_API Portaudio
{

public:                            
    /**
        The character set used by Portaudio.
     */
#ifdef SCL_WIN
    static const CharacterSet characterSet = DefaultCharacterSet;
#else
    static const CharacterSet characterSet = kUtf8;
#endif
};


// =============================================================================

/** 
    An error in Portaudio
 */
class SCLAUDIO_API PortaudioError : public Error
{
public:
    typedef int Code;

    /**
        Constructor.
     */
    PortaudioError(Code error) 
        : mError(error) {}
            
    /**
       Creates a standard error message by prepending 'Portaudio:' to the
       specific error returned by errorString().
     */
    String message();

    /**
        Returns the Portaudio error code.
     */
    Code errorCode();

    /**
        Returns a string describing the specific error.
     */
    String errorString();

private:
    Code mError;
};


// =============================================================================

/**
    A token granting access to Portaudio. Resources referencing an instance of
    this class SCLAUDIO_API should release it after use to allow portaudio to terminate.
 */
class SCLAUDIO_API PortaudioToken 
    : public NonCopyable
    , public Resource
{
public:
    /**   
        Creates a token, possibly starting up Portaudio.
        \throw PortaudioError
     */
    PortaudioToken();

    /**
        Destroys this token, possibly terminating Portaudio.
     */
    ~PortaudioToken();
};


// =============================================================================

class AudioHostData;

/**
    Represents an audio host, such as CoreAudio, ASIO or WASAPI.
 */
class SCLAUDIO_API AudioHost : public NonCopyable, public Resource
{
public:     
    /**
        Destructor.
     */
    ~AudioHost();
   
    /**
        An internal index identifying the host.
     */
    typedef int Index;

    /**
        An internal index identifying the type of host.
     */
    typedef int Type;

    /**
        Returns the index of this device.
     */
    Index index();  

    /**
        Returns type of this host.
     */
    Type type();

    /**
        Returns name of this host.
     */
    String name();

    /**
        Returns whether this host requires exclusive access or not.
     */
    bool isExclusive();

    /**
        Returns the number of devices currently available on this host.
     */
    int numberOfDevices();    

    /**
        Returns a list of all audio devices currently available on this host.
     */
    std::list<AudioDevice*> devices();

    /** 
        Returns a list of all audio hosts currently available.
        \throw PortaudioError 
      */
    static std::list<AudioHost*> hosts();    

    /** 
        Returns the default audio host.
        \throw PortaudioError
      */
    static AudioHost* defaultHost();

private:
    friend class AudioDevice;
    AudioHost(Index index, PortaudioToken* token);
    AudioHostData*  mData;
};       


// =============================================================================

class AudioDeviceData;

/**
    Represents an audio device, such as a sound card or a virtual device.
 */
class SCLAUDIO_API AudioDevice : public NonCopyable, public Resource
{
public:            
    /**
        Destructor.
     */
    ~AudioDevice();
    
    /**
        An internal index identifying the underlying device.
     */
    typedef int Index;
    
    /**
        Returns the index of this device.
     */
    Index index();
    
    /**
        Returns the name of this device.
     */
    String name();
    
    /**
        Returns the host to which this device belongs.
     */
    AudioHost* host();
    
    /**
        Returns the number of input channels supported by this device.
     */
    int numberOfInputs();
    
    /**
        Returns the number of output channels supported by this device.
     */
    int numberOfOutputs();    
        
    /**
        Returns the lowest amount of input latency supported by this device.
     */
    RealTime lowInputLatency();
    
    /**
        Returns the highest amount of input latency supported by this device.
     */
    RealTime highInputLatency();
    
    /**
        Returns the lowest amount of output latency supported by this device.
     */
    RealTime lowOutputLatency();
    
    /**
        Returns the highest amount of output latency supported by this device.
     */
    RealTime highOutputLatency();
   
    /**
        Returns the default sample rate for this device.
     */
    RealTime sampleRate(); 

    /**
        Returns the default audio input device.
        \throw PortaudioError
     */
    static AudioDevice* defaultInputDevice();

    /**
        Returns the default audio output device.
        \throw PortaudioError
     */
    static AudioDevice* defaultOutputDevice();

private:
    friend class AudioHost;
    AudioDevice(Index index, PortaudioToken* token);
    AudioDeviceData* mData;
};

} // namespace
} // namespace

#endif
