/*
    ScoreCleaner Audio Engine

    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
/**
    @file   sclaudiox/device/audio.cc
    @author Hans Hoglund
 */

#include "portaudio.h"
#include "sclaudiox/device/audio.h"

namespace doremir { 
namespace scl {

struct AudioHostData
{   
    PaHostApiIndex  mIndex;
    PortaudioToken* mToken;
};

struct AudioDeviceData
{
    PaDeviceIndex   mIndex;
    PortaudioToken* mToken;
};


// =======================================================================================

String PortaudioError::errorString()
{
    switch (mError)
    {
        case paNoError:                         return "No error";

        case paNotInitialized:                  return "Not initialized";
        case paUnanticipatedHostError:          return "Unanticipated Host Error";
        case paInvalidChannelCount:             return "Invalid channel count";
        case paInvalidSampleRate:               return "Invalid sample rate";
        case paInvalidDevice:                   return "Invalid device";
        case paInvalidFlag:                     return "Invalid flag";
        case paSampleFormatNotSupported:        return "Sample format not supported";
        case paBadIODeviceCombination:          return "Bad IO device combination";
        case paInsufficientMemory:              return "Insufficient memory";
        case paBufferTooBig:                    return "Buffer too big";
        case paBufferTooSmall:                  return "Buffer too small";

        case paNullCallback:                    return "Null callback";
        case paBadStreamPtr:                    return "Bad stream pointer";
        case paTimedOut:                        return "Timed out";
        case paInternalError:                   return "Internal error";
        case paDeviceUnavailable:               return "Device unavailable";
        case paIncompatibleHostApiSpecificStreamInfo:
                                                return "Incompatible host API specific stream info";
        case paStreamIsStopped:                 return "Stream is stopped";
        case paStreamIsNotStopped:              return "Stream is not stopped";
        case paInputOverflowed:                 return "Input overflowed";
        case paOutputUnderflowed:               return "Output underflowed";

        case paHostApiNotFound:                 return "Host API not found";
        case paInvalidHostApi:                  return "Invalid host API";
        case paCanNotReadFromACallbackStream:   return "Can not read from a callback stream";
        case paCanNotWriteToACallbackStream:    return "Can not write to a callback stream";
        case paCanNotReadFromAnOutputOnlyStream:
                                                return "Can not read from an output only stream";
        case paCanNotWriteToAnInputOnlyStream:  return "Can not write to an input only stream";
        case paIncompatibleStreamHostApi:       return "Incompatible stream host API";
        case paBadBufferPtr:                    return "Bad buffer pointer";

        default:                                return "Unknown error.";
    }
}

String PortaudioError::message ()
{
    String msg = errorString();
    return "Portaudio: " + msg;
}

int PortaudioError::errorCode()
{
    return mError;
}

// =======================================================================================

PortaudioToken::PortaudioToken()
{
    PaError err = Pa_Initialize();
    if (err != paNoError)
        throw PortaudioError(err);
}

PortaudioToken::~PortaudioToken() 
{
    Pa_Terminate();
}


// =======================================================================================

AudioHost::AudioHost(Index           index, 
                     PortaudioToken* token) 
    : mData(new AudioHostData) 
{ 
    mData->mIndex = index;
    mData->mToken = token;
    acquire(mData->mToken); 
}

AudioHost::~AudioHost()
{ 
    release(mData->mToken); 
    delete mData;
}

int AudioHost::index()
{
    return mData->mIndex;
}

AudioHost::Type AudioHost::type()
{
    return Pa_GetHostApiInfo(mData->mIndex)->type;
}

String AudioHost::name()
{
    return fromSimpleString<Portaudio::characterSet>(Pa_GetHostApiInfo(mData->mIndex)->name);
}

int AudioHost::numberOfDevices()
{
    return Pa_GetHostApiInfo(mData->mIndex)->deviceCount;
}

bool AudioHost::isExclusive()
{
    switch(type())
    {
        case paASIO:    return true;
        default:        return false;
        // Note: not WASAPI
    }
}      

std::list<AudioDevice*> AudioHost::devices()
{
    int num = this->numberOfDevices();

    std::list<AudioDevice*> lst;

    for (int i = 0; i < num; ++i)
    {
        int index = Pa_HostApiDeviceIndexToDeviceIndex(mData->mIndex, i);
        lst.push_back(new AudioDevice(index, mData->mToken));
    }

    return lst;
}

std::list<AudioHost*> AudioHost::hosts()
{
    PortaudioToken* token = new PortaudioToken;
    int num = Pa_GetHostApiCount();

    std::list<AudioHost*> lst;

    for (int i = 0; i < num; ++i)
    {
        lst.push_back(new AudioHost(i, token));
    }

    return lst;
}

AudioHost* AudioHost::defaultHost()
{
    PortaudioToken* token = new PortaudioToken;
    return new AudioHost(Pa_GetDefaultHostApi(), token);
}

// =======================================================================================

AudioDevice::AudioDevice(Index           index, 
                         PortaudioToken* token)
    : mData(new AudioDeviceData)
{
    mData->mIndex = index;
    mData->mToken = token;
    acquire(mData->mToken);
}

AudioDevice::~AudioDevice()
{ 
    release(mData->mToken); 
    delete mData;
}

int AudioDevice::index()
{
    return mData->mIndex;
}

String AudioDevice::name()
{
    return fromSimpleString<Portaudio::characterSet>(Pa_GetDeviceInfo(mData->mIndex)->name);
}

AudioHost* AudioDevice::host()
{
    return new AudioHost(Pa_GetDeviceInfo(mData->mIndex)->hostApi, mData->mToken);
}

int AudioDevice::numberOfInputs()
{
    return Pa_GetDeviceInfo(mData->mIndex)->maxInputChannels;
}

int AudioDevice::numberOfOutputs()
{
    return Pa_GetDeviceInfo(mData->mIndex)->maxOutputChannels;
}

RealTime AudioDevice::lowInputLatency()
{
    return Pa_GetDeviceInfo(mData->mIndex)->defaultLowInputLatency;
}

RealTime AudioDevice::highInputLatency()
{
    return Pa_GetDeviceInfo(mData->mIndex)->defaultHighInputLatency;
}

RealTime AudioDevice::lowOutputLatency()
{
    return Pa_GetDeviceInfo(mData->mIndex)->defaultLowOutputLatency;
}

RealTime AudioDevice::highOutputLatency()
{
    return Pa_GetDeviceInfo(mData->mIndex)->defaultHighOutputLatency;
}

RealTime AudioDevice::sampleRate()
{
    return Pa_GetDeviceInfo(mData->mIndex)->defaultSampleRate;
}

AudioDevice* AudioDevice::defaultInputDevice()
{
    PortaudioToken* token = new PortaudioToken;
    return new AudioDevice(Pa_GetDefaultInputDevice(), token);
}

AudioDevice* AudioDevice::defaultOutputDevice()
{
    PortaudioToken* token = new PortaudioToken;
    return new AudioDevice(Pa_GetDefaultOutputDevice(), token);
}     

} // namespace
} // namespace
