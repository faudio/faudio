/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
/**
    @file   sclaudiox/device/midi.cc
    @author Hans Hoglund
 */

#include "portmidi.h"
#include "sclaudiox/device/midi.h"

namespace doremir {
namespace scl {
   
struct MidiDeviceData
{
    MidiDevice::Index mIndex;
    PortmidiToken*    mToken;
};

// =============================================================================

PortmidiToken::PortmidiToken() 
{
    PmError err = Pm_Initialize();
    if (err != pmNoError)
        throw PortmidiError(err);
}                

PortmidiToken::~PortmidiToken()
{
    Pm_Terminate();
}

// =============================================================================

String PortmidiError::getErrorString()
{
    switch (mError) 
    {        
        case pmNoError:             return "No error";
        case pmGotData:             return "No error (got data)";

        case pmHostError:           return "Host error";
        case pmInvalidDeviceId:     return "Invalid device ID";
        case pmInsufficientMemory:  return "Insufficient memory";
        case pmBufferTooSmall:      return "Buffer too small";
        case pmBufferOverflow:      return "Buffer overflow";

        case pmBadPtr:              return "Bad stream pointer";
        case pmBadData:             return "Bad data";
        case pmInternalError:       return "Internal error";
        case pmBufferMaxSize:       return "Buffer max size";
        
        default:                    return "Unknown error";
    }
}

String PortmidiError::message () 
{
    String msg = getErrorString();
    return "Portmidi: " + msg;
}

PortmidiError::Code PortmidiError::errorCode() 
{ 
    return mError;
}

// =============================================================================

MidiDevice::MidiDevice(MidiDeviceData* data) 
    : mData(new MidiDeviceData)
    {                         
        mData = data;
        acquire(mData->mToken);
    }

MidiDevice::~MidiDevice() 
{                             
    release(mData->mToken);
    delete mData;
}  

namespace 
{
    MidiDevice* createDevice(MidiDevice::Index index, PortmidiToken* token)
    {
        MidiDeviceData data;
        data.mIndex = index;
        data.mToken = token;
        return new MidiDevice(&data);
    }
}

String MidiDevice::name()
{
    return fromSimpleString<Portmidi::characterSet>(Pm_GetDeviceInfo(mData->mIndex)->name);
}

MidiDevice::Index MidiDevice::index()
{
    return mData->mIndex;
}

String MidiDevice::hostName()
{
    return fromSimpleString<Portmidi::characterSet>(Pm_GetDeviceInfo(mData->mIndex)->interf);
}

bool MidiDevice::hasInput()
{
    return Pm_GetDeviceInfo(mData->mIndex)->input;
}

bool MidiDevice::hasOutput()
{
    return Pm_GetDeviceInfo(mData->mIndex)->output;
}

std::list<MidiDevice*> MidiDevice::devices()
{
    PortmidiToken* token = new PortmidiToken;
    int num = Pm_CountDevices();

    std::list<MidiDevice*> lst;
    for (int i = 0; i < num; ++i)
        lst.push_back(createDevice(i, token));
    
    return lst;
}

MidiDevice* MidiDevice::defaultInputDevice()
{
    PortmidiToken* token = new PortmidiToken;
    int id = Pm_GetDefaultInputDeviceID();

    if (id < 0)
    {
        delete token;
        return NULL;
    }
    else
    {
        return createDevice(id, token);
    }
}

MidiDevice* MidiDevice::defaultOutputDevice()
{
    PortmidiToken* token = new PortmidiToken;
    int id = Pm_GetDefaultOutputDeviceID();
    
    if (id < 0)
    {
        delete token;
        return NULL;
    }
    else
    {
        return createDevice(id, token);
    }
}

} // namespace
} // namespace

