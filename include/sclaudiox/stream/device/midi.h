/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_STREAM_DEVICE_MIDI
#define _SCLAUDIOX_STREAM_DEVICE_MIDI

#include "sclaudiox/stream/device.h"
#include "sclaudiox/scheduling/realtime/midi.h"


#define SCL_MIDI_PM_DRIVER_INFO   NULL
#define SCL_MIDI_PM_LATENCY       0

namespace doremir {
namespace scl {

enum MidiStartupMessage
{
    kMidiOpen,
    kMidiClose
};

class SCLAUDIO_API MidiDeviceStreamDescription : public StreamDescription
{
public:        
    StreamType type()
    {
        return (StreamType) ( kDeviceStream & kMidiStream );
    } 
    bool isRealtime()
    {
        return true;
    }
};

/**
    A stream on Midi devices.
 */
class SCLAUDIO_API MidiDeviceStream : public virtual DeviceStream
{
public:
    MidiDeviceStream(MidiDevice* inputDevice,
                     MidiDevice* outputDevice,
                     DeviceStreamOptions options);
        
    ~MidiDeviceStream();
        

    inline MessageScheduler* midiScheduler()
    {
        return midiSchedulerInstance;
    }     

    inline StreamDescription* description()
    {   
        if (!mDescription)
            mDescription = new MidiDeviceStreamDescription();
        return mDescription;
    }


    /**
        Start processing Midi.
     */
    void startMidi();

    /**
        Stop processing Midi.
     */
    void stopMidi();    

private:    
    /** 
        For passing to Portmidi 
     */
    static PmTimestamp timeCallback(void* obj);
                
    void openMidiStreams(PmError* err); 
        
    void closeMidiStreams(PmError* err);

    /**
        Used to circumvent the Portmidi limitation that only one thread
        can open and close input streams.
     */
    static void midiOpenCloseRoutine(MidiDeviceStream* instance);

    MidiDevice    *inputDevice, *outputDevice;
    MidiScheduler *midiSchedulerInstance;
    Thread        *midiThread;

    SendVar<MidiStartupMessage> midiStartupMessage;
    SendVar<PmError> midiStartupResult;
    Thread  *midiOpenCloseThread;
    
    PmStream *inputStream, *outputStream;
};

} // namespace
} // namespace

#endif
