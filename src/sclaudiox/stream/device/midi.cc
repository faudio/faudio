/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
/**
    @file   sclaudiox/stream/device/midi.cc
    @author Hans Hoglund
 */

#include "portmidi.h"
#include "sclaudiox/stream/device/midi.h"

namespace doremir {
namespace scl {

struct MidiDeviceIndex
{
    PmDeviceID mId;
};


void MidiDeviceStream::startMidi()
{
#ifdef SCL_LOG_MIDI
	{
        ReentrantLock lock (printMutex());
        SCL_WRITE_LOG("Began starting MIDI\n"
              << "    Input device: "  << (inputDevice == NULL ? "N/A" : inputDevice->name())   << "\n"
              << "    Output device: " << (outputDevice == NULL ? "N/A" : outputDevice->name()) << "\n");
	}    
#endif      
    midiStartupMessage.put(kMidiOpen);

    PmError result;
    midiStartupResult.take(&result);    
    SCL_WRITE_LOG("  Receving from Midi control thread\n");

    if (result != pmNoError) 
        throw PortmidiError(result);
    
    midiThread = new Thread(executor, this, midiSchedulerInstance);
}

void MidiDeviceStream::stopMidi()
{
    if (!isCurrentThread(midiThread))
    {
        midiThread->interrupt();
        midiThread->join();
        midiThread = NULL;
    }       
    midiStartupMessage.put(kMidiClose);

    PmError result;
    midiStartupResult.take(&result);
    if (result != pmNoError) 
        throw PortmidiError(result);
}


void MidiDeviceStream::midiOpenCloseRoutine(MidiDeviceStream* instance) 
{
    while (true) 
    {   
        MidiStartupMessage message;
        PmError result;

        instance->midiStartupMessage.take(&message);
        switch (message) 
        {
        case kMidiOpen:
            SCL_WRITE_LOG("  Midi control thread received start\n");
            instance->openMidiStreams(&result);
            break;
        case kMidiClose:
            SCL_WRITE_LOG("  Midi control thread received stop\n");
            instance->closeMidiStreams(&result);
            break;
        default:
            SCL_WRITE_LOG("  Midi control thread received unknown message\n");            
        }
        instance->midiStartupResult.put(result);
    }
}         




PmTimestamp MidiDeviceStream::timeCallback(void* obj)
{
    return ((MidiDeviceStream*) obj)->millisecondTime();
}
            

// =============================================================================

void MidiDeviceStream::openMidiStreams(PmError* err)
{
    if (inputDevice != NULL)
    {
        *err = Pm_OpenInput(
            &inputStream, 
            inputDevice->index()->mId, 
            SCL_MIDI_PM_DRIVER_INFO, 
            SCL_MIDI_INPUT_BUFFER_SIZE, 
            (PmTimeProcPtr) timeCallback, 
            this);
    }

    if (outputDevice != NULL)
    {
        *err = Pm_OpenOutput(
            &outputStream, 
            outputDevice->index()->mId, 
            SCL_MIDI_PM_DRIVER_INFO, 
            SCL_MIDI_OUTPUT_BUFFER_SIZE, 
            (PmTimeProcPtr) timeCallback, 
            this, 
            SCL_MIDI_PM_LATENCY);
    }
}       
    
void MidiDeviceStream::closeMidiStreams(PmError* err)
{                     
    if (inputDevice != NULL)
    {
        *err = Pm_Close(inputStream);
    }

    if (outputDevice != NULL)
    {
        *err = Pm_Close(outputStream);
    }
}



MidiDeviceStream::MidiDeviceStream(MidiDevice* inputDevice,
                                   MidiDevice* outputDevice,
                                   DeviceStreamOptions options)
    : DeviceStream(options) 
    , inputDevice(inputDevice)
    , outputDevice(outputDevice)
    , midiSchedulerInstance(NULL)
    , midiThread(NULL)
    , midiOpenCloseThread(NULL)
    , inputStream(NULL)
    , outputStream(NULL)
{
    midiSchedulerInstance = new MidiScheduler(this, &inputStream, &outputStream);
    midiOpenCloseThread = new Thread(midiOpenCloseRoutine, this);
}


MidiDeviceStream::~MidiDeviceStream()
{
    delete midiSchedulerInstance;
    
    midiOpenCloseThread->interrupt();
    midiOpenCloseThread->join();
    delete midiOpenCloseThread;
    // TODO detach instead of join?
}

    
} // namespace    
} // namespace
