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

    openMidiSignal.notify_all();
    UniqueLock lock (midiOpenCloseDoneMutex);
    {
        openMidiDone.wait(lock);
    }          
    if (midiOpenCloseError != pmNoError) 
        throw PortmidiError(midiOpenCloseError);
    
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

    closeMidiSignal.notify_all();
    UniqueLock lock (midiOpenCloseDoneMutex);
    {
        closeMidiDone.wait(lock);
    }       
    if (midiOpenCloseError != pmNoError) 
        throw PortmidiError(midiOpenCloseError);
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

void MidiDeviceStream::midiOpenCloseRoutine(MidiDeviceStream* instance) 
{
    while (true) 
    {
        UniqueLock lock (instance->midiOpenCloseMutex);

        instance->openMidiSignal.wait(lock);
        instance->openMidiStreams(&instance->midiOpenCloseError);
        instance->openMidiDone.notify_all();

        instance->closeMidiSignal.wait(lock);
        instance->closeMidiStreams(&instance->midiOpenCloseError);            
        instance->closeMidiDone.notify_all();
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
