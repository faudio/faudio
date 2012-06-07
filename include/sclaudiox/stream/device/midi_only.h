/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_STREAM_DEVICE_MIDI_ONLY
#define _SCLAUDIOX_STREAM_DEVICE_MIDI_ONLY

#include "sclaudiox/stream/device/midi.h"

namespace doremir {
namespace scl {

/**
    Implementation of Stream supporting real-time Midi.
 */
class SCLAUDIO_API MidiOnlyDeviceStream : public MidiDeviceStream
{
public:
    MidiOnlyDeviceStream(MidiDevice*         midiInput,
                         MidiDevice*         midiOutput,
                         DeviceStreamOptions options) 
        : DeviceStream(options)
        , MidiDeviceStream(midiInput, midiOutput, options)
        , status(Initial) {}
    
    ~MidiOnlyDeviceStream()
    {
        try 
        {
            if (status == Started) abort();
        }
        catch (Error& e) {}
    }

    MessageScheduler* audioScheduler()
    {
        throw StreamError("Audio not available for this stream.");
    }

    int sampleRate()
    {
        throw StreamError("Audio not available for this stream.");
    }

    int audioBufferSize()
    {
        throw StreamError("Audio not available for this stream.");
    }
    
    bool running()
    {
        Lock lock (mutex);
        bool running = status == Started;
        return running;
    }

    void start()
    {
        Lock lock (mutex);
        if (status != Started)
        {
            try 
            {
                startMidi();
                startActions();
            } 
            catch (PortmidiError& e)
            {
                throw e;
            }
        }
        status = Started;
    }
    
    void stop()
    {
        Lock lock (mutex);

        if (status == Started)
        {
            status = Stopped;
            try 
            {
                stopMidi();
                stopActions();
            } 
            catch (PortmidiError& e)
            {
                stopActions();
                throw e;
            }
        }
    }

    void abort()
    {
        Lock lock (mutex);

        if (status == Started)
        {
            status = Aborted;
            try 
            {
                stopMidi();
                stopActions();
            } 
            catch (PortmidiError& e)
            {
                stopActions();
                throw e;
            }
        }
    }
    
    virtual void handleError(Error& e)
    {
        Lock lock (mutex);

        if (status == Started)
        {
            status = Failed;
            stopActions();
            try { stopMidi(); } catch (...) {}

            if (errorHandler != NULL)
                errorHandler->accept(millisecondTime(), e);
        }
    }    
    
private:
    Mutex mutex;
    enum { Initial, 
           Started, 
           Stopped, 
           Aborted,
           Failed } status;
};

} // namespace
} // namespace

#endif
