/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_STREAM_DEVICE_AUDIO_ONLY
#define _SCLAUDIOX_STREAM_DEVICE_AUDIO_ONLY

#include "sclaudiox/stream/device/audio.h"

namespace doremir {
namespace scl {

/**
    Implementation of Stream supporting real-time audio.
 */
class SCLAUDIO_API AudioOnlyDeviceStream : public AudioDeviceStream
{
public:
    AudioOnlyDeviceStream(AudioDevice*        audioInput,
                          AudioDevice*        audioOutput,
                          AudioProcessor*     processor,
                          DeviceStreamOptions options) 
        : DeviceStream(options)
        , AudioDeviceStream(audioInput, audioOutput, processor, options)
        , status(Initial) {}
    
    ~AudioOnlyDeviceStream()
    {
        try 
        {
            if (status == Started) abort();
        }
        catch (Error& e) {}
    }

    MessageScheduler* midiScheduler()
    {
        throw StreamError("Midi not available for this stream.");
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
                startAudio();
                startActions();
            } 
            catch (PortaudioError& e)
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
                stopAudio();
                stopActions();
            }
            catch (PortaudioError& e) 
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
                abortAudio();
                stopActions();
            }
            catch (PortaudioError& e) 
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
            try { abortAudio(); } catch (...) {}

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
