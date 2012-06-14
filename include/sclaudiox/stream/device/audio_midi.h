/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_STREAM_DEVICE_AUDIO_MIDI
#define _SCLAUDIOX_STREAM_DEVICE_AUDIO_MIDI

#include "sclaudiox/stream/device/audio.h"
#include "sclaudiox/stream/device/midi.h"

namespace doremir {
namespace scl {

class SCLAUDIO_API AudioMidiDeviceStreamDescription : public StreamDescription
{
public:        
    StreamType type()
    {
        return (StreamType) ( kDeviceStream & kMidiStream & kAudioStream );
    }   
    bool isRealtime()
    {
        return true;
    }
};

/**
    Implementation of Stream supporting real-time audio and Midi.
 */
class SCLAUDIO_API AudioMidiDeviceStream : public MidiDeviceStream, 
                                           public AudioDeviceStream
{
public:
    AudioMidiDeviceStream(MidiDevice*         midiInput,
                          MidiDevice*         midiOutput, 
                          AudioDevice*        audioInput,
                          AudioDevice*        audioOutput,
                          AudioProcessor*     processor,
                          DeviceStreamOptions options) 
        : DeviceStream(options)
        , MidiDeviceStream(midiInput, midiOutput, options)
        , AudioDeviceStream(audioInput, audioOutput, processor, options)
        , status(Initial) {}
    
    ~AudioMidiDeviceStream()
    {
        try 
        {
            if (status == Started) abort();
        }
        catch (Error& e) {}
    }  
    
    inline StreamDescription* description()
    {   
        if (!mDescription)
            mDescription = new AudioMidiDeviceStreamDescription();
        return mDescription;
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
                startAudio();
                startActions();
            } 
            catch (PortmidiError& e)
            {
                throw e;
            }
            catch (PortaudioError& e)
            {
                try { stopMidi(); } catch (PortmidiError& e){}
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
                stopAudio();
                stopActions();
            }
            catch (PortmidiError& e) 
            {
                try { stopAudio(); } catch (PortaudioError& e){}
                stopActions();
                throw e;
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
                stopMidi();
                abortAudio();
                stopActions();
            }
            catch (PortmidiError& e) 
            {
                try { abortAudio(); } catch (PortaudioError& e){}
                stopActions();
                throw e;
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
            try { stopMidi(); } catch (...) {}
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
