/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_STREAM_DEVICE
#define _SCLAUDIOX_STREAM_DEVICE

#include "sclaudiox/scheduling/dispatching.h"
#include "sclaudiox/scheduling/realtime/action.h"

#include "sclaudiox/util/concurrency.h"
#include "sclaudiox/util/logging.h"

#ifdef SCL_LOG
  #define SCL_LOG_TIMER 100
  #define SCL_LOG_EXEC 100
#endif

#ifndef SCL_DEF_TIMER_INTERVAL
    #define SCL_DEF_TIMER_INTERVAL      5
#endif

#ifndef SCL_DEF_SAMPLE_RATE
    #define SCL_DEF_SAMPLE_RATE         44100
#endif
#ifndef SCL_DEF_AUDIO_BUFFER_SIZE
    #define SCL_DEF_AUDIO_BUFFER_SIZE   256
#endif
#ifndef SCL_DEF_AUDIO_LATENCY
    #define SCL_DEF_AUDIO_LATENCY       0.0
#endif
#ifndef SCL_DEF_MIDI_LATENCY
    #define SCL_DEF_MIDI_LATENCY        0.0
#endif

#ifdef SCL_AUDIO_PREFER_NONBLOCKING
    #define SCL_NONBLOCKING_AUDIO true
#else
    #define SCL_NONBLOCKING_AUDIO false
#endif

#ifdef SCL_AUDIO_PREFER_EXCLUSIVE
    #define SCL_EXCLUSIVE_AUDIO true
#else
    #define SCL_EXCLUSIVE_AUDIO false
#endif

namespace doremir {
namespace scl {

/**
    Options passed to openDeviceStream().
 */
struct DeviceStreamOptions : public Options
{
    /**
        Default constructor.
     */
    DeviceStreamOptions() 
        : sampleRate(SCL_DEF_SAMPLE_RATE)
        , audioBufferSize(SCL_DEF_AUDIO_BUFFER_SIZE)
        , audioLatency(SCL_DEF_AUDIO_LATENCY)
        , midiLatency(SCL_DEF_MIDI_LATENCY)
        , useNonBlocking(SCL_NONBLOCKING_AUDIO)
        , useExclusiveMode(SCL_EXCLUSIVE_AUDIO) {}
                   
    /** 
        Sample rate of the stream. Default is SCL_DEF_SAMPLE_RATE. 
     */
    RealTime sampleRate;

    /** 
        Size of the internal audio buffer used by the stream. Default is SCL_DEF_AUDIO_BUFFER_SIZE. 
     */
    int audioBufferSize;

    /** 
        Suggested latency for audio, if supported by the stream. Default is SCL_DEF_AUDIO_LATENCY. 
     */
    RealTime audioLatency;

    /** 
        Suggested latency for Midi, if supported by the stream. Default is SCL_DEF_MIDI_LATENCY. 
     */
    RealTime midiLatency;

    /** 
        Whether non-blocking audio should be used or not. True by default iff SCL_PREFER_NONBLOCKING_AUDIO is defined. 
     */
    bool useNonBlocking;

    /** 
        Whether audio devices that support exclusive mode should use it. True by default iff SCL_PREFER_EXCLUSIVE_AUDIO is defined. 
     */
    bool useExclusiveMode;
};


/**
    A real-time stream on audio or midi devices.

    This class uses an internal timer for scheduling actions and messages, but
    subclasses can provide their own timing by overriding useInternalTimer().
 */
class SCLAUDIO_API DeviceStream : public Stream, 
                                  public TimeProvider
{       
public:   
    /**
        Constructor.
     */
    DeviceStream(DeviceStreamOptions options) 
        : TimeProvider(options.sampleRate)
        , options(options)
        , errorHandler(NULL)      
        , mDescription(NULL)
        , actionSchedulerInstance(new RealtimeActionScheduler(this))
        , messageSchedulerInstance(new DispatchingScheduler(this))
        , actionExecutorThread(NULL)
        , timerThread(NULL)
        , time(0) {}

    /**
        Destructor.
     */
    ~DeviceStream()
    {                     
        if (mDescription) delete mDescription;
        delete actionSchedulerInstance;
        delete messageSchedulerInstance;
        // TODO clean up surviving threads if the stream is not stopped/was stopped from exception
    }

    
    Time millisecondTime()
    {
        SharedLock lock (timeMutex);
        return time;
    }

    /**
        Returns the sample rate used by this stream.
        \throw
            StreamError
     */
    virtual int sampleRate() = 0;

    /**
        Returns the size of the audio buffer used by this stream.
        \throw
            StreamError
     */
    virtual int audioBufferSize() = 0;
    
    /**
        Returns whether or not an internal timer should be used (true by default). 
        
        Subclasses overriding this method must also override all methods in the 
        TimeProvider interface.
     */
    virtual bool useInternalTimer()
    {
        return true;
    }    

    ActionScheduler* actionScheduler()
    {
        return this->actionSchedulerInstance;
    }

    MessageScheduler* messageScheduler()
    {
        return this->messageSchedulerInstance;
    }
    
    /**
        Set an error handler for threads started by the stream.
     */
    void setHandler(Handler<Error>* handler)
    {
        errorHandler = handler;
    }

    /**
        Stop all threads created by this stream and invoke the error handler.
        This method should be overriden that start up other new threads or 
        otherwise change the state of the stream upon start.
     */
    virtual void handleError(Error& e)
    {
        stopActions();

        if (errorHandler != NULL)
            errorHandler->accept(millisecondTime(), e);
    }    

    /**
        Block until time is updated by a call to notifyTimeUpdated().
     */
    static void blockUntilTimeUpdated(DeviceStream* instance)
    {
        interruptionPoint();
        {
            // TODO shared lock here (?)
            // TODO cache this value locally ?
            UniqueLock lock (instance->timeMutex);
            instance->timeUpdate.wait(lock);
        }
    }
    
    /**
        Allow threads blocking on blockUntilTimeUpdated() to proceed.
     */
    static void notifyTimeUpdated(DeviceStream* instance)
    {
        interruptionPoint();
        instance->timeUpdate.notify_all();
    }


    /**
        An executing routine for the given scheduler.
     */
    static void executor(DeviceStream* instance, RealtimeScheduler* scheduler)
    {
        SCL_WINDOWS_COM_INIT;
        
        try 
        {
            while (true)
            {
                scheduler->executePending();
                blockUntilTimeUpdated(instance);
            }
        }
        catch (ThreadInterrupted& e)
        {
            throw e;
        }
        catch (Error& e) 
        {
            instance->handleError(e);
        }
        catch (...)
        {
#ifdef SCL_LOG_EXEC
            SCL_WRITE_LOG("Unexpected error in executor thread\n");
#endif
        }
    }

    /**
        A routine that updates time periodically.
     */
    static void timer(DeviceStream* instance, int interval)
    {
        SCL_WINDOWS_COM_INIT;

        try
        {
            while (true)
            {
                notifyTimeUpdated(instance);

                sleepMillis(interval);
                {
// FIXME use atomic
                    UniqueLock lock (instance->timeMutex);
                    instance->time += interval;
                }
            }
        }
        catch (ThreadInterrupted& e)
        {
            throw e;
        }
        catch (...)
        {
#ifdef SCL_LOG_TIMER
            SCL_WRITE_LOG("Unexpected error in timer thread\n");
#endif
        }
    }

    /**
        Start executing actions.
     */
    virtual void startActions()
    {
        if (useInternalTimer())
        {
            timerThread = new Thread(timer, this, SCL_DEF_TIMER_INTERVAL);
        }
            
        actionExecutorThread = new Thread(executor, this, actionSchedulerInstance);
    }

    /**
        Stop executing actions.
     */
    virtual void stopActions()
    {
        if (!isCurrentThread(timerThread) && useInternalTimer())
        {
            timerThread->interrupt();
            timerThread->join();
            delete timerThread;
            timerThread = NULL;
        }

        if (!isCurrentThread(actionExecutorThread))
        {
            actionExecutorThread->interrupt();
            actionExecutorThread->join();
            delete actionExecutorThread;
            actionExecutorThread = NULL;
        }
    } 
    
    static Stream* open ( MidiDevice*         midiInput      = NULL,
                          MidiDevice*         midiOutput     = NULL,
                          DeviceStreamOptions options        = DeviceStreamOptions() )
    {
        return open(midiInput, midiOutput, NULL, NULL, NULL, options);
    }

    static Stream* open ( AudioDevice*        audioInput     = NULL,
                          AudioDevice*        audioOutput    = NULL,
                          AudioProcessor*     audioProcessor = NULL,
                          DeviceStreamOptions options        = DeviceStreamOptions() ) 
    {
        return open(NULL, NULL, audioInput, audioOutput, audioProcessor, options);
    }

    /**
        Returns a stream on the given devices or NULL if no devices are provided.                
        \throw 
            PortaudioError, PortmidiError, DspError
     */
    static Stream* open ( MidiDevice*         midiInput      = NULL,
                          MidiDevice*         midiOutput     = NULL,
                          AudioDevice*        audioInput     = NULL,
                          AudioDevice*        audioOutput    = NULL,
                          AudioProcessor*     audioProcessor = NULL,
                          DeviceStreamOptions options        = DeviceStreamOptions() );
    
protected:
    DeviceStreamOptions      options;
    Handler<Error>*          errorHandler;
    StreamDescription *      mDescription;
    
private:    
    RealtimeActionScheduler* actionSchedulerInstance;
    DispatchingScheduler*    messageSchedulerInstance;

    Thread*                  actionExecutorThread;
    Thread*                  timerThread;
    ConditionVariable        timeUpdate;
    SharedMutex              timeMutex;
    Time                     time;
};




} // namespace
} // namespace

#endif
