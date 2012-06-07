/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_STREAM_DEVICE_AUDIO
#define _SCLAUDIOX_STREAM_DEVICE_AUDIO

#include "portaudio.h" // TODO remove

#include "sclaudiox/stream/device.h"

#include "sclaudiox/scheduling/realtime/audio.h"

#include "sclaudiox/util/queue.h"
#include "sclaudiox/util/concurrency.h"
#include "sclaudiox/util/foreach.h"

namespace doremir {
namespace scl {

class SCLAUDIO_API AudioDeviceStreamDescription : public StreamDescription
{
public:        
    StreamType type()
    {
        kDeviceStream & kAudioStream;
    }      
    bool isRealtime()
    {
        return true;
    }
};

/**
    A stream on audio devices.
 */
class SCLAUDIO_API AudioDeviceStream : public virtual DeviceStream
{
public:
    AudioDeviceStream(AudioDevice*        inputDevice,
                      AudioDevice*        outputDevice,
                      AudioProcessor*     processor,
                      DeviceStreamOptions options);

    ~AudioDeviceStream();

    
    /**
        Start processing audio.
     */
    void startAudio();

    /**
        Stop processing audio (without draining buffers).
     */
    void abortAudio();

    /**
        Stop processing audio (draining buffers).
     */
    void stopAudio();

    MessageScheduler* audioScheduler();
    int sampleRate();
    int audioBufferSize();
    Time sampleTime();
    Time millisecondTime();
    bool useInternalTimer();

    inline StreamDescription* description()
    {   
        // FIXME
        return new AudioDeviceStreamDescription();
    }

private:

    PaError openAudioStream(bool isNonBlocking, bool isExclusiveMode);

    // Non-blocking audio
     
    void prepareNonBlocking();


    /**
       A routine that manages messages to and from audio code.

       Incoming messages are put into the incomingMessages queue and outgoing
       messages read from the outgoingMessages queue.
     */
    static void messageHandler(AudioDeviceStream* instance);
    
    /**
        Audio thread callback, passed to the non-blocking API.
     */
    static int audioProcessingCallback( const void *input, 
                                        void *output, 
                                        unsigned long frameCount, 
                                        const PaStreamCallbackTimeInfo *timeInfo, 
                                        PaStreamCallbackFlags statusFlags, 
                                        void *data );

    /**
        Audio processing finished callback, passed to the non-blocking API.
     */
    static void audioProcessingFinishedCallback(void* data);
          

    // Blocking audio

    /**
        A routine that loops reading and writing audio input, using the blocking API.
     */
    static void processAudioStream(AudioDeviceStream* instance);


    // Shared
    
    inline bool hasInput()
    {
        return numberOfInputs() > 0;
    }       

    inline bool hasOutput()
    {
        return numberOfOutputs() > 0;
    }       
    
    inline int numberOfInputs()
    {  
        if (!inputDevice)
            return 0;
        else
            return std::min( inputDevice->numberOfInputs(), processor->description()->numberOfInputs() );
    }
    
    inline int numberOfOutputs()
    {
        if (!outputDevice)
            return 0;
        else
            return std::min( outputDevice->numberOfOutputs(), processor->description()->numberOfOutputs() );
    }
    
    inline void runSchedulerExecutePending()
    {
        audioSchedulerInstance->executePending();            
    }  
    
    inline void retrieveIncomingMessages()
    {
        boost::tuple< AudioProcessor*, std::list<Message> >* next;
        
        while ( incomingMessagesQueue->remove(&next) )
        {
            AudioProcessor*    processor = next->get<0>();
            std::list<Message> messages  = next->get<1>();
            
            foreach(Message message, messages)
                processor->accept(message);
        }
    }  
    
    inline void allocateDspBuffers(int numberOfChannels, int numberOfFrames)
    {
        buffer = new AudioProcessingBuffer(numberOfChannels, numberOfFrames);
    }     

    inline void freeDspBuffers()
    {
        delete buffer; 
        buffer = NULL;
    }      
    
    inline void allocateInfo()
    {       
        info = new AudioProcessingInformation;
        info->sampleRate  = this->options.sampleRate;
        info->sampleCount = this->sampleCount.value();
    }

    inline void freeInfo()
    {
        delete info;
        info = NULL;
    }

    
    inline void silenceBuffer(int numberOfChannels, int numberOfFrames)
    {
        for (int i = 0; i < (numberOfChannels * numberOfFrames); ++i)
            buffer->data[i] = 0;
    }             
    
    inline void copyOutput(Sample* outputBuffer, int numberOfOutputs, int numberOfFrames)
    {
        for (int frame = 0; frame < numberOfFrames; ++frame)
        {
            for (int channel = 0; channel < numberOfOutputs; ++channel)
            {
                outputBuffer[frame * numberOfOutputs + channel] = 
                    buffer->data[frame + channel * numberOfFrames];
            }
        }
    }

    static void exclusiveStreamClosedCallback(void* data);
    void assureExclusiveOpen();
    void assureExclusiveStart();
    void registerExlusiveStop();
    void registerExlusiveClose();
        
// #ifdef SCL_WIN
//  static ThreadLocal<WindowsComToken> windowsComToken;
// #endif
	
    AudioDevice                 *inputDevice, *outputDevice;
    AudioProcessor              *processor;
    AudioProcessingInformation  *info;
    AudioProcessingBuffer       *buffer;

    PaStream                    *portaudioStream;

    typedef boost::tuple< AudioProcessor*, 
                          std::list<Message> > TimedMessage;

    typedef Queue< TimedMessage* >             IncomingMessagesQueue;


    IncomingMessagesQueue       *incomingMessagesQueue;
    AudioScheduler              *audioSchedulerInstance;
    Thread                      *messageHandlerThread, *audioThread;
    Error                       *audioThreadError;
        
    AtomicNumber<Time>          sampleCount;
};

} // namespace
} // namespace

#endif
