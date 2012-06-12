/*
    ScoreCleaner Audio Engine

    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
/**
    @file   sclaudiox/stream/device/audio.cc
    @author Hans Hoglund
 */

#include "sclaudiox/stream/device/audio.h"

#ifdef SCL_WIN
    #include "pa_win_wasapi.h"
#endif

#ifdef SCL_LOG
  #define SCL_LOG_AUDIO 1000
#endif

#define SCL_CHECK_PORTAUDIO_ERROR(ERR) \
    if (ERR != paNoError) throw PortaudioError(ERR)

#ifdef SCL_WIN
    #define SCL_ASSURE_COINIT() \
        AudioDeviceStream::windowsComToken.get();
#else
    #define SCL_ASSURE_COINIT()
#endif

using namespace doremir::scl;

// =============================================================================

MessageScheduler* AudioDeviceStream::audioScheduler()
{
    return audioSchedulerInstance;
}

Time AudioDeviceStream::sampleTime()
{
    return sampleCount.value();
}

Time AudioDeviceStream::millisecondTime()
{
    return samplesToMilliseconds(sampleTime(), options.sampleRate);
}

bool AudioDeviceStream::useInternalTimer()
{
    return false;
}

int AudioDeviceStream::sampleRate()
{
    return options.sampleRate;
}

int AudioDeviceStream::audioBufferSize()
{
    return options.audioBufferSize;
}
// =============================================================================

void AudioDeviceStream::startAudio()
{
    PaError err;
    bool isNonBlocking = options.useNonBlocking;

    SCL_ASSURE_COINIT();

    if (!portaudioStream)
    {
        err = openAudioStream(options.useNonBlocking, options.useExclusiveMode);
        SCL_CHECK_PORTAUDIO_ERROR(err);
    }

    assureExclusiveStart();

    if (isNonBlocking)
    {
        prepareNonBlocking();
    }

    err = Pa_StartStream(portaudioStream);
    SCL_CHECK_PORTAUDIO_ERROR(err);

    if (isNonBlocking)
    {
        messageHandlerThread = new Thread(messageHandler, this);
        SCL_WRITE_LOG("  Launched audio message handler thread " << messageHandlerThread << "\n");
    }
    else
    {
        audioThread = new Thread(processAudioStream, this);
        SCL_WRITE_LOG("  Launched audio thread " << audioThread << "\n");
    }
    SCL_WRITE_LOG("Finished starting audio\n");
}

void AudioDeviceStream::abortAudio()
{
    PaError err;
    bool isNonBlocking = options.useNonBlocking;

    SCL_ASSURE_COINIT();

    if (isNonBlocking)
    {
        waitForThread(messageHandlerThread);
        delete messageHandlerThread;
        messageHandlerThread = NULL;
    }
    else
    {
        waitForThread(audioThread);
        delete audioThread;
        audioThread = NULL;
    }

    err = Pa_AbortStream(portaudioStream);
    SCL_CHECK_PORTAUDIO_ERROR(err);

    registerExlusiveStop();

    SCL_WRITE_LOG("Finished aborting audio\n");
}

void AudioDeviceStream::stopAudio()
{
    PaError err;
    bool isNonBlocking = options.useNonBlocking;

    SCL_ASSURE_COINIT();

    if (isNonBlocking)
    {
        waitForThread(messageHandlerThread);
        delete messageHandlerThread;
        messageHandlerThread = NULL;
    }
    else
    {
        waitForThread(audioThread);
        delete audioThread;
        audioThread = NULL;
    }

    err = Pa_StopStream(portaudioStream);
    SCL_CHECK_PORTAUDIO_ERROR(err);

    registerExlusiveStop();

    SCL_WRITE_LOG("Finished stopping audio\n");
}


#ifdef SCL_WIN
PaWasapiStreamInfo* createWasapiStreamInfo()
{
    PaWasapiStreamInfo* info = new PaWasapiStreamInfo;
    info->size = sizeof(PaWasapiStreamInfo);

    info->hostApiType   = paWASAPI;
    info->version       = 1;
	info->flags			= 0;

	return info;
}
#endif // SCL_WIN


typedef void PaHostApiSpecificInfo;

PaHostApiSpecificInfo* createStreamInfo(AudioDevice* device)
{
    if (!device) return NULL;

    switch (device->host()->type())
    {
#ifdef SCL_WIN
    case paWASAPI:
        return createWasapiStreamInfo();
#endif
    default:
        return NULL;
    }
}

void freeStreamInfo(AudioDevice* device, PaHostApiSpecificInfo* info)
{
    if (!device) return;

    switch (device->host()->type())
    {
    #ifdef SCL_WIN
        case paWASAPI:
            delete ((PaWasapiStreamInfo*) info);
    #endif
        default:
            break;
    }
}

void setStreamInfoExclusive(AudioDevice* device, PaHostApiSpecificInfo* info)
{
    if (!device) return;

    switch (device->host()->type())
    {
    #ifdef SCL_WIN
        case paWASAPI:
            ((PaWasapiStreamInfo*) info)->flags = paWinWasapiExclusive;
    #endif
        default:
            break;
    }
}


PaError AudioDeviceStream::openAudioStream(bool isNonBlocking, bool isExclusive)
{
    PaStreamParameters inputParams, outputParams;
    PaHostApiSpecificInfo *inputInfo, *outputInfo;
    PaError result;

    inputInfo = createStreamInfo(inputDevice);
    outputInfo = createStreamInfo(outputDevice);

    if (isExclusive)
    {
        setStreamInfoExclusive(inputDevice, inputInfo);
        setStreamInfoExclusive(outputDevice, outputInfo);
    }

    if (inputDevice)
    {
        inputParams.device                      = inputDevice->index();
        inputParams.channelCount                = numberOfInputs();
        inputParams.sampleFormat                = paFloat32;
        inputParams.suggestedLatency            = 0;
        inputParams.hostApiSpecificStreamInfo   = inputInfo;
    }

    if (outputDevice)
    {
        outputParams.device                     = outputDevice->index();
        outputParams.channelCount               = numberOfOutputs();
        outputParams.sampleFormat               = paFloat32;
        outputParams.suggestedLatency           = 0;
        outputParams.hostApiSpecificStreamInfo  = outputInfo;
    }

    SCL_WRITE_LOG ( "Opening AudioDeviceStream\n"
                 << "    Mode: "          << ( isNonBlocking  ? "Non-blocking" : "Blocking" ) << "\n"
                 << "    Exclusive: "     << ( isExclusive    ? "Yes" 		   : "No"       ) << "\n"
                 << "    Input device: "  << ( !inputDevice  ? "N/A" : inputDevice->name()  ) << "\n"
                 << "        Channels: "  << numberOfInputs()                                 << "\n"
                 << "    Output device: " << ( !outputDevice ? "N/A" : outputDevice->name() ) << "\n"
                 << "        Channels: "  << numberOfOutputs()                                << "\n" );

    result = Pa_OpenStream (
        &portaudioStream,
        hasInput()
            ? &inputParams
            : NULL,
        hasOutput()
            ? &outputParams
            : NULL,
        options.sampleRate,
        options.audioBufferSize,
        paNoFlag,
        isNonBlocking
            ? audioProcessingCallback
            : NULL,
        isNonBlocking
            ? this
            : NULL
    );

    freeStreamInfo(inputDevice, inputInfo);
    freeStreamInfo(outputDevice, outputInfo);

    return result;
}



// =============================================================================

// Non-blocking implementation

void AudioDeviceStream::prepareNonBlocking()
{
    Pa_SetStreamFinishedCallback(portaudioStream, audioProcessingFinishedCallback);
    audioThreadError = NULL;

    int numberOfChannels = processor->description()->numberOfChannels(); // TODO should it not be instance->numberOfChannels() ?
    int numberOfFrames   = options.audioBufferSize;

    SCL_WRITE_LOG ( "Preparing audio processing\n"
                 << "    Number of inputs: "         << numberOfInputs()   << "\n"
                 << "    Number of outputs: "        << numberOfOutputs()  << "\n"
                 << "    Total number of channels: " << numberOfChannels   << "\n"
                 << "    Sample rate: "              << options.sampleRate << "\n"
                 << "    Buffer size: "              << numberOfFrames     << "\n" );

    allocateInfo();
    allocateDspBuffers(numberOfChannels, numberOfFrames);

    processor->prepare(*info, *buffer);
}

void AudioDeviceStream::messageHandler(AudioDeviceStream* instance)
{
    // TODO catch exceptions here ?
    while(true)
    {
        blockUntilTimeUpdated(instance);
        instance->runSchedulerExecutePending();
    }
}

int AudioDeviceStream::audioProcessingCallback( const void                     *input,
                                                void                           *output,
                                                unsigned long                  frameCount,
                                                const PaStreamCallbackTimeInfo *timeInfo,
                                                PaStreamCallbackFlags          statusFlags,
                                                void                           *data )
{
    AudioDeviceStream* instance = (AudioDeviceStream*) data;

    try
    {
        int numberOfInputs   = instance->numberOfInputs();
        int numberOfOutputs  = instance->numberOfOutputs();
        int numberOfChannels = instance->processor->description()->numberOfChannels();
        int numberOfFrames   = frameCount;

        AudioProcessingInformation* inf = instance->info;
        AudioProcessingBuffer*      buf = instance->buffer;

        instance->retrieveIncomingMessages();
        inf->sampleCount = instance->sampleCount.value();

        instance->silenceBuffer(numberOfChannels, numberOfFrames);
        instance->processor->process(*inf, *buf);
        instance->copyOutput((Sample*) output, numberOfOutputs, numberOfFrames);

        // atomicAdd<Int32>(&(instance->sampleCount), numberOfFrames);
        instance->sampleCount.increment(numberOfFrames);
        notifyTimeUpdated(instance);

        // For now, handle underflow/overflow uncompromisingly
        // TODO
        // if (statusFlags & paInputOverflow)   throw PortaudioError(paInputOverflowed);
        // if (statusFlags & paOutputUnderflow) throw PortaudioError(paOutputUnderflowed);

        return paContinue;
    }
    catch (Error& e)
    {
        SCL_WRITE_LOG("Exception in the audio thread: " << e.message() << "\n");
        instance->audioThreadError = new StreamError(e.message()); // FIXME
        return paAbort;
    }
    catch (...)
    {
        SCL_WRITE_LOG("Unexpected error in the audio thread\n");
        return paAbort;
    }
}

void AudioDeviceStream::audioProcessingFinishedCallback(void* data)
{
    AudioDeviceStream* instance = (AudioDeviceStream*) data;

    AudioProcessingInformation* inf = instance->info;
    AudioProcessingBuffer*      buf = instance->buffer;

    instance->processor->cleanup( *inf, *buf );

    instance->freeDspBuffers();
    instance->freeInfo();

    if (instance->audioThreadError)
    {
        instance->handleError(*(instance->audioThreadError));
    }
}


// =============================================================================

// Blocking implementation

void AudioDeviceStream::processAudioStream(AudioDeviceStream* instance)
{
    PaError err;

    SCL_ASSURE_COINIT();

    int numberOfInputs   = instance->numberOfInputs();
    int numberOfOutputs  = instance->numberOfOutputs();
    int numberOfChannels = instance->processor->description()->numberOfChannels(); // TODO should it not be instance->numberOfChannels() ?
    int numberOfFrames   = instance->options.audioBufferSize;

    SCL_WRITE_LOG ( "Entering audio loop\n"
                 << "    Number of inputs: "         << numberOfInputs               << "\n"
                 << "    Number of outputs: "        << numberOfOutputs              << "\n"
                 << "    Total number of channels: " << numberOfChannels        << "\n"
                 << "    Sample rate: "              << instance->options.sampleRate << "\n"
                 << "    Buffer size: "              << numberOfFrames               << "\n" );

    Sample* inputBuffer  = new Sample[ numberOfInputs  * numberOfFrames ];
    Sample* outputBuffer = new Sample[ numberOfOutputs * numberOfFrames ];

    instance->allocateInfo();
    instance->allocateDspBuffers(numberOfChannels, numberOfFrames);

    AudioProcessingInformation* inf = instance->info;
    AudioProcessingBuffer*      buf = instance->buffer;

    instance->processor->prepare( *inf, *buf );

    try
    {
        while (true)
        {
            interruptionPoint();

            instance->runSchedulerExecutePending();
            instance->retrieveIncomingMessages();
            inf->sampleCount = instance->sampleCount.value();

            instance->silenceBuffer(numberOfChannels, numberOfFrames);
            instance->processor->process( *inf, *buf );
            instance->copyOutput(outputBuffer, numberOfOutputs, numberOfFrames);

            err = Pa_WriteStream(instance->portaudioStream, outputBuffer, numberOfFrames);
            SCL_CHECK_PORTAUDIO_ERROR(err);

            // atomicAdd<Int32>(&(instance->sampleCount), numberOfFrames);
            instance->sampleCount.increment(numberOfFrames);
            notifyTimeUpdated(instance);
        }
    }
    catch (ThreadInterrupted& e)
    {
        instance->processor->cleanup( *inf, *buf );

        instance->freeDspBuffers();
        instance->freeInfo();

        throw e;
    }
    catch (Error& e)
    {
        SCL_WRITE_LOG("Exception in the audio thread: " << e.message() << "\n");
        instance->processor->cleanup( *(instance->info), *(instance->buffer) );

        instance->freeDspBuffers();
        instance->freeInfo();

        instance->handleError(e);
    }
    catch (...)
    {
        SCL_WRITE_LOG("Unexpected error in audio thread\n");
    }
}



// =============================================================================

void AudioDeviceStream::exclusiveStreamClosedCallback(void* data)
{
	AudioDeviceStream* stream = (AudioDeviceStream*) data;

	SCL_WRITE_LOG("Closing stream due to stealing of exclusive device\n");

	// FIXME Can we assume that stream is stopped here (?)
    Pa_CloseStream(stream->portaudioStream);
}

void AudioDeviceStream::assureExclusiveOpen()
{
    // TODO check input and output as available
    // if (outputDevice)
        // outputDevice->assureExclusiveOpen(exclusiveStreamClosedCallback, this);
}
void AudioDeviceStream::assureExclusiveStart()
{
    // TODO check input and output as available
    // if (outputDevice)
        // outputDevice->assureExclusiveStart(this);
}
void AudioDeviceStream::registerExlusiveStop()
{
    // TODO check input and output as available
    // if (outputDevice)
        // outputDevice->registerExlusiveStop(this);
}
void AudioDeviceStream::registerExlusiveClose()
{
    // TODO check input and output as available
    // if (outputDevice)
        // outputDevice->registerExlusiveClose(this);
}


// =============================================================================

AudioDeviceStream::AudioDeviceStream(AudioDevice*        inputDevice,
                                     AudioDevice*        outputDevice,
                                     AudioProcessor*     processor,
                                     DeviceStreamOptions options)
    : DeviceStream(options)

    , inputDevice(acquire(inputDevice))
    , outputDevice(acquire(outputDevice))
    , processor(acquire(processor))
    , info(NULL)
    , buffer(NULL)

    , portaudioStream(NULL)

    , incomingMessagesQueue(new LinkedAtomicQueue< boost::tuple< AudioProcessor*, std::list<Message> >* >())
    , audioSchedulerInstance(new AudioScheduler(this, processor, incomingMessagesQueue))

    , messageHandlerThread(NULL)
    , audioThread(NULL)
    , audioThreadError(NULL)
    , sampleCount(0)
{
    assureExclusiveOpen();
}

AudioDeviceStream::~AudioDeviceStream()
{
    delete info;
    delete buffer;
    delete incomingMessagesQueue; // TODO queue of pointers, must manually delete elements
    delete audioSchedulerInstance;

    // TODO verify that stream is stopped by enclosing stream destructor
    registerExlusiveClose();
}


// =============================================================================

#ifdef SCL_WIN
    ThreadLocal<WindowsComToken> AudioDeviceStream::windowsComToken;
#endif


