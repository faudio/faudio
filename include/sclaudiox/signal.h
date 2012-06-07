/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef  _SCLAUDIOX_SIGNAL
#define  _SCLAUDIOX_SIGNAL

#include "sclaudiox/core.h"


namespace doremir {
namespace scl {

/**
    A structure encapsulating information about the current state of the
    audio engine, to be passed to AudioProcessor during processing.
 */
struct SCLAUDIO_API AudioProcessingInformation
{
    /**
        Sample rate of this audio computation.
     */
    RealTime sampleRate;

    /**
        Number of samples processed before the current call to process().
     */
    Sample sampleCount;
};


/**
    A structure containing the current value of the audio buffers, 
    to be passed to AudioProcessor during processing.
 */
class SCLAUDIO_API AudioProcessingBuffer : public NonCopyable
{    
public:
    AudioProcessingBuffer(int numberOfChannels, 
                          int numberOfFrames)
        : numberOfChannels(numberOfChannels)
        , numberOfFrames(numberOfFrames)
    {
        this->data = new Sample[ numberOfChannels * numberOfFrames ];
    }
    
    ~AudioProcessingBuffer()
    {
       delete this->data;
    }
    
    int     numberOfChannels;
    int     numberOfFrames;

    Sample* data; 
};

} // namespace
} // namespace

#endif
