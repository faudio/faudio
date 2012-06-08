/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_SYNTH_RAND
#define _SCLAUDIOX_SYNTH_RAND

#include "sclaudiox/device/audio.h"
#include "sclaudiox/processor.h"

namespace doremir {
namespace scl {

// =============================================================================

class SCLAUDIO_API RandDescription : public AudioProcessorDescription
{
public:
    String name() { return "Random"; }
    int numberOfInputs() { return 0; }     
    int numberOfOutputs() { return 2; } 
    int numberOfBuses() { return 0; }            
    
    bool isAtomic() { return true; }                   
    bool isStateful() { return true; }                   
    bool isPlugin() { return false; }
}; 


// =============================================================================

class SCLAUDIO_API Rand : public AudioProcessor 
{
public:    
    Rand()
        : mDescr(NULL) {}

    ~Rand() 
    {
        if (mDescr)
            delete mDescr;
    }

    AudioProcessorDescription* description()
    {                         
        if (!mDescr)
            mDescr = new RandDescription();
        return mDescr;
    }

    void accept(Message message) {}

    void prepare(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
    {
        srand((unsigned) time(0));
    }
    void process(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
    {
        for (int i = 0; i < (signal.numberOfChannels * signal.numberOfFrames); ++i)
            signal.data[i] = (((float)rand()/(float)RAND_MAX) * 2.0 - 1.0) * 0.1;
    }
    void cleanup(AudioProcessingInformation& info, AudioProcessingBuffer &signal) {}
private:
    RandDescription* mDescr;
};

} // namespace
} // namespace

#endif
