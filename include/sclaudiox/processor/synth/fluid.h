/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_SYNTH_FLUID
#define _SCLAUDIOX_SYNTH_FLUID

#include "sclaudiox/device/audio.h"
#include "sclaudiox/processor.h"

namespace doremir {
namespace scl {

class FluidSynthData;


// =============================================================================

class SCLAUDIO_API FluidSynthError : public DspError
{
public:
    explicit FluidSynthError(String message);
    explicit FluidSynthError(FluidSynthData* data);
    
    String message();            

private:
    String specificMessage;
};


// =============================================================================

class SCLAUDIO_API FluidSynthDescription : public AudioProcessorDescription
{
public:
    String name() { return "FluidSynth"; }
    int numberOfInputs() { return 0; }     
    int numberOfOutputs() { return 2; } 
    int numberOfBuses() { return 0; }            
    
    bool isAtomic() { return true; }                   
    bool isStateful() { return true; }                   
    bool isPlugin() { return false; }
}; 


// =============================================================================

/**
    An audio processor wrapping the FluidSynth software synthesizer.
 */
class SCLAUDIO_API FluidSynth : public AudioProcessor 
{
public:    
    /**
        Constructs a FluidSynth instance, loading a sound font from the given path.

        \throw DspError 
            If the sound font could not be loaded.
     */
    explicit FluidSynth(FilePath path);    

    /**
        Destructor.
     */
    ~FluidSynth();

    AudioProcessorDescription* description();

    void accept(Message message);

    void prepare(AudioProcessingInformation& info, AudioProcessingBuffer &signal);
    void process(AudioProcessingInformation& info, AudioProcessingBuffer &signal);
    void cleanup(AudioProcessingInformation& info, AudioProcessingBuffer &signal);
private:
    FluidSynthData* mData;
};

} // namespace
} // namespace

#endif
