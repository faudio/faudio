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
    
    String message() const;            

private:
    String specificMessage;
};


// =============================================================================

class SCLAUDIO_API FluidSynthDescription : public AudioProcessorDescription
{
public:
    String name() const { return "FluidSynth"; }
    int numberOfInputs() const { return 0; }     
    int numberOfOutputs() const { return 2; } 
    int numberOfBuses() const { return 0; }            
    
    bool isAtomic() const { return true; }                   
    bool isStateful() const { return true; }                   
    bool isPlugin() const { return false; }
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

    AudioProcessorDescription* description() const;

    void accept(Message message);

    void prepare(AudioProcessingInformation& info, AudioProcessingBuffer &signal);
    void process(AudioProcessingInformation& info, AudioProcessingBuffer &signal);
    void cleanup(AudioProcessingInformation& info, AudioProcessingBuffer &signal);
private:
    static void handleLogMessage(int level, char* message, void* data);
    FluidSynthData* mData;
};

} // namespace
} // namespace

#endif
