/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef  _SCLAUDIOX_PROCESSOR
#define  _SCLAUDIOX_PROCESSOR

#include <list>

#include "sclaudiox/core.h"
#include "sclaudiox/error.h"
#include "sclaudiox/signal.h"
#include "sclaudiox/control.h"


namespace doremir {
namespace scl {

class Stream;
class Processor;


// =============================================================================

/**
    A signal processing error.
 */
class SCLAUDIO_API DspError : public Error
{
public:    
    explicit DspError() {}

    String message () const
    {
        return "A signal processing error occured";
    }
    
private:
    Stream*    stream;
    Processor* processor;
};


// =============================================================================

// typedef String Control;
// 
// /**
//     An object supporting control messages.
//  */
// class SCLAUDIO_API Controlled
// {    
// public:     
//     virtual ~Controlled() {}
// 
//     /**
//         Returns the name of the controls used by this processor.
//      */
//     virtual std::list<Control> controls() const 
//     { 
//         return list::create<Control>(); 
//     }
// 
//     /**
//         Accepts a message.
//      */
//     virtual void accept(Message message) {};
// };  


// =============================================================================

/**
    An abstract description of an audio processor.
    Used to retreive information about an AudioProcessor or AudioPlugin. 
 */
class SCLAUDIO_API AudioProcessorDescription : public NonCopyable
{
public:        
    /**
        Constructor.
     */
    AudioProcessorDescription(){}

    /**
        Destructor.
     */
    virtual ~AudioProcessorDescription(){}


    /** 
        Returns the name of the audio processor.
     */
    virtual String name() const = 0;
    
    /**
        Whether or not the audio processor is atomic.
        `isAtomic()` implies `!isCompound()`.
     */
    virtual bool isAtomic() const = 0;

    /**
        Whether or not the audio processor is compound.
        `isCompound()` implies `!isAtomic()`.
     */
    virtual bool isCompound() const
    {
        return !isAtomic();
    } 

    /**
        Whether or not the audio processor is stateful.
        A compound processor is stateful if one or more of its child processors are stateful.
        
        If this method returns true the following invariants should hold:

        - `prepare()` and `cleanup()` does not modify the processor
        - `process()` does not modify the processor (but may modify its arguments)
        - `process()` may be called by concurrent threads
     */
    virtual bool isStateful() const = 0;  

    /**
        Whether the processor is loaded from a plugin. 
        This implies that it is a subclass of PluginAudioProcessor.
     */
    virtual bool isPlugin() const = 0;

    /** 
        Returns the number of input channels.
     */
    virtual int numberOfInputs() const = 0;

    /** 
        Returns the number of output channels.
     */
    virtual int numberOfOutputs() const = 0;

    /** 
        Returns the number of bus channels.
     */
    virtual int numberOfBuses() const = 0;

    /**
        Returns the total number of channels.
     */
    inline int numberOfChannels() const
    {
        return std::max(numberOfInputs(), numberOfOutputs()) + numberOfBuses();
    }

    /**
        Checks that internal connections have matching inputs and outputs.
        For atomic processors, this function does nothing. 
        @throws DspError
     */
    virtual void checkInputsAndOutputs() const
    {
        // nothing
    }    
};


// =============================================================================

/**
    An object that transforms audio signals. May be atomic or created by combinators.    
 */
class SCLAUDIO_API AudioProcessor : public NonCopyable, public Resource
{
public:    
    /**
        Constructor.
     */
    AudioProcessor(){}
    
    /**
        Destructor.
     */
    virtual ~AudioProcessor(){}

    /**
        Returns a description of this audio processor.
     */
    virtual AudioProcessorDescription* description() const = 0;

    /**
        Accepts a message.
     */
    virtual void accept(Message message) {};

    /** 
        Called once before audio processing begins.
        @throws DspError
     */
    virtual void prepare(AudioProcessingInformation& info, AudioProcessingBuffer &signal) {};

    /** 
        Called repeatedly during audio processing.
        @throws DspError
     */
    virtual void process(AudioProcessingInformation& info, AudioProcessingBuffer &signal) = 0;

    /** 
        Called once after audio processing is finished.
        @throws DspError
     */
    virtual void cleanup(AudioProcessingInformation& info, AudioProcessingBuffer &signal) {};

    /** 
        Combines the given processors to run in sequence.
     */
    static AudioProcessor* sequence(std::list<AudioProcessor*> inputs);

    /** 
        Combines the given processors to run in parallel.
     */
    static AudioProcessor* parallel(std::list<AudioProcessor*> outputs);
};   



} // namespace
} // namespace

#endif