/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
/**
    @file   sclaudiox/processor.cc
    @author Hans Hoglund
 */

#include <list>
#include <sstream>

#include "sclaudiox/error.h"
#include "sclaudiox/processor.h"
#include "sclaudiox/util/foreach.h"

namespace doremir {
namespace scl {


// =============================================================================

/**
    Base implementation for process combinators.
 */
class AbstractProcessCombinator : public virtual AudioProcessor
{
public:
    // TODO
    // int numberOfBuses() const
    // {
    //     int result = 0;
    //     foreach(AudioProcessor* child, children)
    //         result += child->numberOfBuses();
    //     return result;
    // }

    virtual std::list<String> controls() const
    {
        throw Unimplemented();
    }

    void prepare(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
    {
        foreach(AudioProcessor* child, children)
            child->prepare(info, signal);
    }
    
    void cleanup(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
    {
        foreach(AudioProcessor* child, children)
            child->cleanup(info, signal);
    }

    AbstractProcessCombinator(std::list<AudioProcessor*> children) 
        : children(children) {}

protected:
    std::list<AudioProcessor*> children;
};



// =============================================================================

class SequenceAudioProcessor : public AbstractProcessCombinator
{
public:
    // int numberOfInputs() const 
    // {
    //     return children.front()->numberOfInputs();
    // }
    // 
    // int numberOfOutputs() const
    // {
    //     return children.back()->numberOfOutputs();
    // }
    // 
    SequenceAudioProcessor(std::list<AudioProcessor*> procs)
        : AbstractProcessCombinator(procs) {}

};


// =============================================================================

class ParallelAudioProcessor : public AbstractProcessCombinator
{
public:    
    // int numberOfInputs() const 
    // {
    //     int result = 0;
    //     foreach(AudioProcessor* child, children)
    //         result += child->numberOfInputs();
    //     return result;
    // }
    // 
    // int numberOfOutputs() const
    // {
    //     int result = 0;
    //     foreach(AudioProcessor* child, children)
    //         result += child->numberOfOutputs();
    //     return result;
    // }
    
    ParallelAudioProcessor(std::list<AudioProcessor*> procs)
        : AbstractProcessCombinator(procs) {}
};


// =============================================================================

AudioProcessor* AudioProcessor::sequence(std::list<AudioProcessor*> procs) 
{
    throw Unimplemented();
}

AudioProcessor* AudioProcessor::parallel(std::list<AudioProcessor*> procs) 
{
    throw Unimplemented();
}

} // namespace
} // namespace
