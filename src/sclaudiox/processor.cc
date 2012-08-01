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

#include "sclaudiox/error.h"
#include "sclaudiox/processor.h"
#include "sclaudiox/util/foreach.h"

namespace doremir {
namespace scl {

class AbstractProcessCombinatorDescription;
    
/**
    Base implementation for process combinators.
 */
class AbstractProcessCombinator : public AudioProcessor
{
public:
    AbstractProcessCombinator(std::list<AudioProcessor*> processors) 
        : mChildren(processors) {}

    void accept(Message message)
    {
        // FIXME
    }

    void prepare(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
    {
        foreach(AudioProcessor* child, mChildren)
            child->prepare(info, signal);
    }
    
    void cleanup(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
    {
        foreach(AudioProcessor* child, mChildren)
            child->cleanup(info, signal);
    }

protected:    
    friend class AbstractProcessCombinatorDescription;
    std::list<AudioProcessor*> mChildren;
};

class AbstractProcessCombinatorDescription : public AudioProcessorDescription
{
    int numberOfBuses()
    {
        int result = 0;
        foreach(AudioProcessor* child, mProcessor->mChildren)
            result += child->description()->numberOfBuses();
        return result;
    }                                  
private:
    AbstractProcessCombinator* mProcessor;
};


class SequenceAudioProcessor : public AbstractProcessCombinator
{
public:
    SequenceAudioProcessor(std::list<AudioProcessor*> processors)
        : AbstractProcessCombinator(processors) {}

    AudioProcessorDescription* description()
    {
        // FIXME
        return NULL;
    }

    // int numberOfInputs() 
    // {
    //     return children.front()->numberOfInputs();
    // }
    // 
    // int numberOfOutputs()
    // {
    //     return children.back()->numberOfOutputs();
    // }
    //    
    void process(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
    {
        // FIXME
    }

};


class ParallelAudioProcessor : public AbstractProcessCombinator
{
public:
    ParallelAudioProcessor(std::list<AudioProcessor*> processors)
        : AbstractProcessCombinator(processors) {}

    AudioProcessorDescription* description()
    {
        // FIXME
      return NULL;
    }

    // int numberOfInputs() 
    // {
    //     int result = 0;
    //     foreach(AudioProcessor* child, children)
    //         result += child->numberOfInputs();
    //     return result;
    // }
    // 
    // int numberOfOutputs()
    // {
    //     int result = 0;
    //     foreach(AudioProcessor* child, children)
    //         result += child->numberOfOutputs();
    //     return result;
    // }     
    
    void process(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
    {
        // FIXME
    }
};


AudioProcessor* AudioProcessor::sequence(std::list<AudioProcessor*> processors) 
{
    return new SequenceAudioProcessor(processors);
}

AudioProcessor* AudioProcessor::parallel(std::list<AudioProcessor*> processors) 
{
    return new ParallelAudioProcessor(processors);
}

} // namespace
} // namespace
