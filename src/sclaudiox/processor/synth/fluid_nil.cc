/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
/**
    @file   sclaudiox/processor/synth/fluid.cc
    @author Hans Hoglund
 */

#include "sclaudiox/control.h"
#include "sclaudiox/util/concurrency.h"
#include "sclaudiox/processor/synth/fluid.h"

#ifdef SCL_LOG
#define SCL_FLUID_LOG
#endif // SCL_LOG

namespace doremir {
namespace scl {




// ============================================================================

FluidSynthError::FluidSynthError(String message) 
    : specificMessage(message) {}

FluidSynthError::FluidSynthError(FluidSynthData* data)
{
}

String FluidSynthError::message()
{
}  


// ============================================================================

FluidSynth::FluidSynth(FilePath path)
    : mData(NULL)
{
}

FluidSynth::~FluidSynth()
{
}

AudioProcessorDescription* FluidSynth::description()
{
}       

void FluidSynth::accept(Message message)
{
}

void FluidSynth::prepare(AudioProcessingInformation& info, 
                         AudioProcessingBuffer& signal)
{
}

void FluidSynth::process(AudioProcessingInformation& info, 
                         AudioProcessingBuffer &signal)
{
}

void FluidSynth::cleanup(AudioProcessingInformation& info, 
                         AudioProcessingBuffer &signal) {}



} // namespace
} // namespace
