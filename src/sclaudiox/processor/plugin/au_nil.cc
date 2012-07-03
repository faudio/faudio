/*
    ScoreCleaner Audio Engine

    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
/**
    @file   sclaudiox/processor/plugin/au_nil.cc
    @author Hans Hoglund
 */

#include "sclaudiox/processor/plugin/au.h"
#include "sclaudiox/util/misc.h"
#include "sclaudiox/util/logging.h"


namespace doremir {
namespace scl {

AudioUnitDescription::AudioUnitDescription(AudioUnitDescriptionData* data) {}
AudioUnitDescription::~AudioUnitDescription() {}

String AudioUnitDescription::name()
{
    return "";
}

bool AudioUnitDescription::isAtomic()
{
    return true;
}

bool AudioUnitDescription::isStateful()
{
    return true;
}

bool AudioUnitDescription::isPlugin()
{
    return true;
}


// TODO throws
void AudioUnitDescription::assureProcessor()
{
}

int AudioUnitDescription::numberOfInputs()
{
    return 0;
}

int AudioUnitDescription::numberOfOutputs()
{
    return 0;
}

int AudioUnitDescription::numberOfBuses()
{
    return 0;
}


AudioUnitVersion AudioUnitDescription::version()
{
    // FIXME
    AudioUnitVersion v;
    return v;
}

AudioPlugin* AudioUnitDescription::plugin()
{
    return 0;
}



AudioUnitProcessor::AudioUnitProcessor(AudioUnitProcessorData* data) {}
AudioUnitProcessor::~AudioUnitProcessor() {}

AudioProcessorDescription* AudioUnitProcessor::description()
{
    return NULL;
}

void AudioUnitProcessor::accept(Message message){}
void AudioUnitProcessor::prepare(AudioProcessingInformation& info, AudioProcessingBuffer &signal) {}
void AudioUnitProcessor::process(AudioProcessingInformation& info, AudioProcessingBuffer &signal) {}
void AudioUnitProcessor::cleanup(AudioProcessingInformation& info, AudioProcessingBuffer &signal) {}

AudioPlugin* AudioUnitProcessor::plugin()
{
    return NULL;
}

void* AudioUnitProcessor::nativePluginInstance()
{
    return NULL;
}



AudioUnit::AudioUnit(AudioUnitData* data) {}
AudioUnit::~AudioUnit() {}
void AudioUnit::assureProcessor() {}

AudioPluginProcessorDescription* AudioUnit::description()
{
    return NULL;
}

AudioPluginProcessor* AudioUnit::createProcessor()
{
    return NULL;
}

void* AudioUnit::nativePlugin()
{
    return NULL;
}

AudioUnitDescription* AudioUnit::audioUnitDescription()
{
    return NULL;
}

AudioUnitProcessor* AudioUnit::createAudioUnitProcessor()
{
    return NULL;
}

std::list<AudioUnit*> AudioUnit::audioUnits()
{
    return list::create<AudioUnit*>();
}

AudioUnit* AudioUnit::dlsMusicDevice()
{
    return NULL;
}


} // namespace
} // namespace
