/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
/**
    @file   sclaudiox/processor/plugin/nil.cc
    @author Hans Hoglund
 */

// specific includes

#include "sclaudiox/processor/plugin/nil.h"
#include "sclaudiox/util/misc.h"


namespace doremir {
namespace scl {

struct NilPluginDescriptionData
{                          
    NilPlugin* plugin;
    // info
};
struct NilPluginProcessorData
{
    NilPlugin* plugin;
    NilPluginDescription* description;
    // instance
};
struct NilPluginData
{
    NilPluginDescription* description;
    // plugin
};


NilPluginDescription::NilPluginDescription(NilPluginDescriptionData* data)
    : mData(data) {}
    
NilPluginDescription::~NilPluginDescription()
{
    delete mData;
}

String NilPluginDescription::name()
{         
    // TODO implement
}                  
bool NilPluginDescription::isAtomic()
{
    // TODO implement
}
bool NilPluginDescription::isStateful()
{
    // TODO implement
}
bool NilPluginDescription::isPlugin()
{
    // TODO implement
}

int NilPluginDescription::numberOfInputs()
{
    // TODO implement
}

int NilPluginDescription::numberOfOutputs()
{
    // TODO implement
}

int NilPluginDescription::numberOfBuses()
{
    // TODO implement
}


AudioPlugin* NilPluginDescription::plugin()
{
    // TODO implement
}              




NilPluginProcessor::NilPluginProcessor(NilPluginProcessorData* data)
    : mData(data) {}
    
NilPluginProcessor::~NilPluginProcessor()
{
    delete mData;
}


AudioProcessorDescription* NilPluginProcessor::description()
{
    // TODO implement
}

void NilPluginProcessor::accept(Message message)
{
    // TODO implement
}

void NilPluginProcessor::prepare(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
{
    // TODO implement
}

void NilPluginProcessor::process(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
{
    // TODO implement
}

void NilPluginProcessor::cleanup(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
{
    // TODO implement
}

AudioPlugin* NilPluginProcessor::plugin()
{
    // TODO implement
}
    
void* NilPluginProcessor::nativePluginInstance()
{
    // TODO implement
}
                                      



NilPlugin::NilPlugin(NilPluginData* data)
    : mData(data) {}

NilPlugin::~NilPlugin()
{
    delete mData;
}

AudioPluginProcessorDescription* NilPlugin::description()
{
    return vst2PluginDescription();
}

AudioPluginProcessor* NilPlugin::createProcessor()
{
    return createNilPluginProcessor();
}               

void* NilPlugin::nativePlugin()
{
    // TODO implement
}

NilPluginDescription* NilPlugin::vst2PluginDescription()
{                       
    if(!mData->description)
    {                                                   
        // create and init description
    }   
    return mData->description;
}

NilPluginProcessor* NilPlugin::createNilPluginProcessor()
{
    // TODO implement
} 

std::list<NilPlugin*> NilPlugin::vst2Plugins(FilePath path)
{                                                
    // TODO implement
}

} // namespace
} // namespace                    
