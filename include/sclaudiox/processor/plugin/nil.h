/*
    ScoreCleaner Audio Engine

    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef  _SCLAUDIOX_PROCESSOR_PLUGIN_VST2
#define  _SCLAUDIOX_PROCESSOR_PLUGIN_VST2

#include "sclaudiox/processor/plugin.h"

namespace doremir {
namespace scl {

class NilPluginDescriptionData;
class NilPluginProcessorData;
class NilPluginData;

/**
    Description of an NilPlugin or processor.
 */
class NilPluginDescription : public AudioPluginProcessorDescription
{
public:
    NilPluginDescription(NilPluginDescriptionData* data);    
    ~NilPluginDescription();    

    /* (see AudioProcessorDescription) */
    String name();

    /* (see AudioProcessorDescription) */
    bool isAtomic();

    /* (see AudioProcessorDescription) */
    bool isStateful();

    /* (see AudioProcessorDescription) */
    bool isPlugin();

    /* (see AudioProcessorDescription) */
    int numberOfInputs();

    /* (see AudioProcessorDescription) */
    int numberOfOutputs();

    /* (see AudioProcessorDescription) */
    int numberOfBuses();

    /* (see AudioPluginProcessorDescription) */
    AudioPlugin* plugin();

private:
    NilPluginDescriptionData* mData;
};

/**
    An NilPlugin instance.
 */
class NilPluginProcessor : public AudioPluginProcessor
{
public:     
    NilPluginProcessor(NilPluginProcessorData* data);    
    ~NilPluginProcessor();    

    /* (see AudioProcessor) */
    AudioProcessorDescription* description();

    /* (see AudioProcessor) */
    void accept(Message message);

    /* (see AudioProcessor) */
    void prepare(AudioProcessingInformation& info, AudioProcessingBuffer &signal);

    /* (see AudioProcessor) */
    void process(AudioProcessingInformation& info, AudioProcessingBuffer &signal);

    /* (see AudioProcessor) */
    void cleanup(AudioProcessingInformation& info, AudioProcessingBuffer &signal);

    /* (see AudioPluginProcessor) */
    AudioPlugin* plugin();
        
    /* (see AudioPluginProcessor) */
    void* nativePluginInstance();

private:                  
    NilPluginProcessorData* mData; 
};

/**
    An template audio plugin, useful as a starting point for plugin host implementations.
 */
class NilPlugin : public AudioPlugin
{
public:
    NilPlugin(NilPluginData* data);
    ~NilPlugin();

    /* (see AudioPlugin) */
    AudioPluginProcessorDescription* description();

    /* (see AudioPlugin) */
    AudioPluginProcessor* createProcessor();

    /* (see AudioPlugin) */
    void* nativePlugin();
    
    /** Creates an Nil-specific description of this plugin. */
    NilPluginDescription* nilPluginDescription();

    /** Creates an Nil-specific instance of this plugin. */
    NilPluginProcessor* createNilPluginProcessor();

    // custom factory methods

private:     
    NilPluginData* mData;
};



} // namespace
} // namespace

#endif
