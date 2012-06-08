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

class Vst2PluginDescriptionData;
class Vst2PluginProcessorData;
class Vst2PluginData;

/**
    Description of an Vst 2 plugin or processor.
 */
class Vst2PluginDescription : public AudioPluginProcessorDescription
{
public:
    Vst2PluginDescription(Vst2PluginDescriptionData* data);    
    ~Vst2PluginDescription();    

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
    Vst2PluginDescriptionData* mData;
};

/**
    An Vst 2 plugin instance.
 */
class Vst2PluginProcessor : public AudioPluginProcessor
{
public:     
    Vst2PluginProcessor(Vst2PluginProcessorData* data);    
    ~Vst2PluginProcessor();    

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
    Vst2PluginProcessorData* mData; 
};

/**
    An Vst 2 plugin.
 */
class Vst2Plugin : public AudioPlugin
{
public:
    Vst2Plugin(Vst2PluginData* data);
    ~Vst2Plugin();

    /* (see AudioPlugin) */
    AudioPluginProcessorDescription* description();

    /* (see AudioPlugin) */
    AudioPluginProcessor* createProcessor();

    /* (see AudioPlugin) */
    void* nativePlugin();
    
    /** Creates an VST-specific description of this plugin. */
    Vst2PluginDescription* vst2PluginDescription();

    /** Creates an VST-specific instance of this plugin. */
    Vst2PluginProcessor* createVst2PluginProcessor();

    /** Load a plugin from the given file. */
    static Vst2Plugin* loadVst2Plugin(FilePath path);

    static std::list<Vst2Plugin*> vst2Plugins(FilePath path);

private:     
    Vst2PluginData* mData;
};



} // namespace
} // namespace

#endif
