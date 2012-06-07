/*
    ScoreCleaner Audio Engine

    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef  _SCLAUDIOX_PROCESSOR_PLUGIN_AU
#define  _SCLAUDIOX_PROCESSOR_PLUGIN_AU

#include "sclaudiox/processor/plugin.h"

namespace doremir {
namespace scl {

struct AudioUnitVersion
{
    Int16 major;
    Int16 minor;
    Int16 dot;
};

class AudioUnitDescriptionData;
class AudioUnitProcessorData;
class AudioUnitData;

/**
    Description of an AudioUnit plugin or processor.
 */
class SCLAUDIO_API AudioUnitDescription : public PluginAudioProcessorDescription
{
public:
    AudioUnitDescription(AudioUnitDescriptionData* data);    
    ~AudioUnitDescription();    

    /* (see AudioProcessorDescription) */
    String name() const;

    /* (see AudioProcessorDescription) */
    bool isAtomic() const;

    /* (see AudioProcessorDescription) */
    bool isStateful() const;

    /* (see AudioProcessorDescription) */
    bool isPlugin() const;

    /* (see AudioProcessorDescription) */
    int numberOfInputs() const;

    /* (see AudioProcessorDescription) */
    int numberOfOutputs() const;

    /* (see AudioProcessorDescription) */
    int numberOfBuses() const;

    /* (see PluginAudioProcessorDescription) */
    AudioPlugin* plugin();

    /** Returns the version of this audio unit. */
    AudioUnitVersion  version();

private:
    AudioUnitDescriptionData* mData;
};

/**
    An AudioUnit plugin instance.
 */
class SCLAUDIO_API AudioUnitProcessor : public PluginAudioProcessor
{
public:     
    AudioUnitProcessor(AudioUnitProcessorData* data);    
    ~AudioUnitProcessor();    

    /* (see AudioProcessor) */
    AudioProcessorDescription* description() const;

    /* (see AudioProcessor) */
    void accept(Message message);

    /* (see AudioProcessor) */
    void prepare(AudioProcessingInformation& info, AudioProcessingBuffer &signal);

    /* (see AudioProcessor) */
    void process(AudioProcessingInformation& info, AudioProcessingBuffer &signal);

    /* (see AudioProcessor) */
    void cleanup(AudioProcessingInformation& info, AudioProcessingBuffer &signal);

    /* (see PluginAudioProcessor) */
    AudioPlugin* plugin();
        
    /* (see PluginAudioProcessor) */
    void* nativePluginInstance();

private:                  
    AudioUnitProcessorData* mData; 
};

/**
    An AudioUnit plugin.
 */
class SCLAUDIO_API AudioUnit : public AudioPlugin
{
public:
    AudioUnit(AudioUnitData* data);
    ~AudioUnit();

    /* (see AudioPlugin) */
    PluginAudioProcessorDescription* description();

    /* (see AudioPlugin) */
    PluginAudioProcessor* createProcessor();

    /* (see AudioPlugin) */
    void* nativePlugin();
    
    /** Creates an AudioUnit-specific description of this plugin. */
    AudioUnitDescription* audioUnitDescription();

    /** Creates an AudioUnit-specific instance of this plugin. */
    AudioUnitProcessor* createAudioUnitProcessor();

    /** Returns a list of available AudioUnit instances. */
    static std::list<AudioUnit*> audioUnits();

private:     
    AudioUnitData* mData;
};



} // namespace
} // namespace

#endif
