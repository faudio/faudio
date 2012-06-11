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
class SCLAUDIO_API AudioUnitDescription : public AudioPluginProcessorDescription
{
public:
    AudioUnitDescription(AudioUnitDescriptionData* data);    
    ~AudioUnitDescription();    

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
    int numberOfAUBuses(); // FIXME

    /* (see AudioPluginProcessorDescription) */
    AudioPlugin* plugin();

    /** Returns the version of this audio unit. */
    AudioUnitVersion  version();

private:
    void assureProcessor();
    void assureChannelInfo();
    AudioUnitDescriptionData* mData;
};

/**
    An AudioUnit plugin instance.
 */
class SCLAUDIO_API AudioUnitProcessor : public AudioPluginProcessor
{
public:     
    AudioUnitProcessor(AudioUnitProcessorData* data);    
    ~AudioUnitProcessor();    

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
    friend class AudioUnitDescription;
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
    AudioPluginProcessorDescription* description();

    /* (see AudioPlugin) */
    AudioPluginProcessor* createProcessor();

    /* (see AudioPlugin) */
    void* nativePlugin();
    
    /** Creates an AudioUnit-specific description of this plugin. */
    AudioUnitDescription* audioUnitDescription();

    /** Creates an AudioUnit-specific instance of this plugin. */
    AudioUnitProcessor* createAudioUnitProcessor();

    /** Returns a list of available AudioUnit instances. */
    static std::list<AudioUnit*> audioUnits();

    /** Returns the DLSMusicDevice software synth, or `NULL` if it is not available. */
    static AudioUnit* dlsMusicDevice();

private:      
    friend class AudioUnitDescription;
    friend class AudioUnitProcessor;   
    void assureProcessor();
    AudioUnitData* mData;
};



} // namespace
} // namespace

#endif
