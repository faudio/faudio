/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
/**
    @file   sclaudiox/processor/plugin/au.mm
    @author Hans Hoglund
 */

#include <AudioUnit/AudioUnit.h>
#include <AudioUnit/AUCocoaUIView.h>
#include <CoreAudioKit/AUGenericView.h>

#include "sclaudiox/processor/plugin/au.h"
#include "sclaudiox/util/misc.h"


namespace doremir {
namespace scl {

struct AudioUnitDescriptionData
{                          
    AudioUnit* unit;
    AudioComponent component;
};
struct AudioUnitProcessorData
{
    AudioUnit* unit;
    AudioUnitDescription* description;
    AudioComponentInstance instance;
};
struct AudioUnitData
{
    AudioUnitDescription* description;
    AudioComponent component;    
};


AudioUnitDescription::AudioUnitDescription(AudioUnitDescriptionData* data)
    : mData(data) {}
    
AudioUnitDescription::~AudioUnitDescription()
{
    delete mData;
}

String fromCFString(CFStringRef cfstr)
{   
    int length = CFStringGetLength(cfstr);
    char* buffer = new char[length + 1];
    CFStringGetCString(cfstr, buffer, length + 1, kCFStringEncodingUTF8);

    String str = fromSimpleString<kUtf8>(buffer);
    delete buffer;
    return str;
}

String AudioUnitDescription::name() const
{         
    CFStringRef cfName;
    AudioComponentCopyName(mData->component, &cfName);
    return fromCFString(cfName);
}                  
bool AudioUnitDescription::isAtomic() const
{
    return true;
}
bool AudioUnitDescription::isStateful() const
{
    return true;
}
bool AudioUnitDescription::isPlugin() const
{
    return true;
}

int AudioUnitDescription::numberOfInputs() const
{
    // FIXME
}

int AudioUnitDescription::numberOfOutputs() const
{
    // FIXME
}

int AudioUnitDescription::numberOfBuses() const
{
    // FIXME
}


AudioUnitVersion AudioUnitDescription::version()
{
    // FIXME
}                                  

AudioPlugin* AudioUnitDescription::plugin()
{
    // FIXME
}              




AudioUnitProcessor::AudioUnitProcessor(AudioUnitProcessorData* data)
    : mData(data) {}
    
AudioUnitProcessor::~AudioUnitProcessor()
{
    delete mData;
}


AudioProcessorDescription* AudioUnitProcessor::description() const
{
    // FIXME
}

void AudioUnitProcessor::accept(Message message)
{
    // FIXME
}

void AudioUnitProcessor::prepare(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
{
    // FIXME
}

void AudioUnitProcessor::process(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
{
    // FIXME
}

void AudioUnitProcessor::cleanup(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
{
    // FIXME
}

AudioPlugin* AudioUnitProcessor::plugin()
{
    // FIXME
}
    
void* AudioUnitProcessor::nativePluginInstance()
{
    return (void*) mData->instance;
}
                                      



AudioUnit::AudioUnit(AudioUnitData* data)
    : mData(data) {}

AudioUnit::~AudioUnit()
{
    delete mData;
}

PluginAudioProcessorDescription* AudioUnit::description()
{
    return audioUnitDescription();
}

PluginAudioProcessor* AudioUnit::createProcessor()
{
    return createAudioUnitProcessor();
}               

void* AudioUnit::nativePlugin()
{
    return (void*) mData->component;
}

AudioUnitDescription* AudioUnit::audioUnitDescription()
{                       
    if(!mData->description)
    {                                                   
        AudioUnitDescriptionData* data = new AudioUnitDescriptionData();
        data->component = mData->component;
        mData->description = new AudioUnitDescription(data);
    }   
    return mData->description;
}

AudioUnitProcessor* AudioUnit::createAudioUnitProcessor()
{
    // FIXME
} 


namespace
{
    /* 
        Find all audio components matching the given description.
     */
    std::list<AudioComponent> findAudioComponents(AudioComponentDescription* description)
    {                                                
        // use this if last doesn't return NULL
        int numberOfAudioUnits = AudioComponentCount(description);

        AudioComponent            component = NULL;
        std::list<AudioComponent> components;
        // do
        for (int i = 0; i < numberOfAudioUnits; ++i)
        {                        
            component = AudioComponentFindNext(component, description);
            components.push_back(component);
        } 
        // while (component != NULL);
        return components;
    }


    AudioUnit* fromComponent(AudioComponent component)
    {                        
        AudioUnitData* data = new AudioUnitData();
        data->description = NULL;
        data->component = component;
        return new AudioUnit(data);
    }   
                      
}

std::list<AudioUnit*> AudioUnit::audioUnits()
{                                                
    // AudioComponentDescription* anyComponent = NULL;           

    AudioComponentDescription descr;
    // descr.componentType         = kAudioUnitType_MusicDevice;
    // descr.componentSubType      = kAudioUnitSubType_DLSSynth;
    // descr.componentManufacturer = kAudioUnitManufacturer_Apple;
    descr.componentType         = 0;
    descr.componentSubType      = 0;
    descr.componentManufacturer = 0;
    descr.componentFlags        = 0;
    descr.componentFlagsMask    = 0;
     
    std::list<AudioComponent> components = findAudioComponents(&descr);

    // std::list<AudioUnit*> units;    
    // std::transform(components.begin(), components.end(), units.begin(), fromComponent);

    return list::transform(fromComponent, components);
}

} // namespace
} // namespace                    
