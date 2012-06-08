/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
/**
    @file   sclaudiox/processor/plugin/au.mm
    @author Hans Hoglund
 */

#include <CoreAudio/CoreAudio.h>
#include <CoreAudioKit/AUGenericView.h>
#include <AudioUnit/AudioUnit.h>
#include <AudioUnit/AUCocoaUIView.h>

#include "sclaudiox/processor/plugin/au.h"
#include "sclaudiox/util/misc.h"


namespace doremir {
namespace scl {

struct AudioUnitDescriptionData
{                          
    AudioUnit*          unit;
    AudioUnitProcessor* processor;
};

struct AudioUnitProcessorData
{
    AudioUnit* unit;
    AudioUnitDescription* description; // lazy

    int                   maxChannels;
    int                   maxFrames;
    AudioBufferList*      bufferList;   
    AudioTimeStamp*       timeStamp;
    
    AudioComponentInstance instance; // the instance
};

struct AudioUnitData
{
    AudioUnitDescription* description; // lazy
    AudioUnitProcessor* example; // lazy, needed by the descriptor
    
    AudioComponent component; // the component
}; 


// =======================================================================================

namespace
{           
    String fromOsStatus(OSStatus err)
    {
        return fromSimpleString<kDefaultCharSet>(GetMacOSStatusErrorString(err));        
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
}

class AudioUnitError : public AudioPluginError
{   
public:       
    AudioUnitError(AudioUnit* plugin, AudioUnitProcessor* processor, OSStatus status)
        : AudioPluginError(plugin, processor)
        , mStatus(status) {}
        
    String message()
    {
        return "Audio Unit: " + fromOsStatus(mStatus);
    }              
private:
    OSStatus mStatus;
};      

class AudioUnitProcessorError : public DspError
{   
public:       
    AudioUnitProcessorError(AudioUnitProcessor* processor, OSStatus status)
        : mProcessor(processor)
        , mStatus(status) {}
        
    String message()
    {
        return "Audio Unit: " + fromOsStatus(mStatus);
    }              
private:        
    AudioUnitProcessor* mProcessor;
    OSStatus mStatus;
};          

namespace
{
    inline void checkOsStatus(AudioUnit* plugin, AudioUnitProcessor* processor, OSStatus status)
    {
        if (status != noErr)  
            throw AudioUnitError(plugin, processor, status);
    }
    
    inline void checkOsStatusProc(AudioUnitProcessor* processor, OSStatus status)
    {
        if (status != noErr)  
            throw AudioUnitProcessorError(processor, status);
    }
}


// =======================================================================================

AudioUnitDescription::AudioUnitDescription(AudioUnitDescriptionData* data)
    : mData(data) {}
    
AudioUnitDescription::~AudioUnitDescription()
{        
    delete mData;
}

String AudioUnitDescription::name()
{         
    CFStringRef cfName;
    AudioComponentCopyName(mData->unit->mData->component, &cfName);
    return fromCFString(cfName);
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

// namespace
// {    
//     void channelInfo(AUChannelInfo info)
//     {
//         if (info.inChannels == -1 && info.outChannels == -1)
//         {
//             // (a, a)
//         }
//         else
//         if ( (info.inChannels == -1 && info.outChannels == -2)
//                     || 
//              (info.inChannels == -2 && info.outChannels == -1) )
//         {
//             // (a, b)
//         }   
//         else
//         if (info.inChannels == -1)
//         {
//             // (a, n)
//         }
//     }     
// }          

// TODO throws      
void AudioUnitDescription::assureProcessor()
{
    if (mData->processor) 
        return;

    mData->unit->assureProcessor();
    mData->processor = mData->unit->mData->example;
}           


namespace
{
    // FIXME
    void getNumChannels (AudioComponentInstance instance, int& numIns, int& numOuts)
    {
        numIns = 0;
        numOuts = 0;

        AUChannelInfo supportedChannels [128];
        UInt32 supportedChannelsSize = sizeof (supportedChannels);

        if (AudioUnitGetProperty (instance, kAudioUnitProperty_SupportedNumChannels, kAudioUnitScope_Global,
                                  0, supportedChannels, &supportedChannelsSize) == noErr
            && supportedChannelsSize > 0)
        {
            for (int i = 0; i < supportedChannelsSize / sizeof (AUChannelInfo); ++i)
            {
                numIns = math::maximum(numIns, (int) supportedChannels[i].inChannels);
                numOuts = math::maximum(numOuts, (int) supportedChannels[i].outChannels);
            }
        }
        else
        {
                // (this really means the plugin will take any number of ins/outs as long
                // as they are the same)
            numIns = numOuts = 2;
        }
    }
    
}

int AudioUnitDescription::numberOfInputs()
{           
    int inputs, outputs;
    assureProcessor();
    getNumChannels(mData->processor->mData->instance, inputs, outputs);
    return inputs;
}

int AudioUnitDescription::numberOfOutputs()
{
    int inputs, outputs;
    assureProcessor();
    getNumChannels(mData->processor->mData->instance, inputs, outputs);
    return outputs;
}

int AudioUnitDescription::numberOfBuses()
{
    return 0;
}


AudioUnitVersion AudioUnitDescription::version()
{
    // FIXME
}                                  

AudioPlugin* AudioUnitDescription::plugin()
{    
    return mData->unit;
}              




// =======================================================================================

AudioUnitProcessor::AudioUnitProcessor(AudioUnitProcessorData* data)
    : mData(data) {}
    
AudioUnitProcessor::~AudioUnitProcessor()
{
    delete mData;
}

// FIXME throws
void AudioUnit::assureProcessor()
{
    if (mData->example)
        return;
    mData->example = createAudioUnitProcessor();
}

AudioProcessorDescription* AudioUnitProcessor::description()
{
    if(!mData->description)
    {                                                   
        AudioUnitDescriptionData* data = new AudioUnitDescriptionData();
        data->unit      = mData->unit;
        data->processor = const_cast<AudioUnitProcessor*>(this);
        mData->description = new AudioUnitDescription(data);
    }   
    return mData->description;
}

/*
    FIXME assumes Midi messages
 */
 
void AudioUnitProcessor::accept(Message message)
{
    OSStatus err;
    int status, data1, data2;
    messageTo(message, status, data1, data2);

    if (!isSysEx(status))
    {
        err = MusicDeviceMIDIEvent( mData->instance, status, data1, data2, 0 );
    }
    else
    {         
        err = noErr;
    }    
    checkOsStatusProc(this, err);
}

namespace
{
    /**
        Allocate a buffer list, containing one buffer with the given channel configuration.
     */
    AudioBufferList* allocateBufferList(int numChannels, int numFrames)
    {                        
        size_t bufferSize = sizeof(Float32) * numChannels * numFrames;
          
        AudioBufferList* list = (AudioBufferList*) 
            calloc(1, 
                sizeof(UInt32) + // mNumberBuffers
                sizeof(UInt32) + // mNumberChannels
                sizeof(UInt32) + // mDataByteSize
                bufferSize);
        
        list->mNumberBuffers = 1;
        list->mBuffers[0].mNumberChannels = numChannels;
        list->mBuffers[0].mDataByteSize   = bufferSize;
        return list;
    }

    void freeBufferList(AudioBufferList* list)
    {              
        free(list);
    }
}          


void AudioUnitProcessor::prepare(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
{                                                       
    OSStatus err = AudioUnitInitialize(mData->instance);
    checkOsStatusProc(this, err);

    mData->maxChannels = signal.numberOfChannels;
    mData->maxFrames   = signal.numberOfFrames;
    mData->bufferList  = allocateBufferList(mData->maxChannels, mData->maxFrames);
    mData->timeStamp   = new AudioTimeStamp;
    FillOutAudioTimeStampWithSampleTime(*mData->timeStamp, 
        ((Float64) info.sampleCount) / ((Float64) info.sampleRate));
}

void AudioUnitProcessor::process(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
{
    OSStatus                   err;      
    AudioUnitRenderActionFlags flags = 0;

    FillOutAudioTimeStampWithSampleTime(*mData->timeStamp, 
        ((Float64) info.sampleCount) / ((Float64) info.sampleRate));

    // err = AudioUnitRender (
    //     mData->instance,
    //     &flags,
    //     mData->timeStamp,
    //     0,
    //     mData->maxFrames,
    //     mData->bufferList
    // );       
    
    Sample* buffer = (Sample*) mData->bufferList->mBuffers[0].mData;
    int numChannels = mData->maxChannels;
    int numFrames = mData->maxFrames;

    float sig;
    for(int channel = 0; channel < numChannels; ++channel)
    {
        for(int frame = 0; frame < numFrames; ++frame)
        {                                              
            // signal.data[channel * numFrames + frame] = sig;
            // sig = sig + 0.1;
            signal.data[channel * numFrames + frame] = buffer[frame * numChannels + channel];
        }
    }   
    checkOsStatusProc(this, err);
}

void AudioUnitProcessor::cleanup(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
{
    OSStatus err = AudioUnitUninitialize(mData->instance);
    checkOsStatusProc(this, err);
    freeBufferList(mData->bufferList);   
    delete mData->timeStamp;
}

AudioPlugin* AudioUnitProcessor::plugin()
{
    return mData->unit;
}
    
void* AudioUnitProcessor::nativePluginInstance()
{
    return (void*) mData->instance;
}
                                      



// =======================================================================================

AudioUnit::AudioUnit(AudioUnitData* data)
    : mData(data) {}

AudioUnit::~AudioUnit()
{
    delete mData;
}

AudioPluginProcessorDescription* AudioUnit::description()
{
    return audioUnitDescription();
}

AudioPluginProcessor* AudioUnit::createProcessor()
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
        data->unit = this;
        data->processor = NULL;                                      
        mData->description = new AudioUnitDescription(data);
    }   
    return mData->description;
}

AudioUnitProcessor* AudioUnit::createAudioUnitProcessor()
{                                            
    AudioComponentInstance instance;
    OSStatus err = AudioComponentInstanceNew(mData->component, &instance);
    checkOsStatus(this, NULL, err);

    AudioUnitProcessorData* data = new AudioUnitProcessorData();
    data->unit        = this;
    data->description = NULL;
    data->instance    = instance;

    return new AudioUnitProcessor(data);     
} 


namespace
{
    /* 
        Find all audio components matching the given description.
     */
    std::list<AudioComponent> findAudioComponents(AudioComponentDescription* description)
    {                                                
        int numberOfComponents = AudioComponentCount(description);

        AudioComponent            component = NULL;
        std::list<AudioComponent> components;
        
        for(int i = 0; i < numberOfComponents; ++i)
        {                        
            component = AudioComponentFindNext(component, description);
            components.push_back(component);
        } 
        return components;
    }   
    
    std::list<AudioComponent> findAllAudioComponents()
    {
        AudioComponentDescription descr;
        descr.componentType         = 0;
        descr.componentSubType      = 0;
        descr.componentManufacturer = 0;
        descr.componentFlags        = 0;
        descr.componentFlagsMask    = 0;

        return findAudioComponents(&descr);
    }

    std::list<AudioComponent> findDLSMusicDevice()
    {
        AudioComponentDescription descr;
        descr.componentType         = kAudioUnitType_MusicDevice;
        descr.componentSubType      = kAudioUnitSubType_DLSSynth;
        descr.componentManufacturer = kAudioUnitManufacturer_Apple;

        return findAudioComponents(&descr);
    }

    AudioUnit* fromComponent(AudioComponent component)
    {                        
        AudioUnitData* data = new AudioUnitData();
        data->description = NULL;
        data->example     = NULL;
        data->component   = component;
        return new AudioUnit(data);
    }   
                      
}

std::list<AudioUnit*> AudioUnit::audioUnits()
{                                                
    std::list<AudioComponent> components = findAllAudioComponents();
    return list::transform(fromComponent, components);
}

AudioUnit* AudioUnit::dlsMusicDevice()
{                                                
    std::list<AudioComponent> components = findDLSMusicDevice();
    if (components.size() < 1)
        return NULL;
    return fromComponent(list::index(components, 0));
}


} // namespace
} // namespace                    
