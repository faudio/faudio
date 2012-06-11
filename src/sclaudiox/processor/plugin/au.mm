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
#include "sclaudiox/util/logging.h"


namespace doremir {
namespace scl {

struct AudioUnitDescriptionData
{                          
    AudioUnit*          unit;
    AudioUnitProcessor* processor;
};

struct AudioUnitProcessorData
{
    AudioUnit*                  unit;
    AudioUnitDescription*       description; // lazy

    AudioUnitRenderActionFlags  renderActionFlags;
    AudioTimeStamp              timeStamp;
    int                         busNumber;
    int                         maxChannels;
    int                         maxFrames;
    AudioBufferList*            bufferList;   
    
    AudioComponentInstance      instance; // the instance
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
    String fromOSStatus(OSStatus err)
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
        return "Audio Unit: " + fromOSStatus(mStatus);
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
        return "Audio Unit: " + mProcessor->description()->name() + ": " + fromOSStatus(mStatus);
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

    inline void failOsStatus(AudioUnit* plugin, AudioUnitProcessor* processor, OSStatus status)
    {
        throw AudioUnitError(plugin, processor, status);
    }
    
    inline void failOsStatusProc(AudioUnitProcessor* processor, OSStatus status)
    {
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
    typedef std::list< std::list<int> > ChannelList;

    /** 
        Returns a list on the form ((2,2), (1,1)) representing the channel configuration of each bus 
    */
    inline ChannelList getChannelList(AudioComponentInstance instance)
    {
        ChannelList channelList;

        AUChannelInfo supportedChannels [128];
        UInt32 supportedChannelsSize = sizeof (supportedChannels);

        if (AudioUnitGetProperty (instance, kAudioUnitProperty_SupportedNumChannels, kAudioUnitScope_Global,
                                  0, supportedChannels, &supportedChannelsSize) == noErr
                   && 
            supportedChannelsSize > 0)
        {
            for (int i = 0; i < supportedChannelsSize / sizeof (AUChannelInfo); ++i)
            {
                std::list<int> channels;
                channels.push_back(supportedChannels[i].inChannels);
                channels.push_back(supportedChannels[i].outChannels);
                channelList.push_back(channels);
            }
        }
        return channelList;
    }

    
    inline void getNumChannels(AudioComponentInstance instance, int& numIns, int& numOuts)
    {
        numIns = 0;
        numOuts = 0;

        AUChannelInfo supportedChannels [128];
        UInt32 supportedChannelsSize = sizeof (supportedChannels);

        if (AudioUnitGetProperty (instance, kAudioUnitProperty_SupportedNumChannels, kAudioUnitScope_Global,
                                  0, supportedChannels, &supportedChannelsSize) == noErr
                   && 
            supportedChannelsSize > 0)
        {
            for (int i = 0; i < supportedChannelsSize / sizeof (AUChannelInfo); ++i)
            {
                numIns = math::maximum(numIns, (int) supportedChannels[i].inChannels);
                numOuts = math::maximum(numOuts, (int) supportedChannels[i].outChannels);
            }
        }
        else
        {
            numIns = numOuts = 1;
        }
    }

    inline int getNumBuses(AudioComponentInstance instance)
    {
        AUChannelInfo supportedChannels [128];
        UInt32 supportedChannelsSize = sizeof (supportedChannels);

        if (AudioUnitGetProperty (instance, kAudioUnitProperty_SupportedNumChannels, kAudioUnitScope_Global,
                                  0, supportedChannels, &supportedChannelsSize) == noErr)
        {
            return supportedChannelsSize / sizeof (AUChannelInfo);
        }
        else
        {
            return -1;
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

int AudioUnitDescription::numberOfAUBuses()
{        
    return getNumBuses(mData->processor->mData->instance);
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

AudioProcessorDescription* AudioUnitProcessor::description()
{
    if (!mData->description)
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
        if (err = MusicDeviceMIDIEvent(mData->instance, status, data1, data2, 0))
            failOsStatusProc(this, err);
    }
    else
    {         
    }    
}

namespace
{         
    // TODO some utility function for this, in case we ever need to switch from 32-bit
    inline Sample float32ToSample(Float32 x) { return x; }
    inline Float32 sampleToFloat32(Sample x) { return x; }
    
    /*
        Allocate a buffer list.
     */
    AudioBufferList* createBufferList(int numBuffers, int numChannels, int numFrames)
    {                        
        size_t bufferSize     = sizeof(Float32) * numChannels * numFrames;
        size_t bufferListSize = sizeof(UInt32) + bufferSize * numBuffers;
        
        AudioBufferList* list = (AudioBufferList*) calloc(1, bufferListSize);
        
        list->mNumberBuffers = numBuffers;
        
        for(int i = 0; i < numBuffers; ++i)
        {             
            Float32 * buffer = (Float32*) calloc(1, bufferSize);

            list->mBuffers[i].mNumberChannels = numChannels;
            list->mBuffers[i].mDataByteSize   = bufferSize;
            list->mBuffers[i].mData           = buffer;            
        }
        return list;
    }

    void freeBufferList(AudioBufferList* list)
    {          
        for (int i = 0; i < list->mNumberBuffers; ++i)      
            free(list->mBuffers[i].mData);
        free(list);
    }   
}          


void AudioUnitProcessor::prepare(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
{                                                       
    OSStatus err;
    if (err = AudioUnitInitialize(mData->instance))
        failOsStatusProc(this, err);

    mData->maxChannels       = 2/*signal.numberOfChannels*/;
    mData->maxFrames         = 256/*signal.numberOfFrames*/;
    mData->bufferList        = createBufferList(2, 1/*mData->maxChannels*/, 256/*mData->maxFrames*/);
    mData->renderActionFlags = 0;
    mData->busNumber         = 0;
    
    UInt32 sampleCount = mData->maxFrames;
    Float64 sampleRate = info.sampleRate;
                                    
    AudioUnitSetProperty (mData->instance, kAudioUnitProperty_MaximumFramesPerSlice, kAudioUnitScope_Global, 0, 
                          &sampleCount, sizeof(sampleCount));
    
    AudioUnitSetProperty (mData->instance, kAudioUnitProperty_SampleRate, kAudioUnitScope_Global, 0, 
                          &sampleRate, sizeof(sampleRate));

SCL_WRITE_LOG( ">>>>> " << sampleCount << "\n" );
SCL_WRITE_LOG( ">>>>> " << sampleRate << "\n" );
SCL_WRITE_LOG( ">>>>> " << toString(getChannelList(mData->instance)) << "\n" );

}

static int c = 0;
void AudioUnitProcessor::process(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
{
    OSStatus err;      
                                                               
    Float64 time = ((Float64) info.sampleCount) / ((Float64) info.sampleRate);
    FillOutAudioTimeStampWithSampleTime(mData->timeStamp, time);

//     if (!(c++ % 10))
//     {
// SCL_WRITE_LOG( "Inst  : " << mData->instance << "\n" );
// SCL_WRITE_LOG( "Flags : " << mData->renderActionFlags << "\n" );
// SCL_WRITE_LOG( "Time  : " << mData->timeStamp.mSampleTime << "\n" );
// SCL_WRITE_LOG( "Bus   : " << mData->busNumber << "\n" );
// SCL_WRITE_LOG( "Frs   : " << signal.numberOfFrames << "\n" );
// SCL_WRITE_LOG( "Bufs  : " << mData->bufferList->mNumberBuffers << "\n" );
// for (int i = 0; i < mData->bufferList->mNumberBuffers; ++i) {
//   SCL_WRITE_LOG( "  Bch :  " << mData->bufferList->mBuffers[i].mNumberChannels << "\n" );
//   SCL_WRITE_LOG( "  Bsz :  " << mData->bufferList->mBuffers[i].mDataByteSize << "\n" );
// }
// SCL_WRITE_LOG( "=======\n" );
// 
//     }

    if (err = AudioUnitRender (mData->instance, &mData->renderActionFlags, &mData->timeStamp,
                               mData->busNumber, signal.numberOfFrames, mData->bufferList))       
        failOsStatusProc(this, err);
    
    int numChannels = mData->maxChannels;
    int numFrames   = signal.numberOfFrames;

    for(int channel = 0; channel < numChannels; ++channel)
    {
        // TODO assert (channel < mData->bufferList->mNumberBuffers)
        // TODO assert (frame < mData->bufferList->mDataByteSize / sizeof(Float32))
        Float32* buffer = (Float32*) mData->bufferList->mBuffers[channel].mData;

        float x;
        for(int frame = 0; frame < numFrames; ++frame)
        {            
            // if (!channel)
            //     signal.data[channel * numFrames + frame] = sin(time * 50);
            // else
            //     signal.data[channel * numFrames + frame] = sin(time * 52);
            
            signal.data[channel * numFrames + frame] = buffer[frame];

            // signal.data[channel * numFrames + frame] = 0;
        }
    }   
}

void AudioUnitProcessor::cleanup(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
{
    OSStatus err;
    if (err = AudioUnitUninitialize(mData->instance))
        failOsStatusProc(this, err);
    freeBufferList(mData->bufferList);   
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

// FIXME throws
void AudioUnit::assureProcessor()
{
    if (mData->example)
        return;
    mData->example = createAudioUnitProcessor();
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
    if (!mData->description)
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
