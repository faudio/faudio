
#include <fa/fa.h>

#define NO_THREAD_T // TODO
#include <fa/util.h>
#undef NO_THREAD_T

#include <CoreAudio/CoreAudio.h>
#include <CoreAudioKit/AUGenericView.h>
#include <AudioUnit/AudioUnit.h>
#include <AudioUnit/AUCocoaUIView.h>


string_t from_os_status(OSStatus err)
{                
    return string((char*) GetMacOSStatusErrorString(err));
}

string_t component_name(AudioComponent component)
{
    CFStringRef cf_str;
    AudioComponentCopyName(component, &cf_str);
    return fa_string_from_native((void*) cf_str);
}

inline static 
void instance_num_channels(AudioComponentInstance instance, int* numIns, int* numOuts)
{
    *numIns = 0;
    *numOuts = 0;

    OSStatus err;
    AUChannelInfo supportedChannels [128];
    UInt32 supportedChannelsSize = sizeof (supportedChannels);

    if ((err = AudioUnitGetProperty (instance, kAudioUnitProperty_SupportedNumChannels, kAudioUnitScope_Global,
                              0, supportedChannels, &supportedChannelsSize)
               &&
        supportedChannelsSize > 0))
    {
        assert(false && "Could not get number of channels");
    }
        
    for (int i = 0; i < supportedChannelsSize / sizeof (AUChannelInfo); ++i)
    {                     
        *numIns = MAX(*numIns, (int) supportedChannels[i].inChannels);
        *numOuts = MAX(*numOuts, (int) supportedChannels[i].outChannels);
    }
}

inline static 
int instance_num_inputs(AudioComponentInstance instance)
{
    int inputs, outputs;
    instance_num_channels(instance, &inputs, &outputs);
    return inputs;
}

inline static 
int instance_num_outputs(AudioComponentInstance instance)
{
    int inputs, outputs;
    instance_num_channels(instance, &inputs, &outputs);
    return outputs;
}


void instance_send_midi(AudioComponentInstance instance, int status, int data1, int data2)
{
    OSStatus err;
    if ((err = MusicDeviceMIDIEvent(instance, status, data1, data2, 0))) {
        fa_print_ln(from_os_status(err));
        assert(false && "Could not send note");
    }
    mark_used(err);
}



// TODO localize
AudioUnitRenderActionFlags  gRenderFlags;
int                         gBusNumber;
AudioTimeStamp              gTimeStamp;
AudioBufferList*            gBufferList;

void init_audio_time_stamp (AudioTimeStamp *time_stamp, Float64 inSampleTime) {
   time_stamp->mSampleTime = inSampleTime;
   time_stamp->mHostTime = 0;
   time_stamp->mRateScalar = 0;
   time_stamp->mWordClockTime = 0;
   memset (&time_stamp->mSMPTETime, 0, sizeof (SMPTETime));
   time_stamp->mFlags = kAudioTimeStampSampleTimeValid;
}

/*
    Allocate a buffer list.
 */
AudioBufferList* create_buffer_list(int numBuffers, int numChannels, int numFrames)
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

void instance_prepare(AudioComponentInstance instance)
{
    OSStatus err;

    Float64 sampleRate       = (Float64) 44100;
    Float64 sampleTime       = (Float64) 0;
    UInt32  numberOfChannels = (UInt32)  2;
    UInt32  numberOfFrames   = (UInt32)  44100;        // TODO

    AudioUnitSetProperty (instance, kAudioUnitProperty_SampleRate, kAudioUnitScope_Global, 0,
                          &sampleRate, sizeof(sampleRate));
    AudioUnitSetProperty (instance, kAudioUnitProperty_SampleRate, kAudioUnitScope_Input, 0,
                          &sampleRate, sizeof(sampleRate));
    AudioUnitSetProperty (instance, kAudioUnitProperty_SampleRate, kAudioUnitScope_Output, 0,
                          &sampleRate, sizeof(sampleRate));
    
    AudioUnitSetProperty (instance, kAudioUnitProperty_MaximumFramesPerSlice, kAudioUnitScope_Global, 0,
                          &numberOfFrames, sizeof(numberOfFrames));
    AudioUnitSetProperty (instance, kAudioUnitProperty_MaximumFramesPerSlice, kAudioUnitScope_Input, 0,
                          &numberOfFrames, sizeof(numberOfFrames));
    AudioUnitSetProperty (instance, kAudioUnitProperty_MaximumFramesPerSlice, kAudioUnitScope_Output, 0,
                          &numberOfFrames, sizeof(numberOfFrames));

    AudioUnitReset(instance, kAudioUnitScope_Global, 0);
    AudioUnitReset(instance, kAudioUnitScope_Input, 0);
    AudioUnitReset(instance, kAudioUnitScope_Output, 0);
    
    gRenderFlags = 0;
    gBusNumber   = 0;                              
    
    init_audio_time_stamp(&gTimeStamp, sampleTime / sampleRate);
    
    gBufferList  = create_buffer_list(numberOfChannels, 1, numberOfFrames);
    // TODO different number of buses ?
    
    
    if ((err = AudioUnitInitialize(instance))) {
        assert(false);
    }
}

void instance_process(AudioComponentInstance instance, double* output)
{
    OSStatus err;
    
    // Float64 sampleRate       = (Float64) 44100;
    Float64 sampleTime       = (Float64) 0;
    int     numberOfChannels = 2;
    int     numberOfFrames   = 44100; // TODO
    
    gTimeStamp.mSampleTime = sampleTime;
    
    if ((err = AudioUnitRender (instance, &gRenderFlags, &gTimeStamp, gBusNumber, numberOfFrames, gBufferList))) 
    {
        fa_print_ln(from_os_status(err));
        assert(false && "Could not render");
    }
    
    for(int channel = 0; channel < numberOfChannels; ++channel)
    {
        // TODO assert (channel < gBufferList->mNumberBuffers)
        // TODO assert (frame < gBufferList->mDataByteSize / sizeof(Float32))
        
        Float32* buffer = (Float32*) gBufferList->mBuffers[channel].mData;
    
        for(int frame = 0; frame < numberOfFrames; ++frame)
        {          
            // TODO writes interleaved
            
            // printf("Index: %d\n", channel + numberOfChannels * frame);
            // printf("Value: %f\n", buffer[frame]);
            
            output[channel + numberOfChannels * frame] = buffer[frame];
        }
    }
}

void instance_cleanup(AudioComponentInstance instance)
{
    OSStatus err;
    
    if ((err = AudioUnitUninitialize(instance))) {
        assert(false);
    }
    
    AudioUnitReset(instance, kAudioUnitScope_Global, 0);
    AudioUnitReset(instance, kAudioUnitScope_Input, 0);
    AudioUnitReset(instance, kAudioUnitScope_Output, 0);
    
    freeBufferList(gBufferList);
}



/**
    @return 
        A list of AudioComponent
*/
list_t find_audio_components(AudioComponentDescription* description)
{
    list_t  all     = list();
    int     count   = AudioComponentCount(description);

    AudioComponent component = NULL;
    
    for(int i = 0; i < count; ++i)
    {
        component = AudioComponentFindNext(component, description);
        if (component) {
            dpush_list(component, all);
        }
    }
    return all;
}

list_t find_all_audio_components()
{
    AudioComponentDescription descr;
    descr.componentType         = 0;
    descr.componentSubType      = 0;
    descr.componentManufacturer = 0;
    descr.componentFlags        = 0;
    descr.componentFlagsMask    = 0;

    return find_audio_components(&descr);
}

list_t find_dls_music_device()
{
    AudioComponentDescription descr;
    descr.componentType         = kAudioUnitType_MusicDevice;
    descr.componentSubType      = kAudioUnitSubType_DLSSynth;
    descr.componentManufacturer = kAudioUnitManufacturer_Apple;
    descr.componentFlags        = 0;
    descr.componentFlagsMask    = 0;

    return find_audio_components(&descr);
    // assert(AudioComponentCount(&desc) > 0 && "No DLSMusicDevice");
    // AudioComponent dls = AudioComponentFindNext(component, &desc);
}

ptr_t new_dls_music_device_instance()
{
    list_t components = find_dls_music_device();
    assert(fa_list_is_single(components) && "Missing or ambigous DLSMusicDevice");
    ptr_t dls = fa_list_head(components);
    ptr_t instance = NULL;
    AudioComponentInstanceNew(dls, (AudioComponentInstance*) &instance);
    return instance;
}


/*
    This program does ...

 */

void run_dsl()
{                  
    ptr_t instance = new_dls_music_device_instance();

    // Render to channels * 44100 samples
    buffer_t outb = fa_buffer_create(2*44100*sizeof(double));
    double* out = fa_buffer_unsafe_address(outb);
    
    
    instance_prepare(instance);

    for (int i = 0; i < 1; ++i) {
        instance_send_midi(instance, 0x90, 60+i*3, 90);
    }
    instance_process(instance, out);

    instance_cleanup(instance);
    
    fa_buffer_write_audio(string("test.wav"), 2, outb);
    
}

int main(int argc, char const *argv[])
{
    fa_fa_initialize();
    
    run_dsl();
    
    fa_fa_terminate();
}
