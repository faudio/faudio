
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
    // FIXME OK?
    return string((char*) GetMacOSStatusErrorString(err));
}

string_t component_name(AudioComponent component)
{
    CFStringRef cf_str;
    AudioComponentCopyName(component, &cf_str);
    return fa_string_from_native((void*) cf_str);
    
    // return string("z");
}

inline static 
void instance_num_channels(AudioComponentInstance instance, int* numIns, int* numOuts)
{
    *numIns = 0;
    *numOuts = 0;

    AUChannelInfo supportedChannels [128];
    UInt32 supportedChannelsSize = sizeof (supportedChannels);

    if (AudioUnitGetProperty (instance, kAudioUnitProperty_SupportedNumChannels, kAudioUnitScope_Global,
                              0, supportedChannels, &supportedChannelsSize) == noErr
               &&
        supportedChannelsSize > 0)
    {
        for (int i = 0; i < supportedChannelsSize / sizeof (AUChannelInfo); ++i)
        {                     
            *numIns = MAX(*numIns, (int) supportedChannels[i].inChannels);
            *numOuts = MAX(*numOuts, (int) supportedChannels[i].outChannels);
        }
    }
    else
    {     
        // TODO why is this default?
        *numIns = 1;
        *numOuts = 1;
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

void FillOutAudioTimeStampWithSampleTime2 (
   AudioTimeStamp *outATS,
   Float64 inSampleTime
) {
   outATS->mSampleTime = inSampleTime;
   outATS->mHostTime = 0;
   outATS->mRateScalar = 0;
   outATS->mWordClockTime = 0;
   memset (&outATS->mSMPTETime, 0, sizeof (SMPTETime));
   outATS->mFlags = kAudioTimeStampSampleTimeValid;
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
    
    // set latency ?
    
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
    
    memset(&gTimeStamp, 0, sizeof(AudioTimeStamp));
    FillOutAudioTimeStampWithSampleTime2(&gTimeStamp, sampleTime / sampleRate);

    // FillOutAudioTimeStampWithSampleTime2(&gTimeStamp, sampleTime / sampleRate);
    // // gTimeStamp.mHostTime   = AudioGetCurrentHostTime();
    // gTimeStamp.mFlags      = kAudioTimeStampSampleTimeValid | kAudioTimeStampHostTimeValid;
    // gTimeStamp.mFlags      = 0;
    
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




/*
    This program does ...

 */

void run_dsl()
{                  
    fa_print_ln(string("This is test_dls!"));
    
    list_t components = find_dls_music_device();
    printf("%d\n", fa_list_length(components));
    ptr_t dls = fa_list_head(components);
    printf("%s\n", unstring(component_name(dls)));
    
    // mark_used(dls);
    ptr_t instance = NULL;
    AudioComponentInstanceNew(dls, (AudioComponentInstance*) &instance);

    printf("%d\n%d\n", instance_num_inputs(instance), instance_num_outputs(instance));


    // Render to channels * 44100 samples
    
    buffer_t outb = fa_buffer_create(2*44100*sizeof(double));
    double* out = fa_buffer_unsafe_address(outb);
    // double* out = fa_malloc(2*44100*sizeof(double));

    instance_prepare(instance);

    for (int i = 0; i < 30; ++i)
        instance_send_midi(instance, 0x90, 48+i*3, 90);

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
