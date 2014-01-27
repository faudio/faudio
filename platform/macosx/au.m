
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/fa.h>

#define NO_THREAD_T
#include <fa/util.h>
#undef NO_THREAD_T

#include <CoreAudio/CoreAudio.h>
#include <CoreAudioKit/AUGenericView.h>
#include <AudioUnit/AudioUnit.h>
#include <AudioUnit/AUCocoaUIView.h>

#include "au.h"

struct au_context {                     
    double *outputs;
    int channels;
    int frames;
    double sample_rate;
    
    AudioComponentInstance      Instance;
    AudioUnitRenderActionFlags  RenderFlags;
    int                         BusNumber;
    AudioTimeStamp              TimeStamp;
    AudioBufferList*            BufferList;
};

au_context_t create_au_context(ptr_t instance, int channels, int frames, double sample_rate)
{
    au_context_t context = fa_new_struct(au_context);
    context->Instance = instance;
    context->channels = channels;
    context->frames = frames;
    context->sample_rate = sample_rate;
    context->outputs = fa_malloc(sizeof(double)*channels*frames);
    return context;
}
void destroy_au_context(au_context_t context) {
    fa_free(context->outputs);
    fa_delete(context);
}


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


void init_audio_time_stamp (AudioTimeStamp *time_stamp, Float64 inSampleTime) {

   time_stamp->mSampleTime          = inSampleTime;

   time_stamp->mHostTime            = 0;
   time_stamp->mRateScalar          = 0;
   time_stamp->mWordClockTime       = 0;

   memset (&time_stamp->mSMPTETime, 0, sizeof (SMPTETime));

   time_stamp->mFlags               = kAudioTimeStampSampleTimeValid;
}

AudioBufferList* new_buffer_list(int numBuffers, int numChannels, int numFrames)
{                   
    // Size of each mData                                         
    size_t bufferSize = (sizeof(Float32) * numChannels * numFrames); 
    
    // Size of the whole thing
    size_t size       = sizeof(UInt32) + (sizeof(AudioBuffer)) * numBuffers;
    
    AudioBufferList* bufferList = fa_malloc(size);
    bufferList->mNumberBuffers = numBuffers;

    for(int i = 0; i < numBuffers; ++i)
    {
        Float32 * buffer = fa_malloc(bufferSize);

        bufferList->mBuffers[i].mNumberChannels = numChannels;
        bufferList->mBuffers[i].mDataByteSize   = bufferSize;
        bufferList->mBuffers[i].mData           = buffer;
    }
    return bufferList;
}

void delete_buffer_list(AudioBufferList* list)
{
    for (int i = 0; i < list->mNumberBuffers; ++i)
        fa_free(list->mBuffers[i].mData);
    fa_free(list);
}

void au_prepare(au_context_t context, double sample_rate)
{
    AudioComponentInstance instance = context->Instance;

    OSStatus err;

    Float64 sampleRate       = (Float64) sample_rate; // TODO
    Float64 sampleTime       = (Float64) 0;
    UInt32  numberOfChannels = (UInt32)  context->channels;
    UInt32  numberOfFrames   = (UInt32)  context->frames;

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
    
    context->RenderFlags = 0;
    context->BusNumber   = 0;                              
    
    init_audio_time_stamp(&context->TimeStamp, sampleTime / sampleRate);
    
    // 1 buffer per channel, 1 'channel' per buffer
    context->BufferList  = new_buffer_list(numberOfChannels, 1, numberOfFrames);

    // TODO different number of buses ?
    
    
    if ((err = AudioUnitInitialize(instance))) {
        assert(false);
    }
}

void au_render(au_context_t context, double time, double* output)
{
    AudioComponentInstance instance = context->Instance;
    OSStatus err;
    
    Float64 sampleTime       = (Float64) time;
    UInt32  numberOfChannels = (UInt32)  context->channels;
    UInt32  numberOfFrames   = (UInt32)  context->frames;
    
    context->TimeStamp.mSampleTime = sampleTime;
    
    if ((err = AudioUnitRender (instance, &context->RenderFlags, &context->TimeStamp, context->BusNumber, numberOfFrames, context->BufferList))) 
    {
        fa_print_ln(from_os_status(err));
        assert(false && "Could not render");
    }
    
    for(int channel = 0; channel < numberOfChannels; ++channel)
    {
        Float32* buffer = (Float32*) context->BufferList->mBuffers[channel].mData;
    
        for(int frame = 0; frame < numberOfFrames; ++frame)
        {          
            double x = buffer[frame]; // sample for current (channel,frame)

            // Non-interleaved output
            context->outputs[numberOfFrames*channel + frame] = x;
        }
    }
}

void au_cleanup(au_context_t context)
{
    AudioComponentInstance instance = context->Instance;
    OSStatus err;
    
    if ((err = AudioUnitUninitialize(instance))) {
        assert(false);
    }
    
    AudioUnitReset(instance, kAudioUnitScope_Global, 0);
    AudioUnitReset(instance, kAudioUnitScope_Input, 0);
    AudioUnitReset(instance, kAudioUnitScope_Output, 0);
    
    delete_buffer_list(context->BufferList);
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
            fa_dpush_list(component, all);
        }
    }
    return all;
}

/**
    @return 
        A list of AudioComponent
*/
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


void au_send_midi(au_context_t context, int status, int data1, int data2)
{
    OSStatus err;
    AudioComponentInstance instance = context->Instance;
    
    if ((err = MusicDeviceMIDIEvent(instance, status, data1, data2, 0))) {
        fa_print_ln(from_os_status(err));
        assert(false && "Could not send note");
    }
}
