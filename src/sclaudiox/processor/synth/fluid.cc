/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
/**
    \file    fluid.cc
    \authors Hans Hoglund <hans.hoglund@doremir.com>
 */

#include "fluidsynth.h"

#include "sclaudiox/control.h"
#include "sclaudiox/util/concurrency.h"
#include "sclaudiox/processor/synth/fluid.h"

#ifdef SCL_LOG
#define SCL_FLUID_LOG
#endif // SCL_LOG

namespace doremir {
namespace scl {

struct FluidSynthData
{
    FluidSynthDescription* description;
    fluid_settings_t*      settings;
    fluid_synth_t*         instance;
};  

FluidSynthError::FluidSynthError(String message) 
    : specificMessage(message) {}

FluidSynthError::FluidSynthError(FluidSynthData* data)
{
    specificMessage = fluid_synth_error(data->instance);
}

String FluidSynthError::message() const
{
    return "FluidSynth error: " + specificMessage;
}

FluidSynth::FluidSynth(FilePath path)
    : mData(new FluidSynthData)
{
    int err;

#ifdef SCL_FLUID_LOG
    fluid_set_log_function(FLUID_WARN, handleLogMessage, NULL);
#endif

    mData->description = new FluidSynthDescription;

    mData->settings = new_fluid_settings();
    if (mData->settings == NULL)
    {
        throw FluidSynthError("Could not create settings");
    }

    fluid_settings_setnum(mData->settings, "synth.gain", 0.45);
    fluid_settings_setint(mData->settings, "synth.threadsafe-api", false);

    mData->instance = new_fluid_synth(mData->settings);
    if (mData->instance == NULL)
    {
        throw FluidSynthError("Could not create synth");
    }

    err = fluid_synth_sfload(mData->instance, toSimpleString(path), true);
    if (err == FLUID_FAILED)
    {
        throw FluidSynthError("Could not load soundfont from '" + path + "'");
    }
    
    fluid_synth_set_interp_method(mData->instance, -1, FLUID_INTERP_7THORDER);
    fluid_synth_set_reverb_on(mData->instance, 1);
    fluid_synth_set_reverb(mData->instance, 0.58, 0.1, 0.05, 0.3);
}

FluidSynth::~FluidSynth()
{
    delete_fluid_synth(mData->instance);
    delete_fluid_settings(mData->settings);
    delete mData->description;
    delete mData;
}

AudioProcessorDescription* FluidSynth::description() const
{
    return mData->description;
}       

#define kMidiNoteOff         0x80
#define kMidiNoteOn          0x90
#define kMidiAfterTouch      0xA0
#define kMidiControlChange   0xB0
#define kMidiProgramChange   0xC0
#define kMidiChannelPressure 0xD0
#define kMidiPitchWheel      0xE0
#define kMidiSysEx           0xF0


void FluidSynth::accept(Message message)
{
    int err;

    int status, data1, data2;
    messageTo(message, status, data1, data2);

    int action, channel;
    action  = status & 0xf0;
    channel = status & 0x0f;

    switch (action)
    {
        /*
            Ignore error for on and off, as we want to 
            tolerate non-matching on and off pairs 
        */
        case kMidiNoteOff:
            fluid_synth_noteoff(mData->instance, channel, data1);            
            err = FLUID_OK;
            break;

        case kMidiNoteOn:
            fluid_synth_noteon(mData->instance, channel, data1, data2);
            err = FLUID_OK;
            break;

        case kMidiAfterTouch:
            err = FLUID_OK;
            break;

        case kMidiControlChange:
            err = fluid_synth_cc(mData->instance, channel, data1, data2);
            break;

        case kMidiProgramChange:
            err = fluid_synth_program_change(mData->instance, channel, data1);
            break;

        case kMidiChannelPressure:
            err = fluid_synth_channel_pressure(mData->instance, channel, data1);
            break;

        case kMidiPitchWheel:
            err = fluid_synth_pitch_bend(mData->instance, channel, (data2 << 7) + data1);
            break;
            
        case kMidiSysEx:
            err = FLUID_OK;
            break;            

        default:
            err = FLUID_OK;
            break;
    }
    if (err == FLUID_FAILED)
    {
        throw FluidSynthError(mData);
    }
}

void FluidSynth::prepare(AudioProcessingInformation& info, 
                         AudioProcessingBuffer& signal)
{
    fluid_synth_set_sample_rate(mData->instance, info.sampleRate);
}

void FluidSynth::process(AudioProcessingInformation& info, 
                         AudioProcessingBuffer &signal)
{
    int err;
    {
        int numberOfFrames = signal.numberOfFrames;
    	err = fluid_synth_write_float(mData->instance,
    	                              numberOfFrames,
                                      signal.data, 0, 1,
                                      signal.data, numberOfFrames, 1);
    }

    if (err == FLUID_FAILED)
    {
        if (strlen(fluid_synth_error(mData->instance)) > 0)
            throw FluidSynthError(mData);
        else
            throw FluidSynthError("Could not synthesize audio block");
    }
}

void FluidSynth::cleanup(AudioProcessingInformation& info, 
                         AudioProcessingBuffer &signal) {}


void FluidSynth::handleLogMessage(int level, char* message, void* data)
{
    SCL_WRITE_LOG("   FluidSynth: " << message << "\n");
}

} // namespace
} // namespace
