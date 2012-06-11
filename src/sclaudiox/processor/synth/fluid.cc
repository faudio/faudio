/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
/**
    @file   sclaudiox/processor/synth/fluid.cc
    @author Hans Hoglund
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
    fluid_synth_t*         synth;
};  


// ============================================================================


FluidSynthError::FluidSynthError(String message) 
    : specificMessage(message) {}

FluidSynthError::FluidSynthError(FluidSynthData* data)
{
    specificMessage = fluid_synth_error(data->synth);
}

String FluidSynthError::message()
{
    return "FluidSynth error: " + specificMessage;
}  


// ============================================================================

namespace
{
    inline void fluid_check_ref(void* ref, String msg)
    {
        if (!ref) throw FluidSynthError(msg);
    }    
    
    inline void fluid_check(int status, String msg)
    {
        if (status == FLUID_FAILED) throw FluidSynthError(msg);
    }
    
    void fluid_write_log(int level, char* message, void* data)
    {
        SCL_WRITE_LOG("   FluidSynth: " << message << "\n");
    }       
    
    void init_fluid_settings(FluidSynthData* data)
    {     
        data->description = new FluidSynthDescription;

        data->settings = new_fluid_settings();
        fluid_check_ref(data->settings, "Could not create settings");

        fluid_settings_setnum(data->settings, "synth.gain", 0.45);
        fluid_settings_setint(data->settings, "synth.threadsafe-api", false);
    }   
    
    void init_fluid_synth(FluidSynthData* data, String path)
    {
        int err;
        data->synth = new_fluid_synth(data->settings);
        fluid_check_ref(data->synth, "Could not create synth");

        err = fluid_synth_sfload(data->synth, toSimpleString(path), true);
        fluid_check(err, "Could not load soundfont from '" + path + "'");

        fluid_synth_set_interp_method(data->synth, -1, FLUID_INTERP_7THORDER);
        fluid_synth_set_reverb_on(data->synth, 1);
        fluid_synth_set_reverb(data->synth, 0.58, 0.1, 0.05, 0.3);
    }
}


FluidSynth::FluidSynth(FilePath path)
    : mData(new FluidSynthData)
{
    int err;
#ifdef SCL_FLUID_LOG
    fluid_set_log_function(FLUID_WARN, fluid_write_log, NULL);
#endif    
    init_fluid_settings(mData);
    init_fluid_synth(mData, path);
}

FluidSynth::~FluidSynth()
{
    delete_fluid_synth(mData->synth);
    delete_fluid_settings(mData->settings);
    delete mData->description;
    delete mData;
}

AudioProcessorDescription* FluidSynth::description()
{
    return mData->description;
}       

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
            fluid_synth_noteoff(mData->synth, channel, data1);            
            err = FLUID_OK;
            break;

        case kMidiNoteOn:
            fluid_synth_noteon(mData->synth, channel, data1, data2);
            err = FLUID_OK;
            break;

        case kMidiAfterTouch:
            err = FLUID_OK;
            break;

        case kMidiControlChange:
            err = fluid_synth_cc(mData->synth, channel, data1, data2);
            break;

        case kMidiProgramChange:
            err = fluid_synth_program_change(mData->synth, channel, data1);
            break;

        case kMidiChannelPressure:
            err = fluid_synth_channel_pressure(mData->synth, channel, data1);
            break;

        case kMidiPitchWheel:
            err = fluid_synth_pitch_bend(mData->synth, channel, (data2 << 7) + data1);
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
    fluid_synth_set_sample_rate(mData->synth, info.sampleRate);
}

void FluidSynth::process(AudioProcessingInformation& info, 
                         AudioProcessingBuffer &signal)
{
    int err;
    {
        int numberOfFrames = signal.numberOfFrames;
    	err = fluid_synth_write_float (mData->synth, numberOfFrames,
                                       signal.data, 0, 1, signal.data, numberOfFrames, 1);
    }
    if (err == FLUID_FAILED)
    {
        if (strlen(fluid_synth_error(mData->synth)) > 0)
            throw FluidSynthError(mData);
        else
            throw FluidSynthError("Could not synthesize audio block");
    }
}

void FluidSynth::cleanup(AudioProcessingInformation& info, 
                         AudioProcessingBuffer &signal) {}



} // namespace
} // namespace
