
#include <fa/signal.h>
#include <fa/util.h>
#include <fluidsynth.h>

pair_t fa_signal_dls()
{
    assert(false && "Not available on this platform");
}


ptr_t before_(ptr_t x, fa_signal_state_t *state)
{
    // Nothing
    return NULL;
}

ptr_t after_(ptr_t x, fa_signal_state_t *state)
{
    fluid_synth_t* synth = x; 
    fluid_settings_t* settings = fluid_synth_get_settings(synth);
    
    delete_fluid_synth(synth);
    delete_fluid_settings(synth);
}

#define kFluidOffset 34
#define kFluidVectorSize 128

ptr_t render_(ptr_t x, fa_signal_state_t *state)
{
    fluid_synth_t* synth = x;

    float left[kFluidVectorSize];
    float right[kFluidVectorSize];

    if (!kVectorMode) {
        if (state->count % kFluidVectorSize == 0) {
            if (FLUID_OK != fluid_synth_write_float(
                synth,
                1, // frames
                left, 0, 1,
                right, 0, 1
                )) 
            {
                warn("Fluidsynth: Could not render");
            }
        }
        state->inputs[(kFluidOffset + 0)*kMaxVectorSize] = left[0];
        state->inputs[(kFluidOffset + 1)*kMaxVectorSize] = right[0];
        return x;
    } else {
        if (FLUID_OK != fluid_synth_write_float(
            synth,
            kFluidVectorSize,
            left, 0, 1,
            right, 0, 1
            )) 
        {
            warn("Fluidsynth: Could not render");
        }

        for (int i = 0; i < kFluidVectorSize; ++i) {
            state->inputs[(kFluidOffset + 0)*kMaxVectorSize + i] = left[i];
            state->inputs[(kFluidOffset + 1)*kMaxVectorSize + i] = right[i];
        }
        return x;
    }

    return NULL;
}

ptr_t receive_(ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    fluid_synth_t* synth = x;

    assert(fa_midi_message_is_simple(msg));

    // printf("System time (early): %lld\n", fa_clock_milliseconds(fa_clock_standard()));

    // TODO
    if (true /*fa_equal(n, string("Fluid"))*/) {
        int status, data1, data2;
        fa_midi_message_decons(msg, &status_channel, &data1, &data2);
        
        int status  = status_channel        & 0x0f;
        int channel = (status_channel >> 4) & 0x0f;
        
        switch (status) {
            case 0x8: // off
                if (FLUID_OK != fluid_send_noteoff(synth, channel, data1, data2))
                    warn("Fluidsynth: Could not send message");
                break;
            case 0x9: // 9 on
                if (FLUID_OK != fluid_send_noten(synth, channel, data1, data2))
                    warn("Fluidsynth: Could not send message");
                break;
            case 0xa: // 10 polyp after
                warn("Fluidsynth: Polyphonic key pressure not supported");
                break;
            case 0xb: // 11 control
                if (FLUID_OK != fluid_synth_cc(synth, channel, data1, data2))
                    warn("Fluidsynth: Could not send message");
                break;
            case 0xc: // 12 program                
                if (FLUID_OK != fluid_synth_program_change(synth, channel, data1))
                    warn("Fluidsynth: Could not send message");
                break;
            case 0xd: // 13 channel press
                warn("Fluidsynth: Channel pressure not supported");
                break;
            case 0xe: // 14 pitch wheel
                warn("Fluidsynth: Pitch wheel not supported");
                break;
            default: {
                warn(false && "Unknown MIDI message to Fluidsynth");
                assert(false && "Unknown MIDI message to Fluidsynth");
            }
        }
    }
    return NULL;
}

pair_t fa_signal_fluidsynth(string_t path2)
{
    // create synth
    fluid_synth_t* synth = NULL;
    {
        fluid_settings_t* settings = new_fluid_settings();

        // fluid_settings_setstr(settings, "name", value);
        // fluid_settings_setsint(settings, "name", value);
        // fluid_settings_setsnum(settings, "name", value);

        // TODO configure
        // TODO hardcoded        
        // warn("Using hardcoded soundfont location '/c/sf.sf2'");
        // char* path = "/c/sf.sf2";
        path = unstring(path2);

        if(FLUID_OK != fluid_synth_sfload(synth, path, true)) {
            warn("Fluidsynth: Could not load sound font");
        }

        synth = new_fluid_synth(settings);
    }

    proc->before  = before_;
    proc->after   = after_;
    proc->render  = render_;
    proc->receive = receive_;
    proc->data    = synth;

    signal_t left  = fa_signal_input(kFluidOffset + 0);
    signal_t right = fa_signal_input(kFluidOffset + 1);

    // Return stereo output, embedding the custom proc (so it is actually run)
    return pair(fa_signal_custom(proc, left), right);
}
