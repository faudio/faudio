
#include <fa/signal.h>
#include <fa/midi/message.h>
#include <fa/util.h>

#include <fa/action.h>
#include "../shared/signal.h"

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
    delete_fluid_settings(settings);
}

#define kFluidOffset 34
#define kFluidVectorSize 128

ptr_t render_(ptr_t x, fa_signal_state_t *state)
{
    fluid_synth_t* synth = x;

    float left[kFluidVectorSize];
    float right[kFluidVectorSize];

	// DEBUG
	if ((state->count % 22050) == 0)
		if (FLUID_FAILED == fluid_synth_noteon(synth, 1, 60, 127))
			warn(string("Could not send..."));

	
    if (!kVectorMode) {
        if (state->count % kFluidVectorSize == 0) {
            if (FLUID_OK != fluid_synth_write_float(
                synth,
                1, // frames
                left, 0, 1,
                right, 0, 1
                )) 
            {
                warn(string("Fluidsynth: Could not render"));
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
            warn(string("Fluidsynth: Could not render"));
        }

        for (int i = 0; i < kFluidVectorSize; ++i) {
            state->inputs[(kFluidOffset + 0)*kMaxVectorSize + i] = left[i];
            state->inputs[(kFluidOffset + 1)*kMaxVectorSize + i] = right[i];
        }
        return x;
    }

    return NULL;
}

void fa_midi_message_decons(fa_midi_message_t midi_message, int *statusCh, int *data1, int *data2);

ptr_t receive_(ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    fluid_synth_t* synth = x;

    assert(fa_midi_message_is_simple(msg));

    // printf("System time (early): %lld\n", fa_clock_milliseconds(fa_clock_standard()));

    // TODO
    if (true /*fa_equal(n, string("Fluid"))*/) {
        int status_channel, data1, data2;
        fa_midi_message_decons(msg, &status_channel, &data1, &data2);
        
        int channel = status_channel        & 0x0f;
        int status  = (status_channel >> 4) & 0x0f;
        
		return NULL; // DEBUG
		printf("%p %d %d %d %d\n", synth, status, channel, data1, data2);
        switch (status) {
            case 0x8: // off
                if (FLUID_OK != fluid_synth_noteoff(synth, channel, data1))
                    warn(string("Fluidsynth: Could not send message"));
                break;
            case 0x9: // 9 on
                if (FLUID_OK != fluid_synth_noteon(synth, channel, data1, data2))
                    warn(string("Fluidsynth: Could not send message"));
                break;
            case 0xa: // 10 polyp after
                warn(string("Fluidsynth: Polyphonic key pressure not supported"));
                break;
            case 0xb: // 11 control
                if (FLUID_OK != fluid_synth_cc(synth, channel, data1, data2))
                    warn(string("Fluidsynth: Could not send message"));
                break;
            case 0xc: // 12 program                
                if (FLUID_OK != fluid_synth_program_change(synth, channel, data1))
                    warn(string("Fluidsynth: Could not send message"));
                break;
            case 0xd: // 13 channel press
                warn(string("Fluidsynth: Channel pressure not supported"));
                break;
            case 0xe: // 14 pitch wheel
                warn(string("Fluidsynth: Pitch wheel not supported"));
                break;
            default: {
                warn(string("Unknown MIDI message to Fluidsynth"));
                assert(false && "Unknown MIDI message to Fluidsynth");
            }
        }
    }
    return NULL;
}

pair_t fa_signal_synth(string_t path2)
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
        warn(string("Using hardcoded soundfont location 'C:\\Users\\Doremir\\sf.sf2'"));
        char* path = "C:\\Users\\Doremir\\sf.sf2";
        //char* path = unstring(path2);

		fluid_settings_setnum(settings, "synth.gain", 1.0);
		fluid_settings_setint(settings, "synth.threadsafe-api", 0);
		fluid_settings_setint(settings, "synth.verbose", 0);
        synth = new_fluid_synth(settings);

        if(FLUID_FAILED == fluid_synth_sfload(synth, path, true)) {
            warn(string("Fluidsynth: Could not load sound font"));
			return NULL; // TODO error
        }
		fluid_synth_set_sample_rate(synth, 44100);
		
		printf("%p\n", synth);
    }

    fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
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
