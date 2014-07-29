
#include <fa/signal.h>
#include <fa/midi/message.h>
#include <fa/util.h>

#include <fa/action.h>
#include "../shared/signal.h"

#include <fluidsynth.h>

fa_pair_t fa_signal_dls()
{
    assert(false && "Not available on this platform");
}


fa_ptr_t before_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    fluid_synth_t *synth = x;
    fluid_synth_set_sample_rate(synth, state->rate);
    return NULL;
}

fa_ptr_t after_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    // Nothing
}

// TODO not reentrant
static float left[kMaxVectorSize];
static float right[kMaxVectorSize];


fa_ptr_t render_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    fluid_synth_t *synth = x;

    float *left2 = left;
    float *right2 = right;

    if (!kVectorMode) {
        if (FLUID_OK != fluid_synth_nwrite_float(
                    synth,
                    1,
                    &left2,
                    &right2,
                    NULL,
                    NULL
                )) {
            fa_warn(fa_string("Fluidsynth: Could not render"));
        }

        state->buffer[(kFluidOffset + 0)*kMaxVectorSize] = left[0];
        state->buffer[(kFluidOffset + 1)*kMaxVectorSize] = right[0];
        return x;
    } else {
        if (FLUID_OK != fluid_synth_nwrite_float(
                    synth,
                    count, // TODO
                    &left2,
                    &right2,
                    NULL,
                    NULL
                )) {
            fa_warn(fa_string("Fluidsynth: Could not render"));
        }

        for (int i = 0; i < count; ++i) {
            state->buffer[(kFluidOffset + 0)*kMaxVectorSize + i] = left[i];
            state->buffer[(kFluidOffset + 1)*kMaxVectorSize + i] = right[i];
        }

        return x;
    }

    return NULL;
}

void fa_midi_message_decons(fa_midi_message_t midi_message, int *statusCh, int *data1, int *data2);

fa_ptr_t receive_(fa_ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    fluid_synth_t *synth = x;


    // printf("System time (early): %lld\n", fa_clock_milliseconds(fa_clock_standard()));

    // TODO
    if (fa_equal(n, fa_string("fluid"))) {
        if (!fa_midi_message_is_simple(msg)) {
            fa_warn(fa_string("Unknown message to Fluidsynth (not a MIDI message)"));
        } else {


            int status_channel, data1, data2;
            fa_midi_message_decons(msg, &status_channel, &data1, &data2);

            int channel = status_channel        & 0x0f;
            int status  = (status_channel >> 4) & 0x0f;

            // printf("%p %d %d %d %d\n", synth, status, channel, data1, data2);
            switch (status) {
            case 0x8: // off
                if (FLUID_OK != fluid_synth_noteoff(synth, channel, data1)) {
                    fa_warn(fa_string("Fluidsynth: Could not send message"));
                }

                break;

            case 0x9: // 9 on
                if (FLUID_OK != fluid_synth_noteon(synth, channel, data1, data2)) {
                    fa_warn(fa_string("Fluidsynth: Could not send message"));
                }

                break;

            case 0xa: // 10 polyp after
                fa_warn(fa_string("Fluidsynth: Polyphonic key pressure not supported"));
                break;

            case 0xb: // 11 control
                if (FLUID_OK != fluid_synth_cc(synth, channel, data1, data2)) {
                    fa_warn(fa_string("Fluidsynth: Could not send message"));
                }

                break;

            case 0xc: // 12 program
                if (FLUID_OK != fluid_synth_program_change(synth, channel, data1)) {
                    fa_warn(fa_string("Fluidsynth: Could not send message"));
                }

                break;

            case 0xd: // 13 channel press
                if (FLUID_OK != fluid_synth_channel_pressure(synth, channel, data1)) {
                    fa_warn(fa_string("Fluidsynth: Could not send message"));
                }

                break;

            case 0xe: { // 14 pitch wheel
                // TODO do we need to to call pitch_wheel_sens etc?
                unsigned short bend;
                bend = (unsigned short)data2;
                bend <<= 7;
                bend |= (unsigned short)data1;

                if (FLUID_OK != fluid_synth_pitch_bend(synth, channel, bend)) {
                    fa_warn(fa_string("Fluidsynth: Could not send message"));
                }

                break;
            }

            default: {
                fa_warn(fa_string_dappend(fa_string("Unknown MIDI message to Fluidsynth: <status="), fa_string_format_integral("%d>", status)));
                // assert(false && "Unknown MIDI message to Fluidsynth");
            }
            }

        }
    }

    return NULL;
}

fa_ptr_t destroy_(fa_ptr_t x)
{
    fa_inform(fa_string("Destroying FluidSynth instance"));
    fluid_synth_t *synth = x;
    fluid_settings_t *settings = fluid_synth_get_settings(synth);

    delete_fluid_synth(synth);
    delete_fluid_settings(settings);
}

fa_pair_t fa_signal_synth(fa_string_t path2)
{
    // create synth
    fluid_synth_t *synth = NULL;
    {
        fluid_settings_t *settings = new_fluid_settings();

        // fluid_settings_setstr(settings, "name", value);
        // fluid_settings_setsint(settings, "name", value);
        // fluid_settings_setsnum(settings, "name", value);

        fluid_settings_setnum(settings, "synth.gain", 0.6);
        fluid_settings_setint(settings, "synth.threadsafe-api", 0);
        fluid_settings_setint(settings, "synth.verbose", 0);

        fa_inform(fa_string("Creating FluidSynth instance"));
        synth = new_fluid_synth(settings);

        fa_inform(fa_string_dappend(fa_string("    Loading sound font"), fa_copy(path2)));
        char *path = fa_unstring(path2);

        if (FLUID_FAILED == fluid_synth_sfload(synth, path, true)) {
            fa_warn(fa_string("Fluidsynth: Could not load sound font"));
            return NULL; // TODO error
        }

        // Sample rate is set later

        printf("%p\n", synth);
    }

    fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
    proc->before  = before_;
    proc->after   = after_;
    proc->render  = render_;
    proc->receive = receive_;
    proc->send    = NULL;
    proc->destroy = destroy_;
    proc->data    = synth;

    fa_signal_t left  = fa_signal_input(kFluidOffset + 0);
    fa_signal_t right = fa_signal_input(kFluidOffset + 1);

    // Return stereo output, embedding the custom proc (so it is actually run)
    return fa_pair_create(fa_signal_custom(proc, left), right);
}
