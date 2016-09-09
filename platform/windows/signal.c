
#include <fa/signal.h>
#include <fa/midi/message.h>
#include <fa/util.h>

#include <fa/action.h>
#include "../shared/signal.h"

#include <fluidsynth.h>

static uint8_t tuning[16][128];

typedef struct fluid_context {
    fluid_synth_t *synth;
    fa_string_t name;
    bool owned;
} fluid_context;

fa_pair_t fa_signal_dls(fa_string_t name)
{
    assert(false && "Not available on this platform");
}


fa_ptr_t before_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    fluid_context *context = x;
    fluid_synth_t *synth = context->synth;
    fluid_synth_set_sample_rate(synth, state->rate);
    for (int ch = 0; ch < 16; ch++) {
        fluid_synth_select_tuning(synth, ch, 0, ch);
        for (int key = 0; key < 128; key++) tuning[ch][key] = 0;
    }
    return NULL;
}

fa_ptr_t after_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    // Nothing
}

// TODO not reentrant
static float left[kMaxVectorSize];
static float right[kMaxVectorSize];


fa_ptr_t render_(fa_ptr_t x, int offset, int count, fa_signal_state_t *state)
{
    fluid_context *context = x;
    fluid_synth_t *synth = context->synth;

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

        state->buffer[(offset + 0)*kMaxVectorSize] = left[0];
        state->buffer[(offset + 1)*kMaxVectorSize] = right[0];
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
            state->buffer[(offset + 0)*kMaxVectorSize + i] = left[i];
            state->buffer[(offset + 1)*kMaxVectorSize + i] = right[i];
        }

        return x;
    }

    return NULL;
}

fa_ptr_t receive_(fa_ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    fluid_context *context = x;
    fluid_synth_t *synth = context->synth;

    // NOTE: this function is called in the audio thread, which means that
    // must work fast. In particular, it should not allocate memory, use locks,
    // or access the file system or other I/O.

    // Unfortunately, logging involves both allocating memory, interacting
    // with the file system and/or pipes. On Mac, this doesn't seem to be
    // a problem in practice, but on Windows, a few log messages is
    // sufficient to cause audio glitches.

    // Therefore, we should only warn for situations that are not supposed to
    // happen at all. For instance, calling fluid_synth_noteoff for certain
    // percussion sounds results in FLUID_FAILED being returned, as those
    // sounds don't have the concept of noteoff. This is not an error,
    // so we don't log it.

    if (fa_equal(n, context->name)) {

        if (!fa_midi_message_is_simple(msg)) {
			fa_warn(fa_string("SYSEX message to Fluidsynth"));
			//if (FLUID_OK != fluid_synth_sysex(synth, msg->sysex->data, msg->sysex->size, NULL, 0, NULL, 0)) {
			//	fa_warn(fa_string("Fluidsynth: Could not send SYSEX message"));
			//}
			
        } else {


            uint8_t status_channel, data1, data2, data3;
            fa_midi_message_ex_decons(msg, &status_channel, &data1, &data2, &data3);

            int channel = status_channel        & 0x0f;
            int status  = (status_channel >> 4) & 0x0f;

            // printf("%p %d %d %d %d\n", synth, status, channel, data1, data2);
            switch (status) {
            case 0x8: // off
                if (FLUID_OK != fluid_synth_noteoff(synth, channel, data1)) {
                    if (channel == 9) {
                        // Don't warn for noteoff on the percussion channel
                    } else {
                        fa_slog_warning("Fluidsynth: Could not send noteoff message ", msg);
                    }
                }

                break;

            case 0x9: { // 9 on
                // Retune if needed
                if (data2 != 0 && data3 != tuning[channel][data1]) {
                    double pitch[1] = {data1 * 100.0 + data3};
                    int key[1] = {data1};
                    fluid_synth_tune_notes(synth, 0, channel, 1, key, pitch, false);
                    tuning[channel][data1] = data3;
                }
                // Send noteon
                if (FLUID_OK != fluid_synth_noteon(synth, channel, data1, data2)) {
                    if ((channel == 9) && (data2 == 0)) {
                        // Don't warn for noteoff on the percussion channel
                    } else {
                        fa_slog_warning("Fluidsynth: Could not send noteon message ", msg);
                    }
                }

                break;
            }

            case 0xa: // 10 polyp after
                fa_warn(fa_string("Fluidsynth: Polyphonic key pressure not supported"));
                break;

            case 0xb: // 11 control
                if (FLUID_OK != fluid_synth_cc(synth, channel, data1, data2)) {
                    fa_slog_warning("Fluidsynth: Could not send control message ", msg);
                }

                break;

            case 0xc: // 12 program
                if (FLUID_OK != fluid_synth_program_change(synth, channel, data1)) {
                    fa_warn(fa_string_format_integral("Fluidsynth: Could not send program change message %u", data1));
                }

                break;

            case 0xd: // 13 channel press
                if (FLUID_OK != fluid_synth_channel_pressure(synth, channel, data1)) {
                    fa_warn(fa_string("Fluidsynth: Could not send channel pressure message"));
                }

                break;

            case 0xe: { // 14 pitch wheel
                // TODO do we need to to call pitch_wheel_sens etc?
                unsigned short bend;
                bend = (unsigned short)data2;
                bend <<= 7;
                bend |= (unsigned short)data1;

                if (FLUID_OK != fluid_synth_pitch_bend(synth, channel, bend)) {
                    fa_warn(fa_string("Fluidsynth: Could not send pitch bend message"));
                }

                break;
            }

            default: {
                if ((status_channel == 0xff) && (data1 == 0x2f)) {
                    // EndOfTrack, ignore
                } else if (status_channel == 0xF8 || status_channel == 0xFB || status_channel == 0xFC) {
                    // MIDI clock message, ignore
                } else if (status_channel == 0xFE) {
                    // "Active sensing" message (i.e a sort of idle message), ignore
                } else {
                    fa_slog_warning("Unknown MIDI message to Fluidsynth: ", msg);
                }
            }
        }

        }
    }

    return NULL;
}

fa_ptr_t destroy_(fa_ptr_t x)
{
    fluid_context *context = x;
    if (context->owned) {
        fa_inform(fa_string("        Destroying FluidSynth instance"));
        fluid_synth_t *synth = context->synth;
        fluid_settings_t *settings = fluid_synth_get_settings(synth);
        delete_fluid_synth(synth);
        delete_fluid_settings(settings);
    }
    if (context->name) fa_destroy(context->name);
    fa_free(context);
}



static fa_pair_t signal_synth(fa_string_t name, fluid_synth_t *synth, bool owned)
{
    fluid_context *context = fa_new_struct(fluid_context);
    context->synth = synth;
    context->name  = fa_copy(name);
    context->owned = owned;

    fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
    proc->before  = before_;
    proc->after   = after_;
    proc->render  = render_;
    proc->receive = receive_;
    proc->send    = NULL;
    proc->destroy = destroy_;
    proc->data    = context;

    fa_signal_t left  = fa_signal_input_with_custom(proc, 0);
    fa_signal_t right = fa_signal_input_with_custom(proc, 1);

    // Return stereo output, embedding the custom proc (so it is actually run)
    return fa_pair_create(fa_signal_custom(proc, left), right);
}

fa_pair_t fa_signal_fluid(fa_string_t name, fluid_synth_t *synth)
{
    return signal_synth(name, synth, false);
}

fa_pair_t fa_signal_synth(fa_string_t name, fa_string_t path2)
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

        fa_inform(fa_string_dappend(fa_string("    Loading sound font "), fa_copy(path2)));
        char *path = fa_unstring(path2);

        if (FLUID_FAILED == fluid_synth_sfload(synth, path, true)) {
            fa_warn(fa_string("Fluidsynth: Could not load sound font"));
            return NULL; // TODO error
        }

        // Sample rate is set later
    }
    return signal_synth(name, synth, true);
}
