
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2016
    All rights reserved.

 */

#include <fa/clock.h>
#include <fa/signal.h>
#include <fa/action.h>
#include <fa/midi/message.h>
#include <fa/util.h>
#include <fa/dynamic.h>
#include <pthread.h>

#include "au.h"
#include "../shared/signal.h"

fa_ptr_t before_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    au_context_t context = x;
    au_prepare(context, state->rate); // Set SR
    return x;
}

fa_ptr_t after_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    au_context_t context = x;
    au_cleanup(context);
    return x;
}

struct au_context {
    double *outputs;
    fa_string_t name;
};

fa_ptr_t render_(fa_ptr_t x, int offset, int count, fa_signal_state_t *state)
{
    au_context_t context = x;

    if (!kVectorMode) {
        int freq = 64;

        if (state->count % freq == 0) {
            au_render(context, state->count, freq, NULL);
        }
        
        // if (state->count % 44100 == 0) {
        //     printf("render_ (DLS) %p\n", pthread_self());
        // }

        state->buffer[(offset + 0)*kMaxVectorSize] = context->outputs[freq * 0 + (state->count % freq)];
        state->buffer[(offset + 1)*kMaxVectorSize] = context->outputs[freq * 1 + (state->count % freq)];

        return x;
    } else {
        au_render(context, state->count, count, NULL);

        for (int i = 0; i < count; ++i) {
            state->buffer[(offset + 0)*kMaxVectorSize + i] = context->outputs[count * 0 + i];
            state->buffer[(offset + 1)*kMaxVectorSize + i] = context->outputs[count * 1 + i];
        }

        return x;
    }
}

fa_ptr_t receive_(fa_ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    au_context_t context = x;
    
    // If we get a number, treat it as a frequency to use as master tuning
    if (fa_is_number(msg)) {
        au_set_master_tuning(context, fa_peek_number(msg));
        return x;
    }

    // Unknown message
    if (fa_dynamic_get_type(msg) != midi_message_type_repr) {
        return x;
    }

    if (fa_equal(n, context->name)) {
        if (!fa_midi_message_is_simple(msg)) {
            fa_warn(fa_string("SYSEX message to DLS, ignoring"));
        } else {
            uint8_t status, data1, data2, data3;
            fa_midi_message_ex_decons(msg, &status, &data1, &data2, &data3);
            uint8_t cmd = status & 0xF0;
            uint8_t ch  = status & 0x0F;
            if ((cmd == 0x80 || cmd == 0x90) && (ch != 9) && (data3 != 0)) {
                // NoteOn and NoteOff with cents != 0
                if ((cmd == 0x90) && (data2 > 0)) {
                    au_send_note_start(context, status, data1, data2, data3);
                } else {
                    au_send_note_stop(context, status, data1, data2, data3);
                }
            } else if ((cmd == 0xB0) && (data1 == 0x7B)) {
                // All Notes Off
                au_send_all_notes_off(context, ch);
            } else {
                // Normal case
                au_send_midi(context, status, data1, data2);
            }
        }
    }

    return x;
}

fa_ptr_t destroy_(fa_ptr_t x)
{
    return x;
}

fa_pair_t fa_signal_dls(fa_string_t name)
{
    au_context_t context = create_au_context(new_dls_music_device_instance(), 2, kMaxVectorSize, 0); // Update SR later
    context->name = fa_copy(name);

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
    fa_signal_t left2 = fa_signal_custom(proc, left);
    return fa_pair_create(left2, right);
}
