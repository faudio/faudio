
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/clock.h>
#include <fa/signal.h>
#include <fa/action.h>
#include <fa/midi/message.h>
#include <fa/util.h>

#include "au.h"
#include "../shared/signal.h"

fa_pair_t fa_signal_synth(fa_string_t path)
{
    assert(false && "Not available on this platform");
}

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
};

fa_ptr_t render_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    au_context_t context = x;

    if (!kVectorMode) {
        int freq = 64;

        if (state->count % freq == 0) {
            au_render(context, state->count, freq, NULL);
        }

        state->buffer[(kAUOffset + 0)*kMaxVectorSize] = context->outputs[freq * 0 + (state->count % freq)];
        state->buffer[(kAUOffset + 1)*kMaxVectorSize] = context->outputs[freq * 1 + (state->count % freq)];

        return x;
    } else {
        au_render(context, state->count, count, NULL);

        for (int i = 0; i < count; ++i) {
            state->buffer[(kAUOffset + 0)*kMaxVectorSize + i] = context->outputs[count * 0 + i];
            state->buffer[(kAUOffset + 1)*kMaxVectorSize + i] = context->outputs[count * 1 + i];
        }

        return x;
    }
}

fa_ptr_t receive_(fa_ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    au_context_t context = x;

    if (fa_equal(n, fa_string("dls"))) {
        if (!fa_midi_message_is_simple(msg)) {
            fa_warn(fa_string("Unknown message to DLS"));
        } else {
            int status, data1, data2;
            fa_midi_message_decons(msg, &status, &data1, &data2);
            au_send_midi(context, status, data1, data2);
        }
    }

    return x;
}

fa_ptr_t destroy_(fa_ptr_t x)
{
    fa_inform(fa_string("Destroying DSLSynth instance (doing nothing)"));
    return x;
}

// Pair of signals
fa_pair_t fa_signal_dls()
{
    au_context_t context = create_au_context(new_dls_music_device_instance(), 2, kMaxVectorSize, 0); // Update SR later
    // TODO destroy

    fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
    proc->before  = before_;
    proc->after   = after_;
    proc->render  = render_;
    proc->receive = receive_;
    proc->send    = NULL;
    proc->destroy = destroy_;
    proc->data    = context;

    fa_signal_t left  = fa_signal_input_with_custom(proc, kAUOffset + 0);
    fa_signal_t right = fa_signal_input_with_custom(proc, kAUOffset + 1);
    fa_signal_t left2 = fa_signal_custom(proc, left);
    return fa_pair_create(left2, right);
    fa_mark_used(left2);
}
