
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

pair_t fa_signal_synth(string_t path)
{
    assert(false && "Not available on this platform");
}

ptr_t before_(ptr_t x, int count, fa_signal_state_t *state)
{
    au_context_t context = x;
    au_prepare(context, state->rate); // Set SR
    return x;
}

ptr_t after_(ptr_t x, int count, fa_signal_state_t *state)
{
    au_context_t context = x;
    au_cleanup(context);
    return x;
}

struct au_context {
    double *outputs;
};

#define kAUOffset 32


ptr_t render_(ptr_t x, int count, fa_signal_state_t *state)
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

ptr_t receive_(ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    au_context_t context = x;
    inform(string("DLS, comparing names: "));
    inform(n);
    inform(string("dls"));

    if (fa_equal(n, string("dls"))) {
        assert(fa_midi_message_is_simple(msg));

        int status, data1, data2;
        fa_midi_message_decons(msg, &status, &data1, &data2);
        au_send_midi(context, status, data1, data2);
    } else {
        warn(string("Unknown message to DLS"));
        // no assert!
    }

    return x;
    mark_used(context);
}

// Pair of signals
pair_t fa_signal_dls()
{
    au_context_t context = create_au_context(new_dls_music_device_instance(), 2, kMaxVectorSize, 0); // Update SR later
    // TODO destroy

    fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
    proc->before  = before_;
    proc->after   = after_;
    proc->render  = render_;
    proc->receive = receive_;
    proc->data    = context;

    signal_t left  = fa_signal_input(kAUOffset + 0);
    signal_t right = fa_signal_input(kAUOffset + 1);
    signal_t left2 = fa_signal_custom(proc, left);
    return pair(left2, right);
    mark_used(left2);
}
