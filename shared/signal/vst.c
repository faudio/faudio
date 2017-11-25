
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2016
    All rights reserved.

 */

#include <fa/signal.h>
#include <fa/midi/message.h>
#include <fa/dynamic.h>
#include <fa/util.h>

#include "../signal.h"

#ifndef _WIN32
#include "../platform/macosx/vst.h"
#endif


#ifndef _WIN32

struct _vst_context {
    fa_string_t name;
    AEffect *plugin;
    float **inputs;
    float **outputs;
};

typedef struct _vst_context vst_context;

// TODO remove
static vst_context *last_vst_plug = NULL;


fa_ptr_t vst_before_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    vst_context *context = x;
    AEffect     *plugin = context->plugin;

    setPluginParams(plugin, state->rate, kMaxVectorSize);
    resumePlugin(plugin);

    return x;
}
fa_ptr_t vst_after_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    vst_context *context = x;
    AEffect     *plugin = context->plugin;

    suspendPlugin(plugin);
    return x;
}

fa_ptr_t vst_render_(fa_ptr_t x, int offset, int count, fa_signal_state_t *state)
{
    vst_context *context = x;
    AEffect     *plugin = context->plugin;

    // assert(count == 64); // TODO (also change VST loader)

    if (kVectorMode) {
        fa_fail(fa_string("Vector mode not supported!"));
        exit(-1);
    } else {
        // fa_fail(fa_string("Non-Vector mode not supported!"));

        assert(context->inputs && context->outputs);
        silenceChannel(context->inputs, plugin->numInputs, count);
        silenceChannel(context->outputs, plugin->numOutputs, count);

        // TODO inputs

        processAudio(plugin, context->inputs, context->outputs, 1);

        for (int channel = 0; channel < plugin->numOutputs; ++channel) {
            for (int sample = 0; sample < 1; ++sample) {
                if (channel < 2) {
                    state->buffer[(offset + channel)*kMaxVectorSize + sample] = context->outputs[channel][sample];
                }
            }
        }
    }

    return x;
}


// TODO move
#define fa_dynamic_is_fa_string_from_utf8(x) (fa_dynamic_get_type(x) == string_type_repr)
#define fa_dynamic_is_bool(x)   (fa_dynamic_get_type(x) == bool_type_repr)
#define fa_dynamic_is_pair(x)   (fa_dynamic_get_type(x) == pair_type_repr)
#define fa_dynamic_is_list(x)   (fa_dynamic_get_type(x) == list_type_repr)

// Used by vst.cc
void vst_log(const char *msg)
{
    // fa_warn(fa_string_dappend(fa_string("Error in VST: "), fa_string_from_utf8((char*) msg)));
}

void vst_log_i(const char *fmt, long n)
{
    // fa_warn(fa_string_dappend(fa_string("Error in VST: "),
    // fa_string_format_integral((char*) fmt, n)
    // ));
}

fa_ptr_t vst_receive_(fa_ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    vst_context *context = x;
    AEffect     *plugin = context->plugin;

    // fa_warn(n);
    // fa_warn(context->name);
    // fa_warn(fa_equal(n, context->name) ? fa_string("t") : fa_string("f"));
    // fa_warn(fa_string(""));

    if (fa_equal(n, context->name)) {

        if (fa_dynamic_is_pair(msg) && fa_dynamic_is_fa_string_from_utf8(fa_pair_first(msg)) && fa_equal(fa_pair_first(msg), fa_string("open"))) {
            fa_warn(fa_string("Opening VST window..."));

            if (fa_pair_second(msg)) {
                openPlugin(plugin, fa_pair_second(msg));
            } else {
                fa_warn(fa_string("Asked to open VST window but handle is NULL, ignoring."));
            }

            return x;
        }

        // Assume this is MIDI message
        // TODO add MIDI messags to dynamic and check this!
        if (!fa_midi_message_is_simple(msg)) {
            fa_warn(fa_string("Unknown message to VST plug"));
            return x;
        } else {
            uint8_t status, data1, data2;
            fa_midi_message_decons(msg, &status, &data1, &data2);

            VstMidiEvent event;
            event.type = kVstMidiType;
            event.byteSize = sizeof(VstMidiEvent);
            event.deltaFrames = 0;
            event.flags = 0;
            event.noteLength = 100;
            event.noteOffset = 0;
            // event.midiData[0] = 0x90;
            event.midiData[0] = status;
            event.midiData[1] = data1;
            event.midiData[2] = data2;
            event.midiData[3] = 0;

            event.detune = 0;
            event.noteOffVelocity = 0;

            VstEvents events;
            events.numEvents = 1;
            events.events[0] = (VstEvent *) &event;

            processMidi(plugin, &events);
            return x;
        }

        assert(false && "Unreachable");
    }

    return x;
}

fa_list_t fa_signal_vst(fa_string_t name1, fa_string_t path1, fa_list_t inputs)
{
    // TODO
    fa_string_t name = fa_copy(name1);
    fa_string_t path = fa_copy(path1);

    char *rpath = fa_unstring(path);
    AEffect *plugin = loadPlugin(rpath);
    if (!plugin) {
        fa_warn(concat(fa_string("Could not load plugin "), path));
    }
    initPlugin(plugin);

    // TODO
    assert(canPluginDo(plugin, "receiveVstMidiEvent"));

    // {
    //     WindowRef theWindow;
    //     Rect contentRect;
    //     contentRect.top = 200;
    //     contentRect.left = 300;
    //     contentRect.bottom = 400;
    //     contentRect.right = 500;
    //
    //     // SetRect(&contentRect, 100,100,100,100);
    //     OSStatus err;
    //
    //     err=CreateNewWindow(kDocumentWindowClass,kWindowStandardDocumentAttributes,&contentRect, &theWindow);
    //     if(err!=noErr)printf("Error in CreateNewWindow\n");
    //     ShowWindow(theWindow);
    //
    //     WindowRef refWindow = theWindow;
    //     openPlugin(plugin, refWindow);
    // }


    vst_context *context = fa_malloc(sizeof(vst_context));
    context->plugin = plugin;
    context->name = name;
    fa_warn(fa_string_append(fa_string("Creating VST plugin\n    Name: "), name));
    fa_warn(fa_string_append(fa_string("    Path: "), path));

    context->inputs = malloc(sizeof(fa_ptr_t) * plugin->numInputs);
    context->outputs = malloc(sizeof(fa_ptr_t) * plugin->numOutputs);

    for (int i = 0; i < plugin->numInputs; ++i) {
        context->inputs[i] = malloc(sizeof(float) * kMaxVectorSize);
    }

    for (int i = 0; i < plugin->numOutputs; ++i) {
        context->outputs[i] = malloc(sizeof(float) * kMaxVectorSize);
    }

    fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
    proc->before  = vst_before_;
    proc->after   = vst_after_;
    proc->render  = vst_render_;
    proc->receive = vst_receive_;
    proc->send    = NULL;
    proc->destroy = NULL; // TODO
    proc->data    = context;

    // TODO
    last_vst_plug = context;

    return list(fa_signal_custom(proc,
                                 fa_signal_input_with_custom(proc, 0)),
                fa_signal_input_with_custom(proc, 1));
}

void fa_signal_show_vst_gui(fa_string_t string, void *handle)
{
    // TODO find correct plugin
    openPlugin(last_vst_plug->plugin, handle);
}
#else // _WIN32
fa_list_t fa_signal_vst(fa_string_t name1, fa_string_t path1, fa_list_t inputs)
{
    fa_fail(fa_string("VST not supported for this platform yet."));
}
#endif // _WIN32

