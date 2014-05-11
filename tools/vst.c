
#include <fa/fa.h>
#include <fa/midi/message.h>
#define NO_THREAD_T
#include <fa/util.h>
#undef NO_THREAD_T
#include "../platform/macosx/vst.h"
#include "../shared/signal.h"
#include "common.h"
#include <Carbon/Carbon.h>
 
/*
    This program implements and plays a trivial custom processor, which generates a click
    sound for each received note. Scheduling is copied from test_dls.
 */

#define RT 1
#define kThisPlugOffset 37 // TODO
#define PATH string("/Library/Audio/Plug-Ins/VST/ComboV.vst")
// #define PATH string("/Library/Audio/Plug-Ins/VST/TAL-U-No-62.vst")
// #define PATH string("/Library/Audio/Plug-Ins/VST/Melodyne.vst")
// #define PATH string("/Library/Audio/Plug-Ins/VST/Kontakt 5.vst")
;

// struct _vst_context {
//     string_t name;
//     AEffect* plugin;
//     float** inputs;
//     float** outputs;    
// };
// typedef struct _vst_context vst_context;
// 
// // TODO place in struct somewhere
// // static float** inputs = NULL;
// // static float** outputs = NULL;    
// 
// ptr_t vst_before_(ptr_t x, int count, fa_signal_state_t *state)
// {
//     vst_context* context = x;
//     AEffect*     plugin = context->plugin;
// 
//     resumePlugin(plugin);
// 
//     {
//         printf(">>>>>>>> Inputs:  %d\n", plugin->numInputs);
//         printf(">>>>>>>> Outputs: %d\n", plugin->numOutputs);
//     }
//     return x;
// }
// ptr_t vst_after_(ptr_t x, int count, fa_signal_state_t *state)
// {
//     vst_context* context = x;
//     AEffect*     plugin = context->plugin;
// 
//     suspendPlugin(plugin);
//     return x;
// }
// 
// ptr_t vst_render_(ptr_t x, int count, fa_signal_state_t *state)
// {
//     vst_context* context = x;
//     AEffect*     plugin = context->plugin;
// 
//     assert(count == 64); // TODO (also change VST loader)
//     
//     if (kVectorMode) {
//         fail(string("Vector mode not supported!"));
//         exit(-1);
//     } else {
//         // fail(string("Non-Vector mode not supported!"));
// 
//         assert(context->inputs && context->outputs);                 
//         silenceChannel(context->inputs, plugin->numInputs, count);
//         silenceChannel(context->outputs, plugin->numOutputs, count);
//         
//         // TODO inputs
//         
//         processAudio(plugin, context->inputs, context->outputs, 1);
//         
//         for (int channel = 0; channel < plugin->numOutputs; ++channel) {                                  
//             for (int sample = 0; sample < 1; ++sample) {                                  
//                 if (channel < 2) {
//                     state->buffer[(kThisPlugOffset + channel)*kMaxVectorSize + sample] = context->outputs[channel][sample];
//                 }
//             }
//         }
//     }
// 
//     return x;
// }
// 
// 
// 
// void fa_midi_message_decons(fa_midi_message_t midi_message, int *statusCh, int *data1, int *data2);
// 
// ptr_t vst_receive_(ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
// {
//     vst_context* context = x;
//     AEffect*     plugin = context->plugin;
// 
//     if (fa_equal(n, context->name)) {
//         if (!fa_midi_message_is_simple(msg)) {
//             warn(string("Unknown message to DLS"));
//         } else {
//             int status, data1, data2;
//             fa_midi_message_decons(msg, &status, &data1, &data2);
// 
//             VstMidiEvent event;
//             event.type = kVstMidiType;
//             event.byteSize = sizeof (VstMidiEvent);
//             event.deltaFrames = 0;
//             event.flags = 0;
//             event.noteLength = 100;
//             event.noteOffset = 0;
//             // event.midiData[0] = 0x90;
//             event.midiData[0] = status;
//             event.midiData[1] = data1;
//             event.midiData[2] = data2;
//             event.midiData[3] = 0;
// 
//             event.detune = 0;
//             event.noteOffVelocity = 0;
//     
//             VstEvents events;
//             events.numEvents = 1;
//             events.events[0] = (VstEvent*) &event;
//     
//             processMidi(plugin, &events);
// 
//         }
//     }
//     return x;
// }
// 
// list_t fa_signal_vs(string_t name, string_t path, list_t inputs)
// {
//     char* rpath = unstring(path);
//     AEffect* plugin = loadPlugin(rpath);
//     initPlugin(plugin);
//     
//     // TODO
//     assert(canPluginDo(plugin, "receiveVstMidiEvent"));
// 
//     // {
//     //     WindowRef theWindow;
//     //     Rect contentRect; 
//     //     contentRect.top = 200;
//     //     contentRect.left = 300;
//     //     contentRect.bottom = 400;
//     //     contentRect.right = 500;
//     //     
//     //     // SetRect(&contentRect, 100,100,100,100);
//     //     OSStatus err;
//     //      
//     //     err=CreateNewWindow(kDocumentWindowClass,kWindowStandardDocumentAttributes,&contentRect, &theWindow);
//     //     if(err!=noErr)printf("Error in CreateNewWindow\n");
//     //     ShowWindow(theWindow);
//     // 
//     //     WindowRef refWindow = theWindow;    
//     //     openPlugin(plugin, refWindow);
//     // }   
//     
// 
//     vst_context* context = fa_malloc(sizeof(vst_context));
//     context->plugin = plugin;
//     context->name = name;
//     context->inputs = malloc(sizeof(ptr_t) * plugin->numInputs);
//     context->outputs = malloc(sizeof(ptr_t) * plugin->numOutputs);
// 
//     for (int i = 0; i < plugin->numInputs; ++i) {
//         context->inputs[i] = malloc(sizeof(float) * kMaxVectorSize);
//     }
//     for (int i = 0; i < plugin->numOutputs; ++i) {
//         context->outputs[i] = malloc(sizeof(float) * kMaxVectorSize);
//     }
// 
//     fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
//     proc->before  = vst_before_;
//     proc->after   = vst_after_;
//     proc->render  = vst_render_;
//     proc->receive = vst_receive_;
//     proc->send    = NULL;
//     proc->destroy = NULL; // TODO
//     proc->data    = context;
// 
//     return list(fa_signal_custom(proc, fa_signal_input(kThisPlugOffset + 0)), fa_signal_input(kThisPlugOffset + 1));
// }

void run_vst()
{
    if (RT) {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);
        list_t out           = fa_signal_vst(string("vst-test"), PATH, empty());

        fa_audio_set_parameter(string("vector-size"), i32(128), s);
        fa_audio_stream_t st = fa_audio_open_stream(i, o, just, out);

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        }

        for (int i = 0; i < 24; ++i) {           
            fa_action_t chord = fa_action_send(string("vst-test"), fa_midi_message_create_simple(0x90, 52 + ((i % 12) * 5), 90));
            fa_audio_schedule_relative(hms(0, 0, 0), chord, st);
            fa_thread_sleep(150);
        }
        // fa_thread_sleep(5000);
    }
}

int main(int argc, char const *argv[])
{
    fa_set_log_tool();
    fa_with_faudio() {
        run_vst();
    }
}
