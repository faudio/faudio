
#include <fa/fa.h>
#include <fa/util.h>
#include "../platform/macosx/vst.h"
#include "../shared/signal.h"
#include "common.h"

/*
    This program implements and plays a trivial custom processor, which generates a click
    sound for each received note. Scheduling is copied from test_dls.
 */


#define RT 1
#define kThisPlugOffset 37 // TODO
#define PATH string("/Library/Audio/Plug-Ins/VST/Kontakt 5.vst")

static bool should_click = false;


ptr_t before_(ptr_t x, int count, fa_signal_state_t *state)
{
    return x;
}
ptr_t after_(ptr_t x, int count, fa_signal_state_t *state)
{
    return x;
}
ptr_t render_(ptr_t x, int count, fa_signal_state_t *state)
{
    if (!kVectorMode) {
        fail(string("Vector mode not supported!"));
        exit(-1);
    } else {
        fail(string("Non-Vector mode not supported!"));
        exit(-1);
        for (int i = 0; i < count; ++i) {
            state->buffer[(kThisPlugOffset + 0)*kMaxVectorSize + i] = should_click;
            state->buffer[(kThisPlugOffset + 1)*kMaxVectorSize + i] = should_click;
            should_click = false;
        }
    }

    return x;
}

ptr_t receive_(ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    // TODO
    return x;
}

pair_t fa_signal_vs(string_t path)
{
    char* rpath = unstring(path);
    AEffect* plugin = loadPlugin(rpath);
    initPlugin(plugin);
    resumePlugin(plugin);

    fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
    proc->before  = before_;
    proc->after   = after_;
    proc->render  = render_;
    proc->receive = receive_;
    proc->send    = NULL;
    proc->destroy = NULL; // TODO
    proc->data    = plugin;

    return pair(fa_signal_custom(proc, fa_signal_input(kThisPlugOffset + 0)), fa_signal_input(kThisPlugOffset + 1));
}

void run_vs()
{
    if (RT) {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);
        list_t out           = fa_pair_to_list(fa_signal_vs(PATH));

        fa_audio_stream_t st = fa_audio_open_stream(i, o, just, out);

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        }
        fa_thread_sleep(1000);
    }
}

int main(int argc, char const *argv[])
{
    fa_set_log_tool();
    fa_with_faudio() {
        run_vs();
    }
}
