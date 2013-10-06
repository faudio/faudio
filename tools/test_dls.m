
#include <fa/fa.h>

#define NO_THREAD_T // TODO
#include <fa/util.h>
#undef NO_THREAD_T

#include <CoreAudio/CoreAudio.h>
#include <CoreAudioKit/AUGenericView.h>
#include <AudioUnit/AudioUnit.h>
#include <AudioUnit/AUCocoaUIView.h>


typedef struct au_context *au_context_t;

au_context_t create_au_context(ptr_t instance);
void destroy_au_context(au_context_t context);
ptr_t new_dls_music_device_instance();
void au_prepare(au_context_t context);
void au_send_midi(au_context_t context, int status, int data1, int data2);
void au_render(au_context_t context, double* output);
void au_cleanup(au_context_t context);

void run_dsl()
{                  
    au_context_t context = create_au_context(new_dls_music_device_instance());
    
    // Render to channels * 44100 samples
    buffer_t outb = fa_buffer_create(2*44100*sizeof(double));
    double* out = fa_buffer_unsafe_address(outb);
    
    
    au_prepare(context);
    for (int i = 0; i < 1; ++i) {
        au_send_midi(context, 0x90, 60+i*3, 90);
    }
    au_render(context, out);
    au_cleanup(context);
    
    fa_buffer_write_audio(string("test.wav"), 1, outb);

    destroy_au_context(context);
}

int main(int argc, char const *argv[])
{
    fa_fa_initialize();
    
    run_dsl();
    
    fa_fa_terminate();
}
