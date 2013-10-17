
// DLSMusicDevice wrapper
// TODO clean

typedef struct au_context *au_context_t;

au_context_t create_au_context(ptr_t instance, int channels, int frames);
void destroy_au_context(au_context_t context);
ptr_t new_dls_music_device_instance();
void au_prepare(au_context_t context);
void au_send_midi(au_context_t context, int status, int data1, int data2);
void au_render(au_context_t context, double time, double *output);
void au_cleanup(au_context_t context);

void fa_midi_message_decons(fa_midi_message_t midi_message, int *statusCh, int *data1, int *data2);
