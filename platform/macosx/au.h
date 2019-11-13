
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2016
    All rights reserved.

 */

typedef struct au_context *au_context_t;

au_context_t create_au_context(fa_ptr_t instance, int channels, int frames, double sample_rate);
void destroy_au_context(au_context_t context);
fa_ptr_t new_dls_music_device_instance();
void au_prepare(au_context_t context, double sample_rate);
void au_send_midi(au_context_t context, int status, int data1, int data2);
void au_send_note_start(au_context_t context, int status, int data1, int data2, int data3);
void au_send_note_stop(au_context_t context, int status, int data1, int data2, int data3);
void au_send_all_notes_off(au_context_t context, int channel);
void au_set_master_tuning(au_context_t context, double pitch);
void au_render(au_context_t context, double time, int count, double *output);
void au_cleanup(au_context_t context);

void fa_midi_message_decons(fa_midi_message_t midi_message, uint8_t *statusCh, uint8_t *data1, uint8_t *data2);
void fa_midi_message_ex_decons(fa_midi_message_t midi_message, uint8_t *statusCh, uint8_t *data1, uint8_t *data2, uint8_t *data3);