
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2016
    All rights reserved.

 */

#include <fa/signal.h>
#include <fa/midi/message.h>
#include <fa/dynamic.h>
#include <fa/util.h>
#include <fa/action.h>
#include <fa/buffer.h>
#include <fa/file_buffer.h>

#include "../signal.h"

struct _play_buffer_slot {
    fa_buffer_t buffer;
    fa_file_buffer_t file_buffer;
    double pos;
    double speed;
    size_t max_pos;
    uint8_t channels;
    fa_sample_type_t sample_type;
    uint8_t sample_size;
    double sample_rate;   // The sample rate of the loaded buffer
    bool playing;
    double volume;        // Set volume
    double pan;           // 
    double left_volume;   // Volume for left channel, calculated from volume and pan
    double right_volume;  // Volume for right channel, calculated from volume and pan
};

typedef struct _play_buffer_slot play_buffer_slot;

struct _play_buffers_context {
    fa_string_t name;
    fa_string_t msg_play;
    fa_string_t msg_stop;
    fa_string_t msg_free;
    fa_string_t msg_volume;
    fa_string_t msg_pan;
    double stream_sample_rate; // cached value, the sample rate of the current stream
    int slot_count;
    play_buffer_slot *slots;
};

typedef struct _play_buffers_context play_buffers_context;

fa_ptr_t play_buffers_before_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    play_buffers_context *context = x;
    context->stream_sample_rate = state->rate;
    return x;
}
fa_ptr_t play_buffers_after_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    play_buffers_context *context = x;
    for (int s = 0; s < context->slot_count; s++) {
        play_buffer_slot *slot = &context->slots[s];
        if (slot->buffer) {
            fa_release_reference(slot->buffer);
            slot->buffer = NULL;
        }
        if (slot->file_buffer) {
            fa_release_reference(slot->file_buffer);
            slot->file_buffer = NULL;
        }
    }
    return x;
}
fa_ptr_t play_buffers_render_(fa_ptr_t x, int offset, int count, fa_signal_state_t *state)
{
    play_buffers_context *context = x;

    if (!kVectorMode) {
        
        double summed_left = 0.0;
        double summed_right = 0.0;
        
        for (int s = 0; s < context->slot_count; s++) {
            
            play_buffer_slot *slot = &context->slots[s];
            
            if (slot->playing) {
                
                if (slot->pos >= slot->max_pos) {
                    slot->playing = false;
                    slot->pos = 0;
                }
                
                else if (slot->file_buffer) {
                    // TODO: interpolating samples for non-integer positions would improve sound quality
                    size_t buffer_pos = ((size_t)slot->pos) * slot->channels;
                    double left, right;
                    switch (slot->sample_type) {
                        case float_sample_type:
                        {
                            left = (double) fa_file_buffer_get_float(slot->file_buffer, buffer_pos + 0);
                            if (slot->channels > 1) {
                                right = (double) fa_file_buffer_get_float(slot->file_buffer, buffer_pos + 1);
                            } else {
                                right = left;
                            }
                            break;
                        }
                        case double_sample_type:
                        {
                            left = fa_file_buffer_get_double(slot->file_buffer, buffer_pos + 0);
                            if (slot->channels > 1) {
                                right = fa_file_buffer_get_double(slot->file_buffer, buffer_pos + 1);
                            } else {
                                right = left;
                            }
                            break;
                        }
                    }
                    summed_left  += left * slot->left_volume;
                    summed_right += right * slot->right_volume;
                    slot->pos += slot->speed; // TODO: is double precision good enough not to get drifting errors?
                    
                    if (state->count % 11025 == 0) { // TODO: how do we know that this is often enough?
                        fa_file_buffer_hint(slot->file_buffer, buffer_pos * slot->sample_size);
                    }
                }
    
                else {
                    // TODO: interpolating samples for non-integer positions would improve sound quality
                    size_t buffer_pos = ((size_t)slot->pos) * slot->channels;
                    double left, right;
                    // Left channel
                    left = fa_buffer_get_double(slot->buffer, buffer_pos + 0);
                    // Right channel: if stereo, get the right channel sample, otherwise, copy the left channel sample
                    if (slot->channels > 1) {
                        right = fa_buffer_get_double(slot->buffer, buffer_pos + 1);
                    } else {
                        right = left;
                    }
                    summed_left  += left * slot->left_volume;
                    summed_right += right * slot->right_volume;
                    slot->pos += slot->speed; // TODO: is double precision good enough not to get drifting errors?
                }
            }   
        }
        
        state->buffer[(offset + 0)*kMaxVectorSize] = summed_left;
        state->buffer[(offset + 1)*kMaxVectorSize] = summed_right;
        
    } else {
        assert(false && "Vector mode not supported yet");
    }

    return x;
}

#define piBy4      0.78539816339
#define kSqrt2by2  0.70710678118

static void update_slot_volume(play_buffer_slot *slot)
{
    double angle = slot->pan * piBy4;
    slot->left_volume = slot->volume * kSqrt2by2 * (cos(angle) - sin(angle));
    slot->right_volume = slot->volume * kSqrt2by2 * (cos(angle) + sin(angle));
}

fa_ptr_t play_buffers_receive_(fa_ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    play_buffers_context *context = x;

    if (fa_equal(n, context->name)) {
        
        // fa_slog_info("play_buffers_receive_ ", msg);

        // Accept single commands, and make slot default to 0
        int slot_num = 0;
        fa_ptr_t cmd = msg;
        
        // If msg is a pair, assume it is a slot number and a command
        if (fa_dynamic_get_type(msg) == pair_type_repr) {
            slot_num = fa_peek_integer(fa_pair_first(msg));
            cmd = fa_pair_second(msg);
        }
        
        if (slot_num >= 0 && slot_num < context->slot_count) {
            play_buffer_slot *slot = &context->slots[slot_num];
            
            fa_dynamic_type_repr_t cmd_type = fa_dynamic_get_type(cmd);
            switch (cmd_type) {
                case buffer_type_repr:
                {
                    slot->playing = false;
                    slot->pos = 0;
                    fa_buffer_t buffer = cmd;
                    if (buffer != slot->buffer) {
                        fa_take_reference(buffer);
                        if (slot->buffer) {
                            fa_release_reference(slot->buffer);
                        }
                        if (slot->file_buffer) {
                            fa_release_reference(slot->file_buffer);
                        }
                        slot->buffer = buffer;
                        slot->file_buffer = NULL;
                        slot->channels = fa_peek_number(fa_get_meta(buffer, fa_string("channels")));
                        size_t size = fa_buffer_size(buffer);
                        slot->max_pos = (size / ((sizeof(double)) * slot->channels)) - 1;
                        double buffer_rate = fa_peek_number(fa_get_meta(buffer, fa_string("sample-rate")));
                        slot->speed = buffer_rate / context->stream_sample_rate;
                        slot->sample_rate = buffer_rate;
                    }
                    break;
                }
                
                case file_buffer_type_repr:
                {
                    slot->playing = false;
                    slot->pos = 0;
                    fa_file_buffer_t buffer = cmd;
                    if (buffer != slot->file_buffer) {
                        fa_take_reference(buffer);
                        if (slot->file_buffer) {
                            fa_release_reference(slot->file_buffer);
                        }
                        if (slot->buffer) {
                            fa_release_reference(slot->buffer);
                        }
                        slot->file_buffer = buffer;
                        slot->buffer = NULL;
                        slot->channels = fa_peek_number(fa_get_meta(buffer, fa_string("channels")));
                        slot->max_pos = fa_peek_integer(fa_get_meta(buffer, fa_string("frames")));
                        double buffer_rate = fa_peek_number(fa_get_meta(buffer, fa_string("sample-rate")));
                        slot->speed = buffer_rate / context->stream_sample_rate;
                        slot->sample_type = fa_peek_integer(fa_get_meta(buffer, fa_string("sample-type")));
                        slot->sample_size = fa_sample_type_size(slot->sample_type);
                        slot->sample_rate = buffer_rate;
                    }
                    break;
                }
                
                // An integer: a new position in frames
                case i8_type_repr:
                case i16_type_repr:
                case i32_type_repr:
                case i64_type_repr:
                {
                    double new_pos = fa_peek_number(cmd);
                    if (new_pos > slot->max_pos) new_pos = slot->max_pos;
                    slot->pos = new_pos;
                    if (slot->file_buffer) {
                        fa_file_buffer_hint(slot->file_buffer, slot->pos * slot->channels * slot->sample_size);
                    }
                    // // For testing only, can't do I/O in this thread
                    // if (slot->file_buffer) {
                    //     uint8_t frame_size = slot->channels * slot->sample_size;
                    //     fa_file_buffer_seek_if_needed(slot->file_buffer, slot->pos * frame_size);
                    // }
                    break;
                }
                // A floating point number: a new position in seconds
                case f32_type_repr:
                case f64_type_repr:
                {
                    double new_pos = fa_peek_number(cmd) * slot->sample_rate;
                    if (new_pos > slot->max_pos) new_pos = slot->max_pos;
                    slot->pos = new_pos;
                    if (slot->file_buffer) {
                        fa_file_buffer_hint(slot->file_buffer, slot->pos * slot->channels * slot->sample_size);
                    }
                    break;
                }
                
                case string_type_repr:
                {
                    if (fa_equal(cmd, context->msg_play)) {
                        slot->playing = true;
                    }
                    else if (fa_equal(cmd, context->msg_stop)) {
                        slot->playing = false;
                    }
                    else if (fa_equal(cmd, context->msg_free)) {
                        if (slot->buffer) {
                            fa_buffer_t buffer = slot->buffer;
                            slot->playing = false;
                            slot->pos = 0;
                            slot->buffer = NULL;
                            fa_release_reference(buffer);
                        }
                        if (slot->file_buffer) {
                            fa_file_buffer_t buffer = slot->file_buffer;
                            slot->playing = false;
                            slot->pos = 0;
                            slot->file_buffer = NULL;
                            fa_release_reference(buffer);
                        }
                    }
                    break;
                }
                
                case pair_type_repr:
                {
                    fa_ptr_t setting = fa_pair_first(cmd);
                    fa_ptr_t value   = fa_pair_second(cmd);
                    if (fa_equal(setting, context->msg_volume)) {
                        double volume = fa_peek_number(value);
                        if (volume < 0) volume = 0;
                        if (volume > 64) volume = 64; // arbitrary max value
                        slot->volume = volume;
                        update_slot_volume(slot);
                    }
                    else if (fa_equal(setting, context->msg_pan)) {
                        double pan = fa_peek_number(value);
                        if (pan < -1.0) pan = -1.0;
                        if (pan > 1.0) pan = 1.0;
                        slot->pan = pan;
                        update_slot_volume(slot);
                    }
                    
                    break;
                }
    
                default:
                break;
            }
        }
    }
    
    return x;
}

fa_ptr_t play_buffers_destroy_(fa_ptr_t x)
{
    play_buffers_context *context = x;
    fa_destroy(context->name);
    fa_destroy(context->msg_play);
    fa_destroy(context->msg_stop);
    fa_destroy(context->msg_free);
    fa_destroy(context->msg_volume);
    fa_destroy(context->msg_pan);
    fa_free(context->slots);
    fa_free(context);
    return NULL;
}

fa_pair_t fa_signal_play_buffers(fa_string_t name, int count)
{
    assert(count > 0 && count <= 32);
    play_buffers_context *context = fa_malloc(sizeof(play_buffers_context));
    context->name = fa_copy(name);
    context->stream_sample_rate = 0;
    
    // Cache messages, so that we don't have to allocate in the receive function
    context->msg_play    = fa_string("play");
    context->msg_stop    = fa_string("stop");
    context->msg_free    = fa_string("free");
    context->msg_volume  = fa_string("volume");
    context->msg_pan     = fa_string("pan");
    
    // Init slots
    context->slots = fa_malloc(count * sizeof(play_buffer_slot));
    context->slot_count = count;
    for (int s = 0; s < count; s++) {
        play_buffer_slot *slot = &context->slots[s];
        slot->buffer = NULL;
        slot->file_buffer = NULL;
        slot->pos = 0;
        slot->max_pos = 0;
        slot->playing = false;
        slot->speed = 1.0;
        slot->sample_type = double_sample_type;
        slot->sample_size = fa_sample_type_size(double_sample_type);
        slot->sample_rate = 44100; // just some default
        slot->volume = 1.0;
        slot->pan = 0.0;
        slot->left_volume = 1.0;
        slot->right_volume = 1.0;
    }

    fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
    proc->before  = play_buffers_before_;
    proc->after   = play_buffers_after_;
    proc->render  = play_buffers_render_;
    proc->receive = play_buffers_receive_;
    proc->send    = NULL;
    proc->destroy = play_buffers_destroy_;
    proc->data    = context;

    fa_signal_t left  = fa_signal_input_with_custom(proc, 0);
    fa_signal_t right = fa_signal_input_with_custom(proc, 1);
    fa_signal_t left2 = fa_signal_custom(proc, left);
    fa_pair_t result = fa_pair_create(left2, right);
    return result;
}

fa_pair_t fa_signal_play_buffer(fa_string_t name)
{
    return fa_signal_play_buffers(name, 1);
}
