#include "fa/fa.h"
#include "fa/alloc.h"

#ifndef __SERVER_SEQUENCE
#define __SERVER_SEQUENCE

#include "server-utils.h"
#include "server-types.h"
#include "server-globals.h"
#include "fa/signal.h"

#include <string.h>

double auto_stop_margin = 0.010; // 10 ms
double export_margin = 0.5; // 0.5 s


// Note: sends nominal (scheduled) time!
fa_ptr_t _playback_started(fa_ptr_t context, fa_time_t time, fa_time_t now)
{
    send_osc_async("/playback/started", "it", peek_oid(context), timetag_from_time(time));
    return NULL;
}

fa_ptr_t _playback_stopped(fa_ptr_t context, fa_time_t time, fa_time_t now)
{
    oid_t id = peek_oid(context);
    if (verbose) printf("_playback_stopped (%hu)\n", id);
    if (remove_playback_semaphore(id)) {
        send_osc_async("/playback/stopped", "itT", id, timetag_from_time(now));
        // printf("calling stop_time_echo from _playback_stopped (%hu)\n", id);
        stop_time_echo();
        //if (current_midi_playback_stream) {
        //    do_schedule_relative(sched_delay, all_notes_off_action(), current_midi_playback_stream);
        //}
    }
    return NULL;
}

typedef struct _sequence_t * sequence_t;
struct _sequence_t {
    oid_t id;
    sequence_status_t status;
    bool buffered;
    fa_list_t midi_actions;     // list of pairs
    fa_list_t audio_actions;    // list of pairs
    fa_list_t audio_buffers;    // references to added audio buffers (pairs of (buffer, slot))
    double start_time;          // actual start time (set when started)
    float max_time;             // maximum relative time; i.e. the "combined duration" of the contained events
    double repeat;              // repeat interval (set when started)
    bool auto_stop;             // stop automatically after last event is played (i.e. at start_time + max_time)
    sequence_t master;     //
    fa_list_t slaves;           //
} _sequence_t;

static inline sequence_t new_sequence(oid_t id, sequence_status_t status, bool buffered) {
    sequence_t result = fa_malloc(sizeof(struct _sequence_t));
    result->id = id;
    result->status = status;
    result->buffered = buffered;
    result->midi_actions = fa_list_empty();
    result->audio_actions = fa_list_empty();
    result->audio_buffers = fa_list_empty();
    result->start_time = 0;
    result->max_time = 0;
    result->auto_stop = true;
    result->master = NULL;
    result->slaves = fa_list_empty();
    return result;
}

static inline void delete_sequence(sequence_t sequence) {
    fa_destroy(sequence->midi_actions);
    fa_destroy(sequence->audio_actions);
    fa_for_each(buffer_slot, sequence->audio_buffers) {
        fa_buffer_t buffer = fa_pair_first((fa_pair_t)buffer_slot);
        fa_release_reference(buffer);
        fa_destroy(fa_pair_second((fa_pair_t)buffer_slot));
        fa_destroy(buffer_slot);
    }
    fa_destroy(sequence->audio_buffers);
    fa_destroy(sequence->slaves);
    sequence->midi_actions = NULL;
    sequence->audio_actions = NULL;
    fa_free(sequence);
}

static void flush_sequence_actions(sequence_t sequence, double time);

fa_ptr_t _sequence_started(fa_ptr_t context, fa_time_t time, fa_time_t now)
{
    sequence_t sequence = context;
    oid_t id;
    bool started = false;
    fa_with_lock(sequences_mutex) {
        if (sequence->status == SEQUENCE_STOPPING) {
            delete_sequence(sequence);
            printf("In _sequence_started and status was SEQUENCE_STOPPING\n");
        } else {
            sequence->status = SEQUENCE_RUNNING;
            sequence->start_time = fa_time_to_double(time);
            if (verbose) printf("_sequence_started  %f\n", sequence->start_time);
            id = sequence->id;
            flush_sequence_actions(sequence, sequence->start_time);
            started = true;
        }
    }
    // fa_for_each(slave, sequence->slaves) {
    //
    // }
    if (started) _playback_started(wrap_oid(id), time, now);
    return NULL;
}

fa_ptr_t _sequence_stopped(fa_ptr_t context, fa_time_t time, fa_time_t now)
{
    sequence_t sequence = context;
    oid_t id;
    bool ok;
    fa_with_lock(sequences_mutex) {
        id = sequence->id;
        ok = (sequence->status == SEQUENCE_STOPPING) ||
            (sequence->auto_stop && fa_time_to_double(time) > (sequence->start_time + sequence->max_time));
        if (verbose) {
            printf("_sequence_stopped ok = %d  %f %f\n", ok, fa_time_to_double(time),(sequence->start_time + sequence->max_time));
        }
        if (!ok) continue;
        _playback_stopped(wrap_oid(id), time, now);
        //sequence->status = SEQUENCE_FINISHED;
        
        // Cleanup
        sequences = fa_map_dremove(wrap_oid(id), sequences);
        if (sequence->status == SEQUENCE_STARTING) {
            sequence->status = SEQUENCE_STOPPING;
            printf(">>>> _sequence_stopped called while sequence status was SEQUENCE_STARTING, setting to SEQUENCE_STOPPING\n");
        } else {
            delete_sequence(sequence); // This also releases buffer references
        }
    }
    return NULL;
}
 
int sequence_new_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;

    // if (noaudio) {
    //     if (verbose) fa_slog_info("Audio disabled, ignoring playback_new");
    //     send_osc(message, user_data, "/error", "is", id, "audio-disabled");
    //     return 0;
    // }

    bool buffered = strcmp(path, "/playback/new/buffered") == 0;
    if (buffered) {
        fa_warn(fa_string("Buffered playback not implemented yet!"));
        // The problem is that we cannot schedule an action_many with start time in the past,
        // even if that is compensated by large relative times in the action_many,
        // because the whole action will be discarded by the scheduler
        buffered = false;
    }
    if (id > last_used_playback_id) {
        fa_with_lock(sequences_mutex) {
            last_used_playback_id = id;
            fa_ptr_t value = new_sequence(id, 0, buffered);
            sequences = fa_map_dset(wrap_oid(id), value, sequences);
        }
        send_osc(message, user_data, "/playback/status", "iii", id, 0, 0); // 0 = stopped
    } else {
        fa_fail(fa_string_format("Playback ID %hu is lower than last used ID (%hu)", id, last_used_playback_id));
        send_osc(message, user_data, "/error", "Nsi", "bad-playback-id", id);
    }
    return 0;
}

static void add_midi(sequence_t sequence, float time, uint8_t cmd, uint8_t ch, uint8_t data1, uint8_t data2, float f0)
{
    if (time < 0) {
        fa_warn(fa_string("Cannot add midi with time < 0"));
        return;
    }
    
    oid_t id = sequence->id;

    fa_action_t a;

    if (cmd == 0x80 || cmd == 0x90) {
        int pitch = f0; // truncate
        int cents = round((double)(f0 - pitch) * (double)100.0);
        // printf("%x %x %x\n", pitch, data2, cents);
        a = fa_action_send(synth_name, fa_midi_message_create_extended(cmd + ch, pitch, data2, (uint8_t)cents));
    } else {
        a = fa_action_send(synth_name, fa_midi_message_create_simple(cmd + ch, data1, data2));
    }
    a = fa_action_if(check_playback_semaphore, wrap_oid(id), a); // This is only necessary for repeated actions, but we don't know yet
    
    switch (sequence->status) {
        case SEQUENCE_RUNNING: {
            if (!sequence->buffered) {
                fa_time_t time_now = fa_clock_time(current_clock);
                double now = fa_time_to_double(time_now);
                fa_destroy(time_now);
                double offset = 0;
                if (sequence->repeat > 0) {
                    time = fmod(time, sequence->repeat); // Modulo. Or should we discard events too far into the future??
                    a = fa_action_repeat(fa_time_from_double(sequence->repeat), 0, a);
                    a = fa_action_while(check_playback_semaphore, wrap_oid(id), a);
                    offset = sequence->repeat * trunc((now - sequence->start_time) / sequence->repeat);
                    if (sequence->start_time + offset + time < now) offset += sequence->repeat;
                } else {
                    // don't schedule events that should already have been played
                    if (sequence->start_time + time < now) {
                        fa_deep_destroy_always(a);
                        break;
                    }
                }
                schedule(fa_time_from_double(sequence->start_time + offset + time), a, current_midi_playback_stream);
                if (time > sequence->max_time) {
                    sequence->max_time = time;
                    if (sequence->repeat == 0) {
                        fa_action_t stop = fa_action_do_with_time(_sequence_stopped, sequence);
                        stop = fa_action_if(check_playback_semaphore, wrap_oid(id), stop);
                        double stop_time = sequence->start_time + sequence->max_time + auto_stop_margin;
                        schedule(fa_time_from_double(stop_time), stop, current_midi_playback_stream);
                    }
                }
                break; // only from switch
            }
            // NB: intentionally fall through to next case if buffered
        }
        case SEQUENCE_STOPPED:
        case SEQUENCE_STARTING: {
            fa_push_list(pair(a, fa_time_from_double(time)), sequence->midi_actions);
            sequence->max_time = MAX(time, sequence->max_time);
            break;
        }
        default: {
            fa_deep_destroy_always(a);
            if (verbose) fa_inform(fa_string("Discarding MIDI message sent to stopped sequence"));
            break;
        }
        
    }
}

//    /playback/add/midi  id  time  cmd  ch  data1  [data2]
int sequence_add_midi_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;
    float time    = argv[1]->f;
    uint8_t cmd   = argv[2]->i;
    uint8_t ch    = argv[3]->i;
    uint8_t data1 = argv[4]->f; // truncate
    uint8_t data2 = argc > 5 ? argv[5]->i : 0;

    if (noaudio) {
        return 0;
    }

    fa_with_lock(sequences_mutex) {
        sequence_t sequence = fa_map_dget(wrap_oid(id), sequences);
        if (!sequence) {
            send_osc(message, user_data, "/error", "Nsi", "no-such-playback", id);
            continue; // don't return or break, that wouldn't release the lock
        }
        add_midi(sequence, time, cmd, ch, data1, data2, argv[4]->f);
    }
    return 0;
}

//    /playback/add/note  id  time  ch  pitch  vel  dur
int sequence_add_note_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id    = argv[0]->i;
    float time  = argv[1]->f;
    uint8_t ch  = argv[2]->i;
    float pitch = argv[3]->f;
    uint8_t vel = argv[4]->i;
    float dur   = argv[5]->f;

    if (noaudio) {
        return 0;
    }
    
    if (!vel || !dur) return 0;
    
    fa_with_lock(sequences_mutex) {
        sequence_t sequence = fa_map_dget(wrap_oid(id), sequences);
        if (!sequence) {
            send_osc(message, user_data, "/error", "Nsi", "no-such-playback", id);
            continue; // don't return or break, that wouldn't release the lock
        }
        add_midi(sequence, time, 0x90, ch, pitch, vel, pitch);
        add_midi(sequence, time+dur, 0x80, ch, pitch, 0, pitch);
    }
    return 0;
}

//    /playback/add/audio  id  time  audio_id  slot  [skip]  [duration]
int sequence_add_audio_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id       = argv[0]->i;
    float time     = argv[1]->f; // not supported yet
    oid_t audio_id = argv[2]->i;
    uint8_t slot   = argv[3]->i;
    float skip     = argc > 4 ? argv[4]->f : 0;
    float duration = argc > 5 ? argv[5]->f : -1;

    if (noaudio) {
        if (verbose) fa_slog_info("Audio disabled, ignoring playback_add_audio");
        return 0;
    }
    
    // Check slot
    if (slot < 0 || slot >= kMaxAudioBufferSignals) {
        fa_fail(fa_format("Bad audio slot %d", slot));
        fa_inform(fa_format("Supported slots: 0-%d", kMaxAudioBufferSignals-1));
        send_osc(message, user_data, "/error", "isii", id, "bad-audio-slot", slot, kMaxAudioBufferSignals);
        return 0;
    }
    
    // Get audio file reference
    fa_ptr_t buffer = NULL;
    fa_with_lock(audio_files_mutex) {
        buffer = fa_map_dget(wrap_oid(audio_id), audio_files);
        if (buffer) {
            fa_take_reference(buffer);
        }
    }
    if (!buffer) {
        send_osc(message, user_data, "/error", "isi", id, "no-such-audio-file", audio_id);
        return 0;
    }
    
    fa_with_lock(sequences_mutex) {
        sequence_t sequence = fa_map_dget(wrap_oid(id), sequences);
        if (!sequence) {
            fa_release_reference(buffer);
            send_osc(message, user_data, "/error", "Nsi", "no-such-playback", id);
            continue; // don't return or break, that wouldn't release the lock
        }
        
        fa_push_list(pair(buffer, fa_i8(slot)), sequence->audio_buffers);
        
        double buffer_duration;
        {
            double sample_rate = fa_peek_number(fa_get_meta(buffer, fa_string("sample-rate")));
            size_t frames = fa_peek_integer(fa_get_meta(buffer, fa_string("frames")));
            buffer_duration = (double) frames / sample_rate; // seconds
        }
        
        if (duration == 0) {
            fa_release_reference(buffer);
            fa_warn(fa_string("Duration = 0"));
            continue;
        }

        if (skip > buffer_duration) {
            fa_release_reference(buffer);
            fa_warn(fa_string("Skipping past end"));
            continue;
        }
        
        // Schedule
        fa_list_t actions = fa_list_empty();
        fa_push_list(pair(fa_action_send_retain(audio_name, pair(fa_i16(slot), buffer)), fa_now()), actions);
        fa_push_list(pair(fa_action_send(audio_name, pair(fa_i16(slot), fa_from_int32(skip * 44100))), fa_now()), actions);
        fa_push_list(pair(fa_action_send(audio_name, pair(fa_i16(slot), fa_string("play"))), fa_now()), actions);
        if (duration > 0) {
            fa_time_t stop_time = fa_time_from_double(duration);
            fa_push_list(pair(fa_action_send(audio_name, pair(fa_i16(slot), fa_string("stop"))), stop_time), actions);
        }
        actions = times_to_delta_times(actions); // Convert absolute times to relative times
        
        fa_action_t a = fa_action_many(actions);
        
        switch (sequence->status) {
            case SEQUENCE_RUNNING: {
                fa_time_t time_now = fa_clock_time(current_clock);
                double now = fa_time_to_double(time_now);
                fa_destroy(time_now);
                double offset = 0;
                if (sequence->repeat > 0) {
                    time = fmod(time, sequence->repeat); // Modulo. Or should we discard events too far into the future??
                    a = fa_action_repeat(fa_time_from_double(sequence->repeat), 0, a);
                    a = fa_action_while(check_playback_semaphore, wrap_oid(id), a);
                    offset = sequence->repeat * trunc((now - sequence->start_time) / sequence->repeat);
                    if (sequence->start_time + offset + time < now) offset += sequence->repeat;
                } else {
                    // don't schedule events that should already have been played
                    if (sequence->start_time + time < now) {
                        printf(">> Discarding audio action\n");
                        fa_deep_destroy_always(a);
                        break;
                    }
                }
                schedule(fa_time_from_double(sequence->start_time + offset + time), a, current_audio_stream);
                double max_time = time + (duration > 0 ? duration : buffer_duration - skip);
                if (max_time > sequence->max_time) {
                    sequence->max_time = max_time;
                    if (sequence->repeat == 0) {
                        fa_action_t stop = fa_action_do_with_time(_sequence_stopped, sequence);
                        stop = fa_action_if(check_playback_semaphore, wrap_oid(id), stop);
                        double stop_time = sequence->start_time + sequence->max_time + auto_stop_margin;
                        schedule(fa_time_from_double(stop_time), stop, current_audio_stream);
                    }
                }
                break; // only from switch
            }
            case SEQUENCE_STOPPED:
            case SEQUENCE_STARTING: {
                fa_push_list(pair(a, fa_time_from_double(time)), sequence->audio_actions);
                double max_time = time + (duration > 0 ? duration : buffer_duration - skip);
                sequence->max_time = MAX(max_time, sequence->max_time);
                break;
            }
            default: {
                fa_deep_destroy_always(a);
                if (verbose) fa_inform(fa_string("Discarding audio message sent to stopped playback"));
                break;
            }
        }
    }
    
    return 0;
}

//    /playback/flush  id
int sequence_flush_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;
    fa_with_lock(sequences_mutex) {
        sequence_t sequence = fa_map_dget(wrap_oid(id), sequences);
        if (!sequence) {
            send_osc(message, user_data, "/error", "Nsi", "no-such-playback", id);
            continue; // don't return or break, that wouldn't release the lock
        }
        if (!sequence->buffered) {
            fa_warn(fa_string("Playback not buffered, flush ignored"));
            continue; // don't return or break, that wouldn't release the lock
        }
        if (sequence->status == SEQUENCE_STOPPED) {
            fa_fail(fa_string("Cannot flush stopped sequence"));
            send_osc(message, user_data, "/error", "Nsi", "playback-stopped", id);
            continue; // don't return or break, that wouldn't release the lock
        }
        flush_sequence_actions(sequence, sequence->start_time);
    }
    return 0;
}

static void schedule_sequence_actions(fa_list_t time_actions, fa_ptr_t stream, sequence_t sequence, bool master, double time) {
    
    if (!master && fa_list_is_empty(time_actions)) {
        fa_destroy(time_actions);
        return;
    }
    
    bool repeat = (sequence->repeat > 0);
    oid_t id = sequence->id;
    
    // Add _playback_started and _playback_stopped actions
    // Easiest to do before sorting the list
    if (master && !repeat) {
        if (sequence->status == SEQUENCE_STOPPED) {
            fa_time_t start_time = fa_now();
            fa_push_list(pair(fa_action_do_with_time(_sequence_started, sequence), start_time), time_actions);
        }
        fa_time_t stop_time = fa_time_from_double(sequence->max_time + auto_stop_margin);
        fa_push_list(pair(fa_action_do_with_time(_sequence_stopped, sequence), stop_time), time_actions);
    }

    // Convert absolute times to relative times
    time_actions = times_to_delta_times(time_actions);
    
    // Wrap actions
    fa_action_t main_action;
    if (repeat) {
        fa_list_t action_list = list(pair(fa_action_while(check_playback_semaphore, wrap_oid(id),
                    fa_action_repeat(fa_time_from_double(sequence->repeat), 0, fa_action_many(time_actions))), fa_now()));
        if (master && sequence->status == SEQUENCE_STOPPED) {
            fa_push_list(pair(fa_action_do_with_time(_sequence_started, sequence), fa_now()), action_list);
        }
        main_action = fa_action_many(action_list);
    } else {
        main_action = fa_action_while(check_playback_semaphore, wrap_oid(id), fa_action_many(time_actions));
    }
    
    // Send to scheduler
    if (time == 0 && !in_bundle) {
        schedule_relative(sched_delay, main_action, stream);
    } else if (time < 0 || (time == 0 && in_bundle)) {
        schedule_relative(fa_now(), main_action, stream);
    } else {
        schedule(fa_time_from_double(time), main_action, stream);
    }
}

static void flush_sequence_actions(sequence_t sequence, double time) {
    bool master_is_audio = (current_audio_stream && fa_list_is_empty(sequence->midi_actions));
    
    // TODO: if the sequence playback is wrapped in a bundle, we get
    //       a crash in schedule_relative since we can currently only
    //       schedule to one stream at a time

    if (current_midi_playback_stream) {
        // if (verbose) fa_slog_info("MIDI events: ", sequence->midi_actions);
        schedule_sequence_actions(sequence->midi_actions, current_midi_playback_stream, sequence, !master_is_audio, time);
    } else {
        fa_deep_destroy_always(sequence->midi_actions);
    }
    if (current_audio_stream) {
        // if (verbose) fa_slog_info("Audio events: ", sequence->audio_actions);
        schedule_sequence_actions(sequence->audio_actions, current_audio_stream, sequence, master_is_audio, time);
    } else {
        fa_deep_destroy_always(sequence->audio_actions);
    }
    sequence->midi_actions = fa_list_empty();
    sequence->audio_actions = fa_list_empty();
}

//    /playback/start         id  [start-time]
//    /playback/start/from    id  skip  [start-time]
//    /playback/repeat        id  [interval]  [start-time]
//    /playback/repeat/from   id  skip interval  [start-time]
int sequence_start_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id   = argv[0]->i;
    float time = 0;
    bool repeat;
    float repeat_interval = 0;
    // float skip = 0; // not implemented yet

    if (noaudio) {
        if (verbose) fa_slog_info("Audio disabled, ignoring sequence_start");
        send_osc(message, user_data, "/error", "is", id, "audio-disabled");
        return 0;
    }

    // No audio stream
    // TODO: this makes it impossible to use playback with MIDI only!
    if (!current_audio_stream || current_sample_rate == 0) {
        fa_slog_warning("No audio stream, ignoring sequence_start");
        send_osc(message, user_data, "/error", "is", id, "no-audio-stream");
        return 0;
    }
        
    if (strcmp(path, "/playback/start") == 0) {
        if (argc > 1) time = argv[1]->f;
        repeat = false;
    } else if (strcmp(path, "/playback/repeat") == 0) {
        if (argc > 1) repeat_interval = argv[1]->f;
        if (argc > 2) time = argv[2]->f;
        repeat = true;
    } else {
        send_osc(message, user_data, "/error", "iss", id, "not-implemented", path);
        return 0;
    }
    
    fa_with_lock(sequences_mutex) {
        sequence_t sequence = fa_map_dget(wrap_oid(id), sequences);
        if (!sequence) {
            send_osc(message, user_data, "/error", "Nsi", "no-such-playback", id);
            continue; // don't return or break, that wouldn't release the lock
        }
        if (sequence->status != SEQUENCE_STOPPED) {
            send_osc(message, user_data, "/error", "Nsi", "playback-already-started", id);
            continue; // don't return or break, that wouldn't release the lock
        }
        
        // Auto repeat
        if (repeat && repeat_interval == 0) {
            repeat_interval = sequence->max_time;
        }
        
        if (repeat && sequence->auto_stop) {
            fa_warn(fa_string("Ignoring auto_stop for repeating sequence"));
        }
        
        add_playback_semaphore(id, NULL, 0);
        
        sequence->repeat = repeat_interval;
        
        if (verbose) printf("Starting sequence %d%s\n", id, repeat ? " (repeat)" : "");
        
        // Schedule!
        flush_sequence_actions(sequence, time);
        
        sequence->status = SEQUENCE_STARTING;
    }
    start_time_echo();
    return 0;
}

// NB: must be called inside a sequences_mutex lock!
void sequence_stop(oid_t id, lo_message message, void *user_data)
{
    sequence_t sequence = fa_map_dget(wrap_oid(id), sequences);
    if (!sequence) {
        fa_warn(fa_format("Couldn't stop sequence %d, it doesn't exist", id));
        return;
    }
    
    if (remove_playback_semaphore(id)) {
        if (verbose) printf("Stopping sequence %d\n", id);
        if (current_clock) {
            fa_time_t now = fa_clock_time(current_clock);
            send_osc(message, user_data, "/playback/stopped", "itF", id, timetag_from_time(now));
            fa_destroy(now);
        } else {
            printf("current_clock is NULL in playback_stop!\n");
            send_osc(message, user_data, "/playback/stopped", "itF", id, timetag_from_double(0));
        }
        stop_time_echo();
        // Stop MIDI
        if (current_midi_playback_stream) {
            // Stop sounding notes as fast as possible...
            do_schedule_now(all_notes_off_action(), current_midi_playback_stream);
            // ... but also schedule a note-off for notes that may
            // already have been sent to the audio thread
            do_schedule_relative(sched_delay, all_notes_off_action(), current_midi_playback_stream);
        }
        // Stop audio
        if (current_audio_stream) {
            // Send stop message to every slot listed in playback->audio_buffers
            // (a slot may be listed more than once, but it doesn't matter)
            fa_for_each(buffer_slot, sequence->audio_buffers) {
                int8_t slot = fa_peek_int8(fa_pair_second((fa_pair_t)buffer_slot));
                do_schedule_now(fa_action_send(audio_name, pair(fa_i16(slot), fa_string("stop"))), current_audio_stream);
            }
        }
        // Cleanup
        sequences = fa_map_dremove(wrap_oid(id), sequences);
        if (sequence->status == SEQUENCE_STARTING) {
            sequence->status = SEQUENCE_STOPPING;
            if (verbose) printf(">>>> playback_stop called while playback was SEQUENCE_STARTING, setting to SEQUENCE_STOPPING\n");
        } else {
            delete_sequence(sequence); // This also releases buffer references
        }
    } else {
        fa_fail(fa_format("Sequence %d was present, but semaphore could not be removed!", id));
    }
}

int sequence_stop_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;
    fa_with_lock(sequences_mutex) {
        sequence_stop(id, message, user_data);
    }
    return 0;
}

int sequence_autostop_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;
    fa_with_lock(sequences_mutex) {
        sequence_t sequence = fa_map_dget(wrap_oid(id), sequences);
        if (!sequence) {
            send_osc(message, user_data, "/error", "Nsi", "no-such-playback", id);
            continue; // don't return or break, that wouldn't release the lock
        }
        sequence->auto_stop = types[1] == 'T';
    }
    return 0;
}

int sequence_status_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;
    fa_with_lock(sequences_mutex) {
        sequence_t sequence = fa_map_dget(wrap_oid(id), sequences);
        if (!sequence) {
            send_osc(message, user_data, "/error", "Nsi", "no-such-playback", id);
            continue; // don't return or break, that wouldn't release the lock
        }
        sequence_status_t status = sequence->status;
        size_t midi_length = fa_list_length(sequence->midi_actions);
        size_t audio_length = fa_list_length(sequence->audio_actions);
        fa_inform(fa_format("Sequence ID %d  status: %d  midi: %zu  audio: %zu", id, status, midi_length, audio_length));
        send_osc(message, user_data, "/playback/status", "iiii", id, status, midi_length, audio_length);
    }
    return 0;
}

int sequence_save_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id        = argv[0]->i;
    oid_t seq_id    = argv[1]->i;
    int sample_rate = argv[3]->i;
    size_t channels = 2; // Hardcoded to stereo

    if (verbose) fa_inform(fa_dappend(fa_string("sequence_save_handler "), fa_string_from_utf8(path)));

    if (sample_rate < 11025 || sample_rate > 192000) {
        if (verbose) fa_fail(fa_format("Bad sample rate %d", sample_rate));
        send_osc(message, user_data, path, "iFs", id, "bad-sample-rate");
        return 0;
    }
    
    fa_with_lock(sequences_mutex) {
        sequence_t sequence = fa_map_dget(wrap_oid(seq_id), sequences);
        if (!sequence) {
            if (verbose) fa_fail(fa_format("No sequence with id %d", seq_id));
            send_osc(message, user_data, "/error", "iFs", id, "no-such-sequence");
            continue; // don't return or break, that wouldn't release the lock
        }
        if (sequence->status != SEQUENCE_STOPPED) {
            if (verbose) fa_fail(fa_format("Sequence not stopped (%d)", sequence->status));
            send_osc(message, user_data, "/error", "iFs", id, "sequence-not-stopped");
            continue; // don't return or break, that wouldn't release the lock
        }

        fa_string_t target_path = fa_string_from_utf8(&argv[2]->s);
        fa_io_sink_t sink = NULL;
        if (strcmp(path, "/playback/save/aiff") == 0) {
            sink = fa_io_write_audio_file(target_path, channels, sample_rate, SF_FORMAT_AIFF | SF_FORMAT_PCM_16);
        } else if (strcmp(path, "/playback/save/wav") == 0) {
            sink = fa_io_write_audio_file(target_path, channels, sample_rate, SF_FORMAT_WAV | SF_FORMAT_PCM_16);
        } else if (strcmp(path, "/playback/save/ogg") == 0) {
            assert(false && "ogg export not implemented");
            // float ogg_quality = (argc >= 5) ? argv[4]->f : default_ogg_quality;
            // if (verbose) fa_inform(fa_format("Exporting with ogg quality %f\n", ogg_quality));
            // source = fa_io_apply(source, fa_io_create_ogg_encoder(sample_rate, channels, ogg_quality));
            // sink = fa_io_write_file(target_path);
        } else if (strcmp(path, "/playback/save/mp3") == 0) {
            int mp3_bitrate = (argc >= 5) ? argv[4]->i : default_mp3_bitrate;
            if (verbose) fa_inform(fa_format("Exporting with mp3 bitrate %d\n", mp3_bitrate));
            // fa_map_t id3 = fa_map_empty();
            // id3 = fa_map_dset(fa_string("TIT2"), fa_string("En titel med åäö i"), id3);
            // id3 = fa_map_dset(fa_string("artist"), fa_string("En artiståäöaerr"), id3);
            fa_map_t id3 = NULL;
            sink = fa_io_write_mp3_file(target_path, channels, sample_rate, mp3_bitrate, id3);
            // fa_destroy(map);
        } else if (strcmp(path, "/audio-file/save/raw") == 0) {
            sink = fa_io_write_file(target_path);
        } else {
            assert(false && "Unknown target format");
        }
        if (fa_check(sink)) {
            if (verbose) fa_fail(fa_format("Sink error: %s", fa_string_peek_utf8(fa_error_message((fa_error_t)sink))));
            send_osc(message, user_data, path, "iFs", id, fa_string_peek_utf8(fa_error_message((fa_error_t)sink)));
            fa_destroy(sink);
            sink = NULL;
        }

        if (sink) {
            fa_list_t setup_actions = list(pair(fa_action_set(kSynthLeft, synth_volume), fa_now()),
                                           pair(fa_action_set(kSynthRight, synth_volume), fa_now()),
                                           pair(fa_action_set(kAudioLeft, audio_volume), fa_now()),
                                           pair(fa_action_set(kAudioRight, audio_volume), fa_now()));
            setup_actions = list(pair(fa_action_many(setup_actions), fa_now()));

            fa_list_t controls = fa_list_append(sequence->midi_actions, sequence->audio_actions);
            controls = fa_list_dappend(setup_actions, controls);

            fa_list_t signals = construct_signal_tree(false, true);
            size_t frames = sample_rate * channels * (sequence->max_time + export_margin);
            add_playback_semaphore(seq_id, NULL, 0);
            fa_signal_run(frames, controls, signals, sample_rate, (fa_signal_audio_callback_t)&fa_io_push, sink);
            remove_playback_semaphore(seq_id);
            send_osc(message, user_data, "/sequence/save", "iTi", id, frames);
            fa_destroy(sink);

            sequences = fa_map_dremove(wrap_oid(seq_id), sequences);
            delete_sequence(sequence);
        }
        fa_destroy(target_path);
    }

    return 0;
}


#endif

