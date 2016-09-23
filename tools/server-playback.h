#include "fa/fa.h"
#include "fa/alloc.h"

#ifndef __SERVER_PLAYBACK
#define __SERVER_PLAYBACK

#include "server-utils.h"
#include "server-types.h"
#include "server-globals.h"
#include "fa/signal.h"

#include <string.h>

double auto_stop_margin = 0.010; // 10 ms


// Note: sends nominal (scheduled) time!
fa_ptr_t _playback_started(fa_ptr_t context, fa_time_t time, fa_time_t now)
{
    send_osc_async("/playback/started", "it", peek_oid(context), timetag_from_time(time));
    return NULL;
}

fa_ptr_t _playback_stopped(fa_ptr_t context, fa_time_t time, fa_time_t now)
{
    oid_t id = peek_oid(context);
    printf("_playback_stopped (%hu)\n", id);
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

typedef struct _playback_data_t * playback_data_t;
struct _playback_data_t {
    oid_t id;
    playback_status_t status;
    fa_list_t midi_actions;     // list of pairs
    fa_list_t audio_actions;    // list of pairs
    double start_time;          // actual start time (set when started)
    float max_time;             // maximum relative time; i.e. the "combined duration" of the contained events
    double repeat;              // repeat interval (set when started)
    bool auto_stop;             // stop automatically after last event is played (i.e. at start_time + max_time)
} _playback_data_t;

static inline playback_data_t new_playback_data(oid_t id, playback_status_t status) {
    playback_data_t result = fa_malloc(sizeof(struct _playback_data_t));
    result->id = id;
    result->status = status;
    result->midi_actions = fa_list_empty();
    result->audio_actions = fa_list_empty();
    result->start_time = 0;
    result->max_time = 0;
    result->auto_stop = false;
    return result;
}

static void flush_playback_actions(playback_data_t playback, double time);

fa_ptr_t _playback_started2(fa_ptr_t context, fa_time_t time, fa_time_t now)
{
    playback_data_t playback = context;
    oid_t id;
    fa_with_lock(playback_data_mutex) {
        playback->status = PLAYBACK_RUNNING;
        playback->start_time = fa_time_to_double(time);
        printf("_playback_started2  %f\n", playback->start_time);
        id = playback->id;
        flush_playback_actions(playback, playback->start_time);
    }
    _playback_started(wrap_oid(id), time, now);
    return NULL;
}

fa_ptr_t _playback_stopped2(fa_ptr_t context, fa_time_t time, fa_time_t now)
{
    playback_data_t playback = context;
    oid_t id;
    bool ok;
    fa_with_lock(playback_data_mutex) {
        id = playback->id;
        ok = (playback->status == PLAYBACK_STOPPING) ||
            (playback->auto_stop && fa_time_to_double(time) > (playback->start_time + playback->max_time));
    }
    printf("_playback_stopped2 ok = %d  %f %f\n", ok, fa_time_to_double(time), (playback->start_time + playback->max_time));
    if (!ok) return NULL;
    _playback_stopped(wrap_oid(id), time, now);
    playback->status = PLAYBACK_FINISHED;
    return NULL;
}


int playback_new_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;
    if (id > last_used_playback_id) {
        fa_with_lock(playback_data_mutex) {
            last_used_playback_id = id;
            fa_ptr_t value = new_playback_data(id, 0);
            playback_data = fa_map_dset(wrap_oid(id), value, playback_data);
        }
        send_osc(message, user_data, "/playback/status", "iii", id, 0, 0); // 0 = stopped
    } else {
        fa_fail(fa_string_format("Playback ID %hu is lower than last used ID (%hu)", id, last_used_playback_id));
        send_osc(message, user_data, "/error", "Nsi", "bad-playback-id", id);
    }
    return 0;
}


static void add_midi(playback_data_t playback, float time, uint8_t cmd, uint8_t ch, uint8_t data1, uint8_t data2, float f0)
{
    if (time < 0) {
        fa_warn(fa_string("Cannot add midi with time < 0"));
        return;
    }
    
    oid_t id = playback->id;

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
    
    switch (playback->status) {
        case PLAYBACK_RUNNING: {
            fa_time_t time_now = fa_clock_time(current_clock);
            double now = fa_time_to_double(time_now);
            fa_destroy(time_now);
            double offset = 0;
            if (playback->repeat > 0) {
                time = fmod(time, playback->repeat); // Modulo. Or should we discard events too far into the future??
                a = fa_action_repeat(fa_time_from_double(playback->repeat), 0, a);
                a = fa_action_while(check_playback_semaphore, wrap_oid(id), a);
                offset = playback->repeat * trunc((now - playback->start_time) / playback->repeat);
                if (playback->start_time + offset + time < now) offset += playback->repeat;
            } else {
                // don't schedule events that should already have been played
                if (playback->start_time + time < now) {
                    fa_deep_destroy_always(a);
                    break;
                }
            }
            schedule(fa_time_from_double(playback->start_time + offset + time), a, current_midi_playback_stream);
            playback->max_time = MAX(time, playback->max_time);
            if (playback->repeat == 0) {
                fa_action_t stop = fa_action_do_with_time(_playback_stopped2, playback);
                stop = fa_action_if(check_playback_semaphore, wrap_oid(id), stop);
                double stop_time = playback->start_time + playback->max_time + auto_stop_margin;
                schedule(fa_time_from_double(stop_time), a, current_midi_playback_stream);
            }
            break; // only from switch
        }
        case PLAYBACK_STOPPED:
        case PLAYBACK_STARTING: {
            fa_push_list(pair(a, fa_time_from_double(time)), playback->midi_actions);
            playback->max_time = MAX(time, playback->max_time);
            break;
        }
        default: {
            fa_deep_destroy_always(a);
            if (verbose) fa_inform(fa_string("Discarding MIDI message sent to stopped playback"));
            break;
        }
        
    }
}


int playback_add_midi_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;
    float time    = argv[1]->f;
    uint8_t cmd   = argv[2]->i;
    uint8_t ch    = argv[3]->i;
    uint8_t data1 = argv[4]->f; // truncate
    uint8_t data2 = argc > 5 ? argv[5]->i : 0;

    fa_with_lock(playback_data_mutex) {
        playback_data_t playback = fa_map_dget(wrap_oid(id), playback_data);
        if (!playback) {
            send_osc(message, user_data, "/error", "Nsi", "no-such-playback", id);
            continue; // don't return or break, that wouldn't release the lock
        }
        add_midi(playback, time, cmd, ch, data1, data2, data1);
    }
    return 0;
}

//    /playback/add/note  id  time  ch  pitch  vel  dur
int playback_add_note_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id    = argv[0]->i;
    float time  = argv[1]->f;
    uint8_t ch  = argv[2]->i;
    float pitch = argv[3]->f;
    uint8_t vel = argv[4]->i;
    float dur   = argv[5]->f;
    
    if (!vel || !dur) return 0;
    
    fa_with_lock(playback_data_mutex) {
        playback_data_t playback = fa_map_dget(wrap_oid(id), playback_data);
        if (!playback) {
            send_osc(message, user_data, "/error", "Nsi", "no-such-playback", id);
            continue; // don't return or break, that wouldn't release the lock
        }
        add_midi(playback, time, 0x90, ch, pitch, vel, pitch);
        add_midi(playback, time+dur, 0x80, ch, pitch, 0, pitch);
    }
    return 0;
}

int playback_add_audio_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
}



static void schedule_playback_actions(fa_list_t time_actions, fa_ptr_t stream, playback_data_t playback, bool master, double time) {
    
    if (!master && fa_list_is_empty(time_actions)) {
        fa_destroy(time_actions);
        return;
    }
    
    bool repeat = (playback->repeat > 0);
    oid_t id = playback->id;
    
    // Add _playback_started and _playback_stopped actions
    // Easiest to do before sorting the list
    if (master && !repeat) {
        if (playback->status == PLAYBACK_STOPPED) {
            fa_time_t start_time = fa_now();
            fa_push_list(pair(fa_action_do_with_time(_playback_started2, playback), start_time), time_actions);
        }
        fa_time_t stop_time = fa_time_from_double(playback->max_time + auto_stop_margin);
        fa_push_list(pair(fa_action_do_with_time(_playback_stopped2, playback), stop_time), time_actions);
    }

    // Convert absolute times to relative times
    time_actions = times_to_delta_times(time_actions);
    
    // Wrap actions
    fa_action_t main_action;
    if (repeat) {
        fa_list_t action_list = list(pair(fa_action_while(check_playback_semaphore, wrap_oid(id),
                    fa_action_repeat(fa_time_from_double(playback->repeat), 0, fa_action_many(time_actions))), fa_now()));
        if (master && playback->status == PLAYBACK_STOPPED) {
            fa_push_list(pair(fa_action_do_with_time(_playback_started2, playback), fa_now()), action_list);
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

static void flush_playback_actions(playback_data_t playback, double time) {
    bool master_is_audio = (current_audio_stream && fa_list_is_empty(playback->audio_actions));
    
    if (current_midi_playback_stream) {
        schedule_playback_actions(playback->midi_actions, current_midi_playback_stream, playback, !master_is_audio, time);
    } else {
        fa_deep_destroy_always(playback->midi_actions);
    }
    if (current_audio_stream) {
        schedule_playback_actions(playback->audio_actions, current_audio_stream, playback, master_is_audio, time);
    } else {
        fa_deep_destroy_always(playback->audio_actions);
    }
    playback->midi_actions = fa_list_empty();
    playback->audio_actions = fa_list_empty();
    
}


//    /playback/start         id  [start-time]
//    /playback/start/from    id  skip  [start-time]
//    /playback/repeat        id  [interval]  [start-time]
//    /playback/repeat/from   id  skip interval  [start-time]
int playback_start_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id   = argv[0]->i;
    float time = 0;
    bool repeat;
    float repeat_interval = 0;
    float skip = 0; // not implemented yet
    
    if (strcmp(path, "/playback/start") == 0) {
        if (argc > 1) time = argv[1]->f;
        repeat = false;
    } else if (strcmp(path, "/playback/repeat") == 0) {
        if (argc > 1) repeat_interval = argv[1]->f;
        if (argc > 2) time = argv[2]->f;
        repeat = true;
    } else {
        send_osc(message, user_data, "/error", "Nss", "not-implemented", path);
        return 0;
    }
    
    fa_with_lock(playback_data_mutex) {
        playback_data_t playback = fa_map_dget(wrap_oid(id), playback_data);
        if (!playback) {
            send_osc(message, user_data, "/error", "Nsi", "no-such-playback", id);
            continue; // don't return or break, that wouldn't release the lock
        }
        if (playback->status != PLAYBACK_STOPPED) {
            send_osc(message, user_data, "/error", "Nsi", "playback-already-started", id);
            continue; // don't return or break, that wouldn't release the lock
        }
        
        // Auto repeat
        if (repeat && repeat_interval == 0) {
            repeat_interval = playback->max_time;
        }
        
        if (repeat && playback->auto_stop) {
            fa_warn(fa_string("Ignoring auto_stop for repeating playback"));
        }
        
        add_playback_semaphore(id, NULL, 0);
        
        playback->repeat = repeat_interval;
        
        // Schedule!
        flush_playback_actions(playback, time);
        
        playback->status = PLAYBACK_STARTING;
    }
    start_time_echo();
    return 0;
}


        /*
        // Add _playback_started and _playback_stopped actions
        // Easiest to do before sorting the list
        if (!repeat) {
            fa_time_t start_time = fa_now();
            fa_time_t stop_time = fa_time_from_double(playback->max_time + auto_stop_margin);
            fa_push_list(pair(fa_action_do_with_time(_playback_started2, playback), start_time), playback->time_actions);
            fa_push_list(pair(fa_action_do_with_time(_playback_stopped2, playback), stop_time), playback->time_actions);
        }

        // Convert absolute times to relative times
        playback->time_actions = times_to_delta_times(playback->time_actions);
        
        // 
        add_playback_semaphore(id, NULL, 0);
        fa_action_t main_action;
        if (repeat) {
            if (playback->auto_stop) {
                fa_warn(fa_string("Ignoring auto_stop for repeating playback"));
            }
            main_action =
                fa_action_many(
                    list(pair(fa_action_do_with_time(_playback_started2, playback), fa_now()),
                         pair(fa_action_while(check_playback_semaphore, wrap_oid(id),
                              fa_action_repeat(fa_time_from_double(repeat_interval), 0, fa_action_many(playback->time_actions))),
                                  fa_now())));
        } else {
            main_action = fa_action_while(check_playback_semaphore, wrap_oid(id), fa_action_many(playback->time_actions));
        }
        
        // Send to scheduler
        if (time == 0 && !in_bundle) {
            schedule_relative(sched_delay, main_action, current_midi_playback_stream);
        } else if (time < 0 || (time == 0 && in_bundle)) {
            schedule_relative(fa_now(), main_action, current_midi_playback_stream);
        } else {
            schedule(fa_time_from_double(time), main_action, current_midi_playback_stream);
        }
        playback->repeat = repeat_interval;
        playback->status = PLAYBACK_STARTING;
        playback->time_actions = fa_list_empty();
        
        start_time_echo();
    }
    
    return 0;
}
*/

int playback_stop_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
}

int playback_autostop_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;
    fa_with_lock(playback_data_mutex) {
        playback_data_t playback = fa_map_dget(wrap_oid(id), playback_data);
        if (!playback) {
            send_osc(message, user_data, "/error", "Nsi", "no-such-playback", id);
            continue; // don't return or break, that wouldn't release the lock
        }
        playback->auto_stop = types[1] == 'T';
    }
    return 0;
}

int playback_status_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;
    fa_with_lock(playback_data_mutex) {
        playback_data_t playback = fa_map_dget(wrap_oid(id), playback_data);
        if (!playback) {
            send_osc(message, user_data, "/error", "Nsi", "no-such-playback", id);
            continue; // don't return or break, that wouldn't release the lock
        }
        playback_status_t status = playback->status;
        size_t midi_length = fa_list_length(playback->midi_actions);
        size_t audio_length = fa_list_length(playback->audio_actions);
        fa_inform(fa_format("Playback ID %d  status: %d  midi: %zu  audio: %zu", id, status, midi_length, audio_length));
        send_osc(message, user_data, "/playback/status", "iiii", id, status, midi_length, audio_length);
    }
    return 0;
}




#endif

