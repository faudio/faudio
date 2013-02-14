
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/audio_engine.h>
#include <doremir/util.h>

#include <unistd.h> // isatty


#define iso8601_k "%Y-%m-%d %H:%M:%S%z"

typedef doremir_audio_engine_log_func_t log_func_t;

static unsigned       init_count_g  = 0;
static log_func_t     log_func_g    = NULL;
static ptr_t          log_data_g      = NULL;



void doremir_device_audio_initialize();
void doremir_device_audio_terminate();
void doremir_device_midi_initialize();
void doremir_device_midi_terminate();
void doremir_thread_initialize();
void doremir_thread_terminate();
void doremir_time_initialize();
void doremir_time_terminate();

/** Performs global initialization.

    This function must be called exactly once before any other function in the library.
    A call to doremir_audio_engine_terminate() will reset the global state so that
    doremir_audio_engine_initialize() may be called again and so on.
 */
void doremir_audio_engine_initialize()
{
    doremir_device_audio_initialize();
    doremir_device_midi_initialize();
    doremir_thread_initialize();
    doremir_time_initialize();
    doremir_audio_engine_log_info(string("Initialized Audio Engine."));

    init_count_g++;
}

/** Performs global cleanup.

    This function may be used to reset the global state as per above. It is not necessary to
    call this function before the program finishes.
 */
void doremir_audio_engine_terminate()
{
    if ((init_count_g--)) {
        doremir_device_audio_terminate();
        doremir_device_midi_terminate();
        doremir_thread_terminate();
        doremir_time_terminate();
        doremir_audio_engine_log_info(string("Terminated Audio Engine."));
    } else {
        doremir_audio_engine_log_warning(string("Audio Engine could not terminate: inconsistent state."));
    }
}

// --------------------------------------------------------------------------------

#define max_log_length_k 3000

static inline void stdlog(ptr_t data, doremir_time_system_t t, doremir_error_t e)
{
    FILE *file = data;
    char msg[max_log_length_k+50];
    bool color = (file == stdout && isatty(fileno(stdout)));

    doremir_let(tm, localtime((long *) &t)) {
        strftime(msg, 50, iso8601_k "  ", tm);
    }
    doremir_with(str, doremir_error_format(color, e),
                 doremir_destroy(str)) {
        doremir_with(cstr, doremir_string_to_utf8(str),
                     free(cstr)) {
            strncat(msg, cstr, max_log_length_k-2);
            strncat(msg, "\n", 1);
        }
    }
    fputs(msg, file);
    fflush(file);
}

/** Instruct the Audio Engine to write log messages to the specific file.
 */
void doremir_audio_engine_set_log_file(doremir_string_file_path_t path)
{
    char *cpath = doremir_string_to_utf8(path);
    log_data_g  = fopen(cpath, "a");
    log_func_g  = stdlog;
    free(cpath);
}

/** Instruct the Audio Engine to write log messages to the standard output.
 */
void doremir_audio_engine_set_log_std()
{
    log_data_g  = stdout;
    log_func_g  = stdlog;
}

/** Instruct the Audio Engine to pass log messages to the given handler.
 */
void doremir_audio_engine_set_log(doremir_audio_engine_log_func_t f, doremir_ptr_t data)
{
    log_func_g  = f;
    log_data_g  = data;
}


// --------------------------------------------------------------------------------

/** Write a log message.

    @param context
        Ignored, declared for compability with user-defined callbacks.
    @param error
        Condition to log. Must implement [Error](@ref doremir_error_interface_t).
 */
void doremir_audio_engine_log(doremir_ptr_t data, doremir_error_t e)
{
    if (log_func_g) {
        log_func_g(log_data_g, (ptr_t) time(NULL), e);
    }
}

void doremir_audio_engine_dlog(doremir_ptr_t data, doremir_error_t e)
{
    doremir_audio_engine_log(data, e);
    doremir_destroy(e);
}


/** Write an informative message to the log.
 */
void doremir_audio_engine_log_info(doremir_string_t msg)
{
    doremir_audio_engine_log_info_from(msg, string(""));
}

void doremir_audio_engine_dlog_info(doremir_string_t msg)
{
    doremir_audio_engine_log_info(msg);
    doremir_destroy(msg);
}


/** Write a warning to the log.
 */
void doremir_audio_engine_log_warning(doremir_string_t msg)
{
    doremir_audio_engine_log_warning_from(msg, string(""));
}

/** Write an error to the log.
 */
void doremir_audio_engine_log_error(doremir_string_t msg)
{
    doremir_audio_engine_log_error_from(msg, string(""));
}

/** Write an informative message to the log.
 */
void doremir_audio_engine_log_info_from(doremir_string_t msg, doremir_string_t origin)
{
    error_t err = doremir_error_create_simple(info, msg, origin);
    doremir_audio_engine_log(NULL, err);
    doremir_destroy(err);
}

/** Write a warning to the log.
 */
void doremir_audio_engine_log_warning_from(doremir_string_t msg, doremir_string_t origin)
{
    error_t err = doremir_error_create_simple(warning, msg, origin);
    doremir_audio_engine_log(NULL, err);
    doremir_destroy(err);
}

/** Write an error to the log.
 */
void doremir_audio_engine_log_error_from(doremir_string_t msg, doremir_string_t origin)
{
    error_t err = doremir_error_create_simple(error, msg, origin);
    doremir_audio_engine_log(NULL, err);
    doremir_destroy(err);
}


