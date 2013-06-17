
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/audio_engine.h>
#include <fae/util.h>

#include <unistd.h> // isatty
#include "config.h"

#define iso8601_k "%Y-%m-%d %H:%M:%S%z"

typedef fae_audio_engine_log_func_t log_func_t;

static unsigned       init_count_g  = 0;
static log_func_t     log_func_g    = NULL;
static ptr_t          log_data_g    = NULL;

static struct { char* pre; int x; int y; int z; char* suff } version_g = AE_VERSION;

void fae_device_audio_initialize();
void fae_device_audio_terminate();
void fae_device_midi_initialize();
void fae_device_midi_terminate();
void fae_thread_initialize();
void fae_thread_terminate();
void fae_time_initialize();
void fae_time_terminate();

fae_list_t fae_audio_engine_version()
{
    return list(
        string(version_g.pre), 
        i16(version_g.x),
        i16(version_g.y),
        i16(version_g.z),
        string(version_g.suff));
}

fae_string_t fae_audio_engine_version_string()
{
    char version[100];
    sprintf(&version, "%s%d.%d.%d%s", version_g.pre, version_g.x, version_g.y, version_g.z, version_g.suff);
    return string(&version);
}

/** Performs global initialization.

    This function must be called exactly once before any other function in the library.
    A call to fae_audio_engine_terminate() will reset the global state so that
    fae_audio_engine_initialize() may be called again and so on.
 */
void fae_audio_engine_initialize()
{
    fae_device_audio_initialize();
    fae_device_midi_initialize();
    fae_thread_initialize();
    fae_time_initialize();
    fae_audio_engine_log_info(string("Initialized Audio Engine."));

    init_count_g++;
}

/** Performs global cleanup.

    This function may be used to reset the global state as per above. It is not necessary to
    call this function before the program finishes.
 */
void fae_audio_engine_terminate()
{
    if ((init_count_g--)) {
        fae_device_audio_terminate();
        fae_device_midi_terminate();
        fae_thread_terminate();
        fae_time_terminate();
        fae_audio_engine_log_info(string("Terminated Audio Engine."));
    } else {
        fae_audio_engine_log_warning(string("Audio Engine could not terminate: inconsistent state."));
    }
}

// --------------------------------------------------------------------------------

#define max_log_length_k 3000

static inline void stdlog(ptr_t data, fae_time_system_t t, fae_error_t e)
{
    FILE *file = data;
    char msg[max_log_length_k + 50];
    bool color = (file == stdout && isatty(fileno(stdout)));

    fae_let(tm, localtime((long *) &t)) {
        strftime(msg, 50, iso8601_k "  ", tm);
    }
    fae_with(str, fae_error_format(color, e),
                 fae_destroy(str)) {
        fae_with(cstr, fae_string_to_utf8(str),
                     free(cstr)) {
            strncat(msg, cstr, max_log_length_k - 2);
            strncat(msg, "\n", 1);
        }
    }
    fputs(msg, file);
    fflush(file);
}

/** Instruct the Audio Engine to write log messages to the specific file.
 */
void fae_audio_engine_set_log_file(fae_string_file_path_t path)
{
    char *cpath = fae_string_to_utf8(path);
    log_data_g  = fopen(cpath, "a");
    log_func_g  = stdlog;
    free(cpath);
}

/** Instruct the Audio Engine to write log messages to the standard output.
 */
void fae_audio_engine_set_log_std()
{
    log_data_g  = stdout;
    log_func_g  = stdlog;
}

/** Instruct the Audio Engine to pass log messages to the given handler.
 */
void fae_audio_engine_set_log(fae_audio_engine_log_func_t f, fae_ptr_t data)
{
    log_func_g  = f;
    log_data_g  = data;
}


// --------------------------------------------------------------------------------

/** Write a log message.

    @param context
        Ignored, declared for compability with user-defined callbacks.
    @param error
        Condition to log. Must implement [Error](@ref fae_error_interface_t).
 */
void fae_audio_engine_log(fae_ptr_t data, fae_error_t e)
{
    if (log_func_g) {
        log_func_g(log_data_g, (ptr_t) time(NULL), e);
    }
}

void fae_audio_engine_dlog(fae_ptr_t data, fae_error_t e)
{
    fae_audio_engine_log(data, e);
    fae_destroy(e);
}


/** Write an informative message to the log.
 */
void fae_audio_engine_log_info(fae_string_t msg)
{
    fae_audio_engine_log_info_from(msg, string(""));
}

void fae_audio_engine_dlog_info(fae_string_t msg)
{
    fae_audio_engine_log_info(msg);
    fae_destroy(msg);
}


/** Write a warning to the log.
 */
void fae_audio_engine_log_warning(fae_string_t msg)
{
    fae_audio_engine_log_warning_from(msg, string(""));
}

/** Write an error to the log.
 */
void fae_audio_engine_log_error(fae_string_t msg)
{
    fae_audio_engine_log_error_from(msg, string(""));
}

/** Write an informative message to the log.
 */
void fae_audio_engine_log_info_from(fae_string_t msg, fae_string_t origin)
{
    error_t err = fae_error_create_simple(info, msg, origin);
    fae_audio_engine_log(NULL, err);
    fae_destroy(err);
}

/** Write a warning to the log.
 */
void fae_audio_engine_log_warning_from(fae_string_t msg, fae_string_t origin)
{
    error_t err = fae_error_create_simple(warning, msg, origin);
    fae_audio_engine_log(NULL, err);
    fae_destroy(err);
}

/** Write an error to the log.
 */
void fae_audio_engine_log_error_from(fae_string_t msg, fae_string_t origin)
{
    error_t err = fae_error_create_simple(error, msg, origin);
    fae_audio_engine_log(NULL, err);
    fae_destroy(err);
}


