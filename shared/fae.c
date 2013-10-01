
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/fa.h>
#include <fa/util.h>

#include <unistd.h> // isatty
#include "config.h"

#define iso8601_k "%Y-%m-%d %H:%M:%S%z"

typedef fa_fa_log_func_t log_func_t;

static unsigned       init_count_g  = 0;
static log_func_t     log_func_g    = NULL;
static ptr_t          log_data_g    = NULL;
static long           gBytesAlloc;
// static long           gBytesFreed;

static struct {
    char *pre;
    int x;
    int y;
    int z;
    char *suff;
} version_g = AE_VERSION;

void fa_audio_initialize();
void fa_audio_terminate();
void fa_midi_initialize();
void fa_midi_terminate();
void fa_thread_initialize();
void fa_thread_terminate();
void fa_time_initialize();
void fa_time_terminate();

fa_list_t fa_fa_version()
{
    return list(
               string(version_g.pre),
               i16(version_g.x),
               i16(version_g.y),
               i16(version_g.z),
               string(version_g.suff));
}

fa_string_t fa_fa_version_string()
{
    char version[100];
    sprintf(version,
            "%s%d.%d.%d%s",
            version_g.pre,
            version_g.x,
            version_g.y,
            version_g.z,
            version_g.suff);
    return string(version);
}

void fa_fa_initialize()
{
    fa_audio_initialize();
    fa_midi_initialize();
    fa_thread_initialize();
    fa_time_initialize();
    fa_fa_log_info(string("Initialized faudio."));

    gBytesAlloc = 0;
    init_count_g++;
}

void fa_fa_terminate()
{
    if ((init_count_g--)) {
        fa_audio_terminate();
        fa_midi_terminate();
        fa_thread_terminate();
        fa_time_terminate();

        fa_fa_log_info(fa_string_dappend(string("Total bytes allocated: "),
                                         fa_string_show(i32(gBytesAlloc))));

        fa_fa_log_info(string("Terminated faudio."));
    } else {
        fa_fa_log_warning(string("Could not terminate faudio: inconsistent state."));
    }
}

// --------------------------------------------------------------------------------


void *fa_malloc(size_t size)
{
    gBytesAlloc += size;
    return malloc(size);
}
void *fa_realloc(void *ptr, size_t size)
{
    return realloc(ptr, size);
}
void fa_free(void *ptr)
{
    free(ptr);
}


// --------------------------------------------------------------------------------

#define kMaxLogSize 3000

static inline void stdlog(ptr_t data, fa_time_system_t t, fa_error_t e)
{
    FILE *file = data;
    char msg[kMaxLogSize + 50];
    bool color = (file == stdout && isatty(fileno(stdout)));

    fa_let(tm, localtime((long *) &t)) {
        strftime(msg, 50, iso8601_k "  ", tm);
    }
    fa_with(str, fa_error_format(color, e),
            fa_destroy(str)) {
        fa_with(cstr, fa_string_to_utf8(str),
                free(cstr)) {
            strncat(msg, cstr, kMaxLogSize - 2);
            strncat(msg, "\n", 1);
        }
    }
    fputs(msg, file);
    fflush(file);
}

void fa_fa_set_log_file(fa_string_t path)
{
    char *cpath = fa_string_to_utf8(path);
    log_data_g  = fopen(cpath, "a");
    log_func_g  = stdlog;
    free(cpath);
}

void fa_fa_set_log_std()
{
    log_data_g  = stdout;
    log_func_g  = stdlog;
}

void fa_fa_set_log(fa_fa_log_func_t f, fa_ptr_t data)
{
    log_func_g  = f;
    log_data_g  = data;
}


// --------------------------------------------------------------------------------

void fa_fa_log(fa_ptr_t data, fa_error_t e)
{
    if (log_func_g) {
        log_func_g(log_data_g, (ptr_t) time(NULL), e);
    }
}

void fa_fa_dlog(fa_ptr_t data, fa_error_t e)
{
    fa_fa_log(data, e);
    fa_destroy(e);
}


void fa_fa_log_info(fa_string_t msg)
{
    fa_fa_log_info_from(msg, string(""));
}

void fa_fa_dlog_info(fa_string_t msg)
{
    fa_fa_log_info(msg);
    fa_destroy(msg);
}

void fa_fa_log_warning(fa_string_t msg)
{
    fa_fa_log_warning_from(msg, string(""));
}

void fa_fa_log_error(fa_string_t msg)
{
    fa_fa_log_error_from(msg, string(""));
}

void fa_fa_log_info_from(fa_string_t msg, fa_string_t origin)
{
    error_t err = fa_error_create_simple(info, msg, origin);
    fa_fa_log(NULL, err);
    fa_destroy(err);
}

void fa_fa_log_warning_from(fa_string_t msg, fa_string_t origin)
{
    error_t err = fa_error_create_simple(warning, msg, origin);
    fa_fa_log(NULL, err);
    fa_destroy(err);
}

void fa_fa_log_error_from(fa_string_t msg, fa_string_t origin)
{
    error_t err = fa_error_create_simple(error, msg, origin);
    fa_fa_log(NULL, err);
    fa_destroy(err);
}


