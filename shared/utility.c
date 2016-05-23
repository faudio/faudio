
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <mpg123.h>
#include <fa/fa.h>
#include <fa/util.h>
#include <portaudio.h>

#include <unistd.h> // Needs isatty()
#include "config.h"

#define kIso8601 "%Y-%m-%d %H:%M:%S%z"

typedef fa_log_func_t log_func_t;

static unsigned             gInitCount    = 0;
static long                 gBytesAlloc   = 0;
static long                 gRegionCount  = 0;
static log_func_t           gLogFunc      = NULL;
static fa_ptr_t             gLogData      = NULL;
static bool                 gLogDupStdout = false; // Hack warning... but it was the easiest way to print to both
static fa_error_severity_t  gLogLevel     = info;

static struct {
    char *pre;
    int x;
    int y;
    int z;
    char *suff;
} version_g = FA_VERSION;

void fa_audio_initialize();
void fa_audio_terminate();
void fa_midi_initialize();
void fa_midi_terminate();
void fa_thread_initialize();
void fa_thread_terminate();
void fa_clock_initialize();
void fa_clock_terminate();
void fa_device_initialize();
void fa_device_terminate();

fa_list_t fa_version()
{
    return list(
               fa_string(version_g.pre),
               fa_i16(version_g.x),
               fa_i16(version_g.y),
               fa_i16(version_g.z),
               fa_string(version_g.suff));
}

fa_string_t fa_version_string()
{
    char version[100];
    sprintf(version,
            "%s%d.%d.%d%s",
            version_g.pre,
            version_g.x,
            version_g.y,
            version_g.z,
            version_g.suff);
    return fa_string(version);
}

/*
    TODO
        - Determine a good stack size for kMaxSignalTreeDepth

inline static
void fix_stack_size()
{
    const rlim_t kStackSize = (0) *(1024*1024);   // min stack size = 16 MB
    struct rlimit rl;
    int result;

    result = getrlimit(RLIMIT_STACK, &rl);
    if (result == 0)
    {
        if (rl.rlim_cur < kStackSize)
        {
            rl.rlim_cur = kStackSize;
            result = setrlimit(RLIMIT_STACK, &rl);
            if (result != 0)
            {
                fprintf(stderr, "setrlimit returned result = %d\n", result);
            }
        }
    }
}
*/

void fa_initialize()
{
    gBytesAlloc = 0;
    gRegionCount = 0;
	
    fa_log_info(fa_string(""));

    fa_log_info(fa_dappend(
                fa_string("Initializing faudio "),
                fa_string_dappend(fa_version_string(),
                                  fa_string_dappend(fa_string(""),
#ifdef FAUDIO_DEBUG
                                                    fa_string(" (debug build)")))
#else
                                                    fa_string(" (release build)")))
#endif
                ));

    //PaVersionInfo paInfo = Pa_GetVersionInfo();
    fa_log_info(fa_dappend(fa_string("Using "), fa_string(Pa_GetVersionText())));

    // fix_stack_size();

    // This order is important!

    // Audio and midi needs to be initalized last as they
    // depend on threads and other stuff.

    fa_device_initialize();
    fa_audio_initialize();
    fa_midi_initialize();
    
    mpg123_init();
	
    fa_log_info(fa_string("Done initializing faudio"));

    gInitCount++;
}

void fa_terminate()
{
    if ((gInitCount--)) {
        fa_audio_terminate();
        fa_midi_terminate();
        fa_thread_terminate();
        fa_clock_terminate();
        fa_device_terminate();
        
        mpg123_exit();

        fa_log_info(fa_string_dappend(fa_string("Total bytes allocated: "),
                                      fa_string_dshow(fa_i32(gBytesAlloc))));

        fa_log_info(fa_string_dappend(fa_string("Regions leaked: "),
                                      fa_string_dshow(fa_i32(gRegionCount))));

        fa_log_info(fa_string("Terminated faudio"));
    } else {
        fa_log_warning(fa_string("Could not terminate faudio: inconsistent state"));
    }
}

// --------------------------------------------------------------------------------

static int reenter_malloc = 0;

void print_malloc_info(size_t ba)
{
    if (!reenter_malloc) {
        reenter_malloc++;

        if (ba > 500) {
            fa_inform(fa_string_format_integral("Allocating %d bytes", (long) ba));
        }

        reenter_malloc--;
    }
}

void print_free_info(size_t rc)
{
}

void *fa_malloc(size_t size)
{
#ifdef FAUDIO_DEBUG_ALLOC
    print_malloc_info(size);
#endif
    gBytesAlloc += size;
    gRegionCount += 1;
    return malloc(size);
}
void *fa_realloc(void *ptr, size_t size)
{
    return realloc(ptr, size);
}
void fa_free(void *ptr)
{
#ifdef FAUDIO_DEBUG_ALLOC
    print_free_info(gRegionCount);
#endif
    gRegionCount -= 1;
    free(ptr);
}


// --------------------------------------------------------------------------------

#define kMaxLogSize 3000

static inline void stdlog(fa_ptr_t data, fa_time_system_t t, fa_error_t e)
{
    FILE *file = data;
    char msg[kMaxLogSize + 50];
#ifdef _WIN32
    bool color = false;
#else
    bool color = (file == stdout && isatty(fileno(stdout)));
#endif
    fa_let(tm, localtime((long *) &t)) {
        strftime(msg, 50, kIso8601 "  ", tm);
    }
    fa_with(str, fa_error_format(color, e),
            fa_destroy(str)) {
        fa_with(cstr, fa_string_to_utf8(str),
                fa_free(cstr)) {
            strncat(msg, cstr, kMaxLogSize - 2);
            strncat(msg, "\n", 1);
        }
    }
    fputs(msg, file);
    fflush(file);
    if (gLogDupStdout && file != stdout) {
        fputs(msg, stdout);
        fflush(stdout);
    }
}

void fa_set_log_file(fa_string_t path)
{
    char *cpath = fa_string_to_utf8(path);
    gLogData  = fopen(cpath, "a");
    gLogFunc  = stdlog;
    fa_free(cpath);
}

void fa_set_log_file_and_stdout(fa_string_t path)
{
    char *cpath = fa_string_to_utf8(path);
    gLogData      = fopen(cpath, "a");
    gLogFunc      = stdlog;
    gLogDupStdout = true;
    fa_free(cpath);
}

void fa_set_log_std()
{
    gLogData      = stdout;
    gLogFunc      = stdlog;
    gLogDupStdout = false;
}

void fa_set_log(fa_log_func_t f, fa_ptr_t data)
{
    gLogFunc      = f;
    gLogData      = data;
    gLogDupStdout = false;
}

void fa_set_log_level(fa_error_severity_t level)
{
    gLogLevel = level;
}


// --------------------------------------------------------------------------------

void fa_log(fa_ptr_t data, fa_error_t e)
{
    if (fa_error_severity(e) < gLogLevel) return;
    if (gLogFunc) {
        gLogFunc(gLogData, (fa_ptr_t) time(NULL), e);
    }
    fa_destroy(e);
}

void fa_log_info(fa_string_t msg)
{
    fa_log_info_from(msg, NULL);
}

// void fa_dlog_info(fa_string_t msg)
// {
//     fa_log_info(msg);
//     fa_destroy(msg);
// }

void fa_log_warning(fa_string_t msg)
{
    fa_log_warning_from(msg, NULL);
}

// void fa_dlog_warning (fa_string_t msg)
// {
//     fa_log_warning(msg);
//     fa_destroy(msg);
// }

void fa_log_error(fa_string_t msg)
{
    fa_log_error_from(msg, NULL);
}

void fa_log_info_from(fa_string_t msg, fa_string_t origin)
{
    if (gLogLevel > info) return;
    fa_error_t err = fa_error_create_simple(info, msg, origin);
    fa_log(NULL, err);
}

void fa_log_warning_from(fa_string_t msg, fa_string_t origin)
{
    if (gLogLevel > warning) return;
    fa_error_t err = fa_error_create_simple(warning, msg, origin);
    fa_log(NULL, err);
}

void fa_log_error_from(fa_string_t msg, fa_string_t origin)
{
    fa_error_t err = fa_error_create_simple(error, msg, origin);
    fa_log(NULL, err);
}

void fa_log_region_count(fa_string_t s)
{
  long count = gRegionCount;
  if (s) {
    fa_log_info(fa_string_dappend(s, fa_string_dappend(fa_string("  Regions allocated: "),
                                  fa_string_dshow(fa_i32(count)))));
  } else {
    fa_log_info(fa_string_dappend(fa_string("  Regions allocated: "), fa_string_dshow(fa_i32(count))));
  }
  //printf("Actually: %ld\n", gRegionCount);
}



