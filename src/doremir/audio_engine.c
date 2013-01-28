
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/audio_engine.h>
#include <doremir/util.h>

#define iso8601_k "%Y-%m-%d %H:%M:%S%z  "

typedef doremir_audio_engine_log_func_t log_func_t;

static int            init_count_g  = 0;
static log_func_t     log_func_g    = NULL;
static ptr_t          log_ct_g      = NULL;

// --------------------------------------------------------------------------------

/*  Forward declare internal routines.
    One initializer and terminator for each module.
 */
void doremir_device_audio_initialize();
void doremir_device_audio_terminate();


// --------------------------------------------------------------------------------

/** Performs global initialization.

    This function must be called exactly once before any other function in the library.
    A call to doremir_audio_engine_terminate() will reset the global state so that
    doremir_audio_engine_initialize() may be called again and so on.
 */
void doremir_audio_engine_initialize()
{
  init_count_g++;
  doremir_device_audio_initialize();
  doremir_audio_engine_log_info(string("Finished initialization."));
}

/** Performs global cleanup.

    This function may be used to reset the global state as per above. It is not necessary to
    call this function before the program finishes.
 */
void doremir_audio_engine_terminate()
{
  init_count_g--;
  doremir_device_audio_terminate();
  doremir_audio_engine_log_info(string("Finished termination."));
}

// --------------------------------------------------------------------------------

/*
  TODO remove coloring if redirected

  Ex

  int is_redirected(){
     if (!isatty(fileno(stdout))){
         fprintf(stdout, "argv, argc, someone is redirecting me elsewhere...\n");
         return 1;
     }
     return 0;
  }
 */

static inline void stdlog(ptr_t ct, doremir_time_system_t t, doremir_error_t e)
{
  FILE *file = ct;
  char csmsg[350];
  doremir_let(tm, localtime((long *) &t)) {
    strftime(csmsg, 50, iso8601_k, tm);
  }
  doremir_with(str, doremir_string_show(e), doremir_destroy(str)) {
    doremir_with(cstr, doremir_string_to_utf8(str), free(cstr)) {
      strncat(csmsg, cstr, 298);
      strncat(csmsg, "\n", 1);
    }
  }
  fputs(csmsg, file);
  fflush(file);
}

/** Instruct the Audio Engine to write log messages to the specific file.
 */
void doremir_audio_engine_set_log_file(doremir_string_file_path_t path)
{
  char *cpath = doremir_string_to_utf8(path);
  log_ct_g = fopen(cpath, "a");
  log_func_g = stdlog;
  free(cpath);
}

/** Instruct the Audio Engine to write log messages to the standard output.
 */
void doremir_audio_engine_set_log_std()
{
  log_ct_g = stdout;
  log_func_g = stdlog;
}

/** Instruct the Audio Engine to pass log messages to the given handler.
 */
void doremir_audio_engine_set_log(doremir_audio_engine_log_func_t f, doremir_ptr_t ct)
{
  log_func_g = f;
  log_ct_g = ct;
}


// --------------------------------------------------------------------------------

/** Write a log message.
    @param ct   Context reference (ignored).
    @param e    A value implementing [Error](@ref doremir_error_interface_t).
 */
void doremir_audio_engine_log(doremir_ptr_t ct, doremir_error_t e)
{
  if (log_func_g) {
    log_func_g(log_ct_g, time(NULL), e);
  }
}

/** Write an informative message to the log.
 */
void doremir_audio_engine_log_info(doremir_string_t msg)
{
  doremir_audio_engine_log_info_from(msg, string(""));
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


