
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/audio_engine.h>
#include <doremir/util.h>

typedef doremir_audio_engine_log_func_t log_func_t;

/*  Forward declare internal routines.
    One initializer and terminator for each module.
 */
void doremir_device_audio_initialize();
void doremir_device_audio_terminate();


static int         count    = 0;
static log_func_t  log_func = NULL;
static ptr_t       log_data = NULL;

/** Performs global initialization.

    This function must be called exactly once before any other function in the library.
    A call to doremir_audio_engine_terminate() will reset the global state so that
    doremir_audio_engine_initialize() may be called again and so on.
 */
void doremir_audio_engine_initialize()
{
  count++;
  doremir_device_audio_initialize();
}

/** Performs global cleanup.

    This function may be used to reset the global state as per above. It is not necessary to
    call this function before the program finishes.
 */
void doremir_audio_engine_terminate()
{
  count--;
  doremir_device_audio_terminate();
}

static inline
void write_log(ptr_t ct, doremir_time_system_t t, doremir_error_t e)
{                               
  FILE *file = ct;
  char csmsg[350];
  
  {    
    // use gmtime?
    struct tm *tm = localtime((long*) &t);
    strftime(csmsg, 50, "%Y-%m-%d %H:%M:%S%z  ", tm);
  }              
  {
    // set csmsg
    doremir_string_t str = doremir_string_show(e);
    char *cstr = doremir_string_to_utf8(str);
    strncat(csmsg, cstr, 298);
    strncat(csmsg, "\n", 1);
    free(cstr);
    doremir_destroy(str);
  }        
  fputs(csmsg, file);
  fflush(file);
}

/** Instruct the Audio Engine to write log messages to the specific file.
 */
void doremir_audio_engine_set_log_file(doremir_string_file_path_t path)
{                        
  char* cpath = doremir_string_to_utf8(path);
  log_data = fopen(cpath, "a");
  log_func = write_log;
  free(cpath);
}

/** Instruct the Audio Engine to write log messages to the standard output.
 */
void doremir_audio_engine_set_log_std()
{
  log_data = stdout;
  log_func = write_log;
}

/** Instruct the Audio Engine to pass log messages to the given handler.
 */
void doremir_audio_engine_set_log(doremir_audio_engine_log_func_t f, doremir_ptr_t ct)
{
  log_func = f;
  log_data = ct;
}

/** Write a log message.
 */
void doremir_audio_engine_log(doremir_ptr_t ct, doremir_ptr_t e)
{     
  if (log_func)
    log_func(log_data, time(NULL), e);
}

