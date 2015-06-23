/*
*  Copyright (C) 2015 Erik Ronström
*
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "lo/lo.h"

#include <fa/fa.h>
#include <fa/util.h>
#include <fa/dynamic.h>
#include "common.h"

#include "server-types.h"
#include "server-globals.h"
#include "server-utils.h"

#define respond(m, s, ...) lo_send_from(lo_message_get_source(m), (lo_server)s, LO_TT_IMMEDIATE, __VA_ARGS__)

int done = 0;

fa_string_t synth_name = NULL;


void liblo_error(int num, const char *m, const char *path);

int generic_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data);
int foo_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data);
int quit_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data);
int midi_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data);
int time_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data);

int main()
{
  const char *port = "7770";
  
  /* start a new server on port 7770 */
  lo_server_thread st = lo_server_thread_new_with_proto(port, LO_TCP, liblo_error);
  lo_server s = lo_server_thread_get_server(st);

  /* add method that will match any path and args */
  //lo_server_thread_add_method(st, NULL, NULL, generic_handler, NULL);

  /* add method that will match the path /foo/bar, with two numbers, coerced
  * to float and int */
  lo_server_thread_add_method(st, "/foo/bar", "fi", foo_handler, NULL);

  /* add method that will match the path /quit with no args */
  lo_server_thread_add_method(st, "/quit", "", quit_handler, NULL);

  /* add method that will match the path /midi with no args */
  lo_server_thread_add_method(st, "/midi", "", midi_handler, s);
  
  /* More methods */
  lo_server_thread_add_method(st, "/time", "", time_handler, s);


  fa_with_faudio() {

#ifdef _WIN32
    synth_name   = fa_string("fluid");
    fa_pair_t synth = fa_signal_synth(synth_name, fa_string("C:\\sf.sf2"));
#else
    synth_name   = fa_string("dls");
    fa_pair_t synth = fa_signal_dls(synth_name);
#endif

    fa_list_t out = fa_pair_to_list(synth);
    current_audio_session = fa_audio_begin_session();
    current_audio_output_device = fa_audio_default_output(current_audio_session);
    current_audio_stream = fa_audio_open_stream(NULL, current_audio_output_device, just, out);
    current_clock = fa_audio_get_clock(current_audio_stream);

    if (fa_check(current_audio_stream)) {
      fa_error_log(current_audio_stream, NULL);
    }


    lo_server_thread_start(st);
    
    printf("Listening on TCP port %s\n", port);

    while (!done) {
#ifdef WIN32
      Sleep(1);
#else
      usleep(1000);
#endif
    }
    
    fa_audio_close_stream(current_audio_stream);
    fa_audio_end_session(current_audio_session);
      
  }

  lo_server_thread_free(st);

  return 0;
}

void liblo_error(int num, const char *msg, const char *path)
{
  printf("liblo server error %d in path %s: %s\n", num, path, msg);
  fflush(stdout);
}

/* catch any incoming messages and display them. returning 1 means that the
* message has not been fully handled and the server should try other methods */
int generic_handler(const char *path, const char *types, lo_arg ** argv,
int argc, void *data, void *user_data)
{
  int i;

  printf("path: <%s>\n", path);
  for (i = 0; i < argc; i++) {
    printf("arg %d '%c' ", i, types[i]);
    lo_arg_pp((lo_type)types[i], argv[i]);
    printf("\n");
  }
  printf("\n");
  fflush(stdout);

  return 1;
}

int foo_handler(const char *path, const char *types, lo_arg ** argv,
int argc, void *data, void *user_data)
{
  /* example showing pulling the argument values out of the argv array */
  printf("%s <- f:%f, i:%d\n\n", path, argv[0]->f, argv[1]->i);
  fflush(stdout);

  return 0;
}

int quit_handler(const char *path, const char *types, lo_arg ** argv,
int argc, void *data, void *user_data)
{
  done = 1;
  printf("quiting\n\n");
  fflush(stdout);

  return 0;
}


int midi_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data)
{
  
    lo_message message = (lo_message)data;
    lo_address address = lo_message_get_source(message);
    lo_server server = (lo_server)user_data;
    const char *host = lo_address_get_hostname(address);
    const char *port = lo_address_get_port(address);

    //printf("in midi handler\n");

    if (!address) {
        printf("Couldn't get message source, quitting.\n");
        done = 1;
        return 0;
    }

    int r = lo_send_from(address, server, LO_TT_IMMEDIATE, "/answer", "i", 1);
    if (r < 0)
        printf("Error sending back message, socket may have closed.\n");
    else
        printf("Sent %d bytes to %s:%s.\n", r, host, port);
    
  
  fa_action_t chord = fa_action_many(list(
    fa_pair_create(
        fa_action_send(synth_name, fa_midi_message_create_simple(0x90, 64 + ((5 % 12) * 3), 90)), fa_hms(0, 0, 1)
    ),
    fa_pair_create(
      fa_action_send(synth_name, fa_midi_message_create_simple(0x90, 61 + ((5 % 12) * 3), 90)), fa_hms(0, 0, 1)
    )
  ));
  // printf("System time (early): %lld\n", fa_clock_milliseconds(fa_clock_standard()));
  fa_audio_schedule_relative(fa_hms(0, 0, 0), chord, current_audio_stream);
  
  return 0;
}

int time_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
  if (current_clock)
    respond(message, user_data, "/time", "h", fa_clock_milliseconds(current_clock));
  else
    respond(message, user_data, "/time", "N");
  return 0;
}

