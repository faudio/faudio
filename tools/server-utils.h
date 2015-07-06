#include "fa/fa.h"
#include "server-types.h"
#include "server-globals.h"

// int lo_message_add_varargs_internal(lo_message m, const char *types,
//                                     va_list ap, const char *file,
//                                     int line); // message.c
//
// int respond(lo_message message, lo_server server, const char *path, const char *types, ...)
// {
//   lo_address address = lo_message_get_source(message);
//   //const char *host = lo_address_get_hostname(address);
//   //const char *port = lo_address_get_port(address);
//
//   if (!address) {
//       printf("Couldn't get message source!\n");
//       return 0;
//   }
//
//   va_list ap;
//   va_start(ap, types);
//   int r = lo_send_from_varargs_internal(address, server, LO_TT_IMMEDIATE, path, types, ap);
//   if (r < 0)
//       printf("Error sending back message, socket may have closed.\n");
//   //else
//   //    printf("Sent %d bytes to %s:%s.\n", r, host, port);
//
//   return r;
// }

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

void schedule_to_midi_echo_stream(fa_time_t time, fa_action_t action)
{
  switch (selected_midi_echo) {
    case FA_ECHO_AUDIO:
      if (current_audio_stream) fa_audio_schedule(time, action, current_audio_stream);
      break;
    case FA_ECHO_DEVICE:
      printf("FA_ECHO_DEVICE not implemented\n");
      break;
    case FA_ECHO_NO_ECHO:
      break;
  }
}

void schedule_to_midi_echo_stream_relative(fa_time_t time, fa_action_t action)
{
  switch (selected_midi_echo) {
    case FA_ECHO_AUDIO:
      if (current_audio_stream) fa_audio_schedule_relative(time, action, current_audio_stream);
      break;
    case FA_ECHO_DEVICE:
      printf("FA_ECHO_DEVICE not implemented\n");
      break;
    case FA_ECHO_NO_ECHO:
      break;
  }
}
