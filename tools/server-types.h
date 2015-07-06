#include "fa/fa.h"

#ifndef __SERVER_TYPES
#define __SERVER_TYPES

typedef enum {
	AUDIO_DEVICE = 1,
  MIDI_DEVICE  = 2
} fa_device_type_t;

typedef struct {
  fa_device_type_t device_type;
  fa_string_t host;
  fa_string_t name;
} *fa_device_descriptor_t;


typedef enum {
  FA_ECHO_NO_ECHO = 0,
  FA_ECHO_AUDIO = 1,
  FA_ECHO_DEVICE = 2
} fa_echo_type_t;


#endif
