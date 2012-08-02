/*
    ScoreCleaner Audio Engine

    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIO_DEVICE_MIDI
#define _SCLAUDIO_DEVICE_MIDI

#ifdef __cplusplus
extern "C" {
#endif

  /**
      \ingroup sclaudio
      \file
    */

  typedef void*    SclMidiDevice;

  SCLAUDIO_API SclString       scl_midi_device_name(SclMidiDevice obj);
  SCLAUDIO_API SclString       scl_midi_device_host_name(SclMidiDevice obj);
  SCLAUDIO_API int             scl_midi_device_has_input(SclMidiDevice obj);
  SCLAUDIO_API int             scl_midi_device_has_output(SclMidiDevice obj);

  SCLAUDIO_API SclMidiDevice   scl_default_midi_input_device(SclPortmidiError* err);
  SCLAUDIO_API SclMidiDevice   scl_default_midi_output_device(SclPortmidiError* err);
  SCLAUDIO_API SclMidiDevice*  scl_midi_devices(int* length, SclPortmidiError* err);


#ifdef __cplusplus
}
#endif

#endif