/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
 
#ifndef _SCLAUDIO_MEMORY
#define _SCLAUDIO_MEMORY

#ifdef __cplusplus
extern "C" {
#endif

/** 
    \ingroup sclaudio 
    \file 
  */

SCLAUDIO_API void  scl_free_error(SclError obj);
SCLAUDIO_API void  scl_free_portaudio_error(SclPortaudioError obj);
SCLAUDIO_API void  scl_free_portmidi_error(SclPortmidiError obj);

SCLAUDIO_API void  scl_free_audio_host(SclAudioHost obj);
SCLAUDIO_API void  scl_free_audio_device(SclAudioDevice obj);
SCLAUDIO_API void  scl_free_midi_device(SclMidiDevice obj);
SCLAUDIO_API void  scl_free_future(SclFuture obj);
SCLAUDIO_API void  scl_free_stream(SclStream obj);
SCLAUDIO_API void  scl_free_processor(SclAudioProcessor obj);
SCLAUDIO_API void  scl_free_future_group(SclFutureGroup obj);

SCLAUDIO_API void  scl_free_atom(SclAtom obj);
SCLAUDIO_API void  scl_free_schedule_options(SclScheduleOptions opts);
SCLAUDIO_API void  scl_free_send_options(SclSendOptions opts);
SCLAUDIO_API void  scl_free_receive_options(SclReceiveOptions opts);

SCLAUDIO_API void  scl_free_array(void** arr);

#ifdef __cplusplus
}
#endif

#endif
