/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
 
#ifndef _SCLAUDIO_STREAM_DEVICE
#define _SCLAUDIO_STREAM_DEVICE

#ifdef __cplusplus
extern "C" {
#endif

/** 
    \ingroup sclaudio 
    \file 
  */

typedef void *SclDeviceStreamOptions;

SCLAUDIO_API SclDeviceStreamOptions scl_default_device_stream_options();

SCLAUDIO_API int scl_device_stream_options_get_sample_rate(SclDeviceStreamOptions obj);
SCLAUDIO_API int scl_device_stream_options_get_audio_buffer_size(SclDeviceStreamOptions obj);
SCLAUDIO_API SclRealTime scl_device_stream_options_get_audio_latency(SclDeviceStreamOptions obj);
SCLAUDIO_API SclRealTime scl_device_stream_options_get_midi_latency(SclDeviceStreamOptions obj);
SCLAUDIO_API int scl_device_stream_options_is_non_blocking(SclDeviceStreamOptions obj);
SCLAUDIO_API int scl_device_stream_options_is_exclusive_mode(SclDeviceStreamOptions obj);

SCLAUDIO_API void scl_device_stream_options_set_sample_rate(SclDeviceStreamOptions obj, int value);
SCLAUDIO_API void scl_device_stream_options_set_audio_buffer_size(SclDeviceStreamOptions obj, int value);
SCLAUDIO_API void scl_device_stream_options_set_audio_latency(SclDeviceStreamOptions obj, SclRealTime value);
SCLAUDIO_API void scl_device_stream_options_set_midi_latency(SclDeviceStreamOptions obj, SclRealTime value);
SCLAUDIO_API SclRealTime scl_device_stream_options_set_non_blocking(SclDeviceStreamOptions obj, int value);
SCLAUDIO_API SclRealTime scl_device_stream_options_set_exclusive_mode(SclDeviceStreamOptions obj, int value);


// =================================================================================================

SCLAUDIO_API SclStream scl_open_device_stream ( SclMidiDevice midi_in,
                                                SclMidiDevice midi_out,
                                              
                                                SclAudioDevice audio_in,
                                                SclAudioDevice audio_out,
                                                SclAudioProcessor processor,
                                                
                                                SclDeviceStreamOptions options,
                                                
                                                SclPortmidiError* pm_err,
                                                SclPortaudioError* pa_err,
                                                SclDspError* dsp_err );                       


#ifdef __cplusplus
}
#endif

#endif
