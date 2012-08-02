/*
    ScoreCleaner Audio Engine

    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIO_DEVICE_AUDIO
#define _SCLAUDIO_DEVICE_AUDIO

#ifdef __cplusplus
extern "C" {
#endif

  /**
      \ingroup sclaudio
      \file
    */

  typedef void*  SclAudioHost;
  typedef void*  SclAudioDevice;

  SCLAUDIO_API SclString scl_audio_host_name(SclAudioHost obj);
  SCLAUDIO_API int scl_audio_host_number_of_devices(SclAudioHost obj);
  SCLAUDIO_API SclAudioDevice* scl_audio_host_devices(SclAudioHost obj, int* len);
  SCLAUDIO_API SclAudioHost* scl_audio_hosts(int* len, SclPortaudioError* err);
  SCLAUDIO_API SclAudioHost scl_default_audio_host(SclPortaudioError* err);

  SCLAUDIO_API SclString scl_audio_device_name(SclAudioDevice obj);
  SCLAUDIO_API SclAudioHost scl_audio_device_host(SclAudioDevice obj);
  SCLAUDIO_API int scl_audio_device_max_input_channels(SclAudioDevice obj);
  SCLAUDIO_API int scl_audio_device_max_output_channels(SclAudioDevice obj);
  SCLAUDIO_API int scl_audio_device_default_low_input_latency(SclAudioDevice obj);
  SCLAUDIO_API int scl_audio_device_default_high_input_latency(SclAudioDevice obj);
  SCLAUDIO_API int scl_audio_device_default_low_output_latency(SclAudioDevice obj);
  SCLAUDIO_API int scl_audio_device_default_high_output_latency(SclAudioDevice obj);
  SCLAUDIO_API int scl_audio_device_default_sample_rate(SclAudioDevice obj);

  SCLAUDIO_API SclAudioDevice  scl_default_audio_input_device(SclPortaudioError* err);
  SCLAUDIO_API SclAudioDevice  scl_default_audio_output_device(SclPortaudioError* err);


#ifdef __cplusplus
}
#endif

#endif
