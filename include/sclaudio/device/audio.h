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

typedef void  *SclAudioHost;
typedef void  *SclAudioDevice;

/**
    Returns name of this host.
 */
SCLAUDIO_API SclString scl_audio_host_name(SclAudioHost obj);

/**
    Returns the number of devices currently available on this host.
 */
SCLAUDIO_API int scl_audio_host_number_of_devices(SclAudioHost obj);

/**
    Returns a list of all audio devices currently available on this host.
 */
SCLAUDIO_API SclAudioDevice* scl_audio_host_devices(SclAudioHost obj, int *len);

/** 
    Returns a list of all audio hosts currently available.
    \throw PortaudioError 
  */
SCLAUDIO_API SclAudioHost* scl_audio_hosts(int *len, SclPortaudioError *err);

/** 
    Returns the default audio host.
    \throw PortaudioError (?)
  */
SCLAUDIO_API SclAudioHost scl_default_audio_host(SclPortaudioError *err);



/**
    Returns the name of this device.
 */
SCLAUDIO_API SclString scl_audio_device_name(SclAudioDevice obj);

/**
    Returns the host to which this device belongs.
 */
SCLAUDIO_API SclAudioHost scl_audio_device_host(SclAudioDevice obj);

/**
    Returns the number of input channels supported by this device.
 */
SCLAUDIO_API int scl_audio_device_max_input_channels(SclAudioDevice obj);

/**
    Returns the number of output channels supported by this device.
 */
SCLAUDIO_API int scl_audio_device_max_output_channels(SclAudioDevice obj);

/**
    Returns the lowest amount of input latency supported by this device.
 */
SCLAUDIO_API int scl_audio_device_default_low_input_latency(SclAudioDevice obj);

/**
    Returns the highest amount of input latency supported by this device.
 */
SCLAUDIO_API int scl_audio_device_default_high_input_latency(SclAudioDevice obj);

/**
    Returns the lowest amount of output latency supported by this device.
 */
SCLAUDIO_API int scl_audio_device_default_low_output_latency(SclAudioDevice obj);

/**
    Returns the highest amount of output latency supported by this device.
 */
SCLAUDIO_API int scl_audio_device_default_high_output_latency(SclAudioDevice obj);

/**
    Returns the default sample rate for this device.
 */
SCLAUDIO_API int scl_audio_device_default_sample_rate(SclAudioDevice obj);



/**
    Returns the default audio input device.
 */
SCLAUDIO_API SclAudioDevice  scl_default_audio_input_device(SclPortaudioError *err);

/**
    Returns the default audio output device.
 */
SCLAUDIO_API SclAudioDevice  scl_default_audio_output_device(SclPortaudioError *err);



#ifdef __cplusplus
}
#endif

#endif
