/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
 
#ifndef _SCLAUDIO_PROCESSOR
#define _SCLAUDIO_PROCESSOR

#ifdef __cplusplus
extern "C" {
#endif

/** 
    \ingroup sclaudio 
    \file 
  */

/**
    An object that transforms audio signals. May be atomic or created by combinators.    
 */
typedef void    *SclAudioProcessor;
typedef void    *SclAudioPlugin;

/** @depracated */
SCLAUDIO_API SclString* scl_processor_controls(SclAudioProcessor obj, int *length);

SCLAUDIO_API SclString  scl_processor_name(SclAudioProcessor obj);
SCLAUDIO_API int        scl_processor_is_atomic(SclAudioProcessor obj);
SCLAUDIO_API int        scl_processor_is_compound(SclAudioProcessor obj);
SCLAUDIO_API int        scl_processor_is_stateful(SclAudioProcessor obj);
SCLAUDIO_API int        scl_processor_is_plugin(SclAudioProcessor obj);
SCLAUDIO_API int        scl_processor_num_inputs(SclAudioProcessor obj);
SCLAUDIO_API int        scl_processor_num_outputs(SclAudioProcessor obj);
SCLAUDIO_API int        scl_processor_num_buses(SclAudioProcessor obj);

SCLAUDIO_API SclAudioProcessor scl_sequence(SclAudioProcessor *objs, int len, SclDspError *err);
SCLAUDIO_API SclAudioProcessor scl_parallel(SclAudioProcessor *objs, int len, SclDspError *err);


SCLAUDIO_API SclAudioProcessor  scl_load_fluidsynth(SclString path, SclDspError *err);

SCLAUDIO_API SclAudioPlugin     scl_plugin_from_error(SclAudioPluginError obj);
SCLAUDIO_API SclAudioPlugin     scl_processor_from_error(SclAudioPluginError obj);
SCLAUDIO_API SclAudioPlugin     scl_plugin_from_processor(SclAudioProcessor obj);
SCLAUDIO_API SclString          scl_plugin_name(SclAudioPlugin obj);
SCLAUDIO_API int                scl_plugin_num_inputs(SclAudioPlugin obj);
SCLAUDIO_API int                scl_plugin_num_outputs(SclAudioPlugin obj);
SCLAUDIO_API int                scl_plugin_num_buses(SclAudioPlugin obj);
SCLAUDIO_API SclAudioProcessor  scl_plugin_create_processor(SclAudioPlugin plugin, SclAudioPluginError* err);

SCLAUDIO_API SclAudioPlugin* scl_load_audio_units(int *len);
SCLAUDIO_API SclAudioPlugin  scl_load_dls_music_device();


#ifdef __cplusplus
}
#endif

#endif
