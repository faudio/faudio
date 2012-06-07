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


SCLAUDIO_API SclString*        scl_processor_controls(SclAudioProcessor obj, int *length);
SCLAUDIO_API int               scl_processor_num_inputs(SclAudioProcessor obj);
SCLAUDIO_API int               scl_processor_num_outputs(SclAudioProcessor obj);

SCLAUDIO_API SclAudioProcessor scl_sequence(SclAudioProcessor *objs, int len, SclDspError *err);
SCLAUDIO_API SclAudioProcessor scl_parallel(SclAudioProcessor *objs, int len, SclDspError *err);

SCLAUDIO_API SclAudioProcessor scl_load_fluidsynth(SclString path, SclDspError *err);


#ifdef __cplusplus
}
#endif

#endif
