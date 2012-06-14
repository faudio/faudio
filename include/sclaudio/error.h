/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
 
#ifndef _SCLAUDIO_ERROR
#define _SCLAUDIO_ERROR

#ifdef __cplusplus
extern "C" {
#endif

/** 
    \ingroup sclaudio 
    \file 
  */

/**
    Generic error type, supporting the message method.
 */
typedef void  *SclError;

typedef void  *SclPortaudioError;
typedef void  *SclPortmidiError;

typedef void  *SclDspError;
typedef void  *SclAudioPluginError;
typedef void  *SclStreamError;

typedef void  (*SclErrorHandler) (SclTime time, SclError err);


SCLAUDIO_API SclString  scl_error_message(SclError obj);
SCLAUDIO_API int        scl_portaudio_error_code(SclPortaudioError obj);
SCLAUDIO_API int        scl_portmidi_error_code(SclPortmidiError obj);


#ifdef __cplusplus
}
#endif

#endif
