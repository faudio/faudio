/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
 
#ifndef _SCLAUDIO_STREAM
#define _SCLAUDIO_STREAM

#ifdef __cplusplus
extern "C" {
#endif

/** 
    \ingroup sclaudio 
    \file 
  */

typedef void    *SclStream;

SCLAUDIO_API int scl_stream_sample_rate(SclStream stream);

SCLAUDIO_API int scl_stream_audio_buffer_size(SclStream stream);

SCLAUDIO_API int scl_stream_running(SclStream stream);

SCLAUDIO_API void scl_stream_start(SclStream stream, 
                                   SclPortaudioError *err1, 
                                   SclPortmidiError* err2, 
                                   SclDspError* err3);

SCLAUDIO_API void scl_stream_stop(SclStream stream, 
                                  SclPortaudioError *err1, 
                                  SclPortmidiError* err2, 
                                  SclDspError* err3);

SCLAUDIO_API void scl_stream_abort(SclStream stream, 
                                   SclPortaudioError *err1, 
                                   SclPortmidiError* err2, 
                                   SclDspError* err3);
                                   
SCLAUDIO_API void scl_stream_set_error_handler(SclStream stream, SclErrorHandler handler);
                                   

#ifdef __cplusplus
}
#endif

#endif
