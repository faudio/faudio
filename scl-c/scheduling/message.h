/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
 
#ifndef _SCLAUDIO_SCHEDULING_MESSAGE
#define _SCLAUDIO_SCHEDULING_MESSAGE

#ifdef __cplusplus
extern "C" {
#endif

/** 
    \ingroup sclaudio 
    \file 
  */

typedef void    *SclSendOptions;
typedef void    *SclReceiveOptions;
typedef void    *SclMessageInfo;

typedef void    (*SclReceiver) (SclTime time, SclAtom* message, int len);
typedef void    (*SclSender)   (SclReceiver receiver, int status);

// =================================================================================================

SCLAUDIO_API SclScheduleOptions scl_default_send_options();

SCLAUDIO_API SclMessageKind     scl_send_options_get_kind(SclSendOptions obj);
SCLAUDIO_API SclAudioProcessor* scl_send_options_get_processors(SclSendOptions obj, int* len);
SCLAUDIO_API SclMidiDevice*     scl_send_options_get_devices(SclSendOptions obj, int* len);
SCLAUDIO_API int*               scl_send_options_get_channels(SclSendOptions obj, int* len);

SCLAUDIO_API void scl_send_options_set_kind(SclSendOptions obj, 
                                            SclMessageKind kind);

SCLAUDIO_API void scl_send_options_set_processors(SclSendOptions obj, 
                                                  SclAudioProcessor *procs, 
                                                  int len);

SCLAUDIO_API void scl_send_options_set_devices(SclSendOptions obj, 
                                               SclMidiDevice *devices, 
                                               int len);

SCLAUDIO_API void scl_send_options_set_channels(SclSendOptions obj, 
                                                int *channels, 
                                                int len);

// =================================================================================================

SCLAUDIO_API SclScheduleOptions scl_default_receive_options();

SCLAUDIO_API SclMessageKind     scl_receive_options_get_kind(SclReceiveOptions obj);
SCLAUDIO_API SclAudioProcessor* scl_receive_options_get_processors(SclReceiveOptions obj, int* len);
SCLAUDIO_API SclMidiDevice*     scl_receive_options_get_devices(SclReceiveOptions obj, int* len);
SCLAUDIO_API int*               scl_receive_options_get_channels(SclReceiveOptions obj, int* len);


SCLAUDIO_API void scl_receive_options_set_kind(SclReceiveOptions obj, 
                                               SclMessageKind kind);

SCLAUDIO_API void scl_receive_options_set_processors(SclReceiveOptions obj, 
                                                     SclAudioProcessor *procs, int len);

SCLAUDIO_API void scl_receive_options_set_devices(SclReceiveOptions obj, 
                                                  SclMidiDevice *devices, 
                                                  int len);

SCLAUDIO_API void scl_receive_options_set_channels(SclReceiveOptions obj, 
                                                   int *channels, 
                                                   int len);


// =================================================================================================

SCLAUDIO_API SclFuture scl_send_now(SclStream stream, 
                                    SclAtom *message, 
                                    int len, 
                                    SclSendOptions opts,
                                    SclStreamError* err);

SCLAUDIO_API SclFuture scl_send_later(SclStream stream, 
                                      SclTime time, 
                                      SclAtom *message, 
                                      int len, 
                                      SclSendOptions opts,
                                      SclStreamError* err);

SCLAUDIO_API SclFuture scl_send_at(SclStream stream, 
                                   SclTime time, 
                                   SclAtom *message, 
                                   int len, 
                                   SclSendOptions opts,
                                   SclStreamError* err);


SCLAUDIO_API SclFuture scl_receive(SclStream stream, 
                                   SclReceiver receiver, 
                                   SclReceiveOptions opts,
                                   SclStreamError* err);    

/*
SCLAUDIO_API SclFuture scl_receive_buffered(SclStream stream, 
                                            SclBufferedReceiver receiver, 
                                            SclReceiveOptions opts,
                                            SclStreamError* err);    
*/


#ifdef __cplusplus
}
#endif

#endif
