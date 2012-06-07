/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
 
#ifndef _SCLAUDIO_SCHEDULING_FUTURE
#define _SCLAUDIO_SCHEDULING_FUTURE

#ifdef __cplusplus
extern "C" {
#endif

/** 
    \ingroup sclaudio 
    \file 
  */

typedef void    *SclInterruptable;
typedef void    *SclFuture;
typedef void    *SclFutureGroup;
            
SCLAUDIO_API void scl_interrupt_future(SclFuture obj);

SCLAUDIO_API void scl_interrupt_future_group(SclFutureGroup obj);

SCLAUDIO_API SclFutureGroup scl_new_future_group(SclInterruptionMode mode);

SCLAUDIO_API SclInterruptionMode scl_future_group_interruption_mode(SclFutureGroup group);


#ifdef __cplusplus
}
#endif

#endif
