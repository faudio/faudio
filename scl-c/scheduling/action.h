/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
 
#ifndef _SCLAUDIO_SCHEDULING_ACTION
#define _SCLAUDIO_SCHEDULING_ACTION

#ifdef __cplusplus
extern "C" {
#endif

/** 
    \ingroup sclaudio 
    \file 
  */

typedef void    *SclScheduleOptions;

typedef void    (*SclAction) (SclTime time);


SCLAUDIO_API SclScheduleOptions scl_default_schedule_options();

SCLAUDIO_API SclTimeUnit        scl_schedule_options_get_unit(SclScheduleOptions obj);
SCLAUDIO_API SclFutureGroup*    scl_schedule_options_get_groups(SclScheduleOptions obj, int* len);
SCLAUDIO_API int                scl_schedule_options_get_repeats(SclScheduleOptions obj);
SCLAUDIO_API SclTime            scl_schedule_options_get_interval(SclScheduleOptions obj);

SCLAUDIO_API void scl_schedule_options_set_unit(SclScheduleOptions obj, 
                                                SclTimeUnit unit);

SCLAUDIO_API void scl_schedule_options_set_groups(SclScheduleOptions obj, 
                                                  SclFutureGroup *groups, 
                                                  int len);

SCLAUDIO_API void scl_schedule_options_set_repeats(SclScheduleOptions obj, 
                                                   int repeats);

SCLAUDIO_API void scl_schedule_options_set_interval(SclScheduleOptions obj, 
                                                    SclTime interval);


// =================================================================================================

SCLAUDIO_API SclFuture scl_schedule_now(SclStream stream, 
                                        SclAction action, 
                                        SclScheduleOptions opts,
                                        SclStreamError* err);

SCLAUDIO_API SclFuture scl_schedule_later(SclStream stream, 
                                          SclTime time, 
                                          SclAction action, 
                                          SclScheduleOptions opts,
                                          SclStreamError* err);

SCLAUDIO_API SclFuture scl_schedule_at(SclStream stream, 
                                       SclTime time, 
                                       SclAction action, 
                                       SclScheduleOptions opts,
                                       SclStreamError* err);


#ifdef __cplusplus
}
#endif

#endif
