/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
 
#ifndef _SCLAUDIO_ENUM
#define _SCLAUDIO_ENUM

#ifdef __cplusplus
extern "C" {
#endif

/** 
    \ingroup sclaudio 
    \file 
  */

typedef int  SclAtomType;
typedef int  SclMessageKind;
typedef int  SclTimeUnit;
typedef int  SclInterruptionMode;
typedef int  SclStreamType;    


SCLAUDIO_API SclMessageKind      scl_message_type_audio();
SCLAUDIO_API SclMessageKind      scl_message_type_midi();

SCLAUDIO_API SclTimeUnit         scl_time_unit_samples();
SCLAUDIO_API SclTimeUnit         scl_time_unit_milliseconds();

SCLAUDIO_API SclAtomType         scl_atom_string();
SCLAUDIO_API SclAtomType         scl_atom_int();
SCLAUDIO_API SclAtomType         scl_atom_double();

SCLAUDIO_API SclInterruptionMode scl_interruption_mode_simple();
SCLAUDIO_API SclInterruptionMode scl_interruption_mode_forcing();
SCLAUDIO_API SclInterruptionMode scl_interruption_mode_transactional();


#ifdef __cplusplus
}
#endif

#endif
