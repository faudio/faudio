/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
 
#ifndef _SCLAUDIO_ATOM
#define _SCLAUDIO_ATOM

#ifdef __cplusplus
extern "C" {
#endif

/** 
    \ingroup sclaudio 
    \file 
  */

typedef void *SclAtom;


SCLAUDIO_API SclAtomType     scl_atom_type(SclAtom atom);

SCLAUDIO_API int             scl_atom_to_int(SclAtom atom);
SCLAUDIO_API double          scl_atom_to_double(SclAtom atom);
SCLAUDIO_API SclString       scl_atom_to_string(SclAtom atom);

SCLAUDIO_API SclAtom         scl_atom_from_int(int value);
SCLAUDIO_API SclAtom         scl_atom_from_double(double value);
SCLAUDIO_API SclAtom         scl_atom_from_string(SclString value);


#ifdef __cplusplus
}
#endif

#endif
