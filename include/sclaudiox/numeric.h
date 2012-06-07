/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
 
/**
    \file       numeric.h
    \brief      Redefines from sclaudio/numeric.h in the scorecleaner namespace.
 */

#ifndef  _SCLAUDIOX_NUMERIC
#define  _SCLAUDIOX_NUMERIC

#include "sclaudio/numeric.h"
                            
namespace doremir {
namespace scl {

typedef SclInt16   Int16;
typedef SclInt32   Int32;
typedef SclInt64   Int64;

typedef SclWord16  Word16;
typedef SclWord32  Word32;
typedef SclWord64  Word64;

typedef SclChar    Char;

typedef SclFloat32 Float32;
typedef SclFloat64 Float64;

} // namespace
} // namespace

#endif
