/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
 
/**
    \file       numeric.h
    \brief      Fixed sized numeric types.
 */

#ifndef _SCLAUDIO_NUMERIC
#define _SCLAUDIO_NUMERIC

#ifdef _WIN32
    #include "Windows.h"
#else
    #include <stdint.h>
#endif

#ifdef _WIN32

typedef SHORT     SclInt16;
typedef INT       SclInt32;
typedef LONGLONG  SclInt64;
typedef WORD      SclWord16;
typedef DWORD     SclWord32;
typedef DWORDLONG SclWord64;

#ifdef SCL_UNICODE
    typedef unsigned short SclChar;
#else
    typedef char SclChar;
#endif

typedef float  SclFloat32;
typedef double SclFloat64;    

#else // WIN32

typedef int16_t SclInt16;
typedef int32_t SclInt32;
typedef int64_t SclInt64;
typedef uint16_t SclWord16;
typedef uint32_t SclWord32;
typedef uint64_t SclWord64;

#ifdef SCL_UNICODE
    typedef unsigned short SclChar;
#else
    typedef char           SclChar;
#endif

typedef float               SclFloat32;
typedef double              SclFloat64;
                      
#endif // WIN32

#endif
