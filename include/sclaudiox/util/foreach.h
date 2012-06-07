/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_UTIL_FOREACH
#define _SCLAUDIOX_UTIL_FOREACH

#include <boost/foreach.hpp>

#ifdef foreach
    #error "Previous definition of 'foreach'"
#endif

#ifdef reverse_foreach
    #error "Previous definition of 'reverse_foreach'"
#endif

#define foreach         BOOST_FOREACH
#define reverse_foreach BOOST_REVERSE_FOREACH

#endif