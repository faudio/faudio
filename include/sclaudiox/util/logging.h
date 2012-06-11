/*
    ScoreCleaner Audio Engine

    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_UTIL_LOGGING
#define _SCLAUDIOX_UTIL_LOGGING

#include <iostream>
#include <fstream>
#include "sclaudiox/util/concurrency.h"

/**
    The name of the audio engine log file.
 */
#ifndef SCL_LOG_FILE_NAME
    #define SCL_LOG_FILE_NAME "ScoreCleanerAudio.log"
#endif


#ifdef SCL_LOG
    #define SCL_WRITE_LOG(EXPR)                 \
        {                                       \
            ReentrantLock lock (printMutex());  \
            initLog();                          \
            logStream() << EXPR;                \
            logStream().flush();                \
        }                                       \

    #define SCL_WRITE_LOG_IF(COND, EXPR)        \
        if (COND)                               \
            SCL_WRITE_LOG(EXPR);                \

    #define SCL_WRITE_LOG_IFN(COND, EXPR)       \
        if (!COND)                              \
            SCL_WRITE_LOG(EXPR);                \

#else // SCL_LOG
    #define SCL_WRITE_LOG(EXPR)
    #define SCL_WRITE_LOG_IF(COND, EXPR)
    #define SCL_WRITE_LOG_IFN(COND, EXPR)

#endif // SCL_LOG


namespace doremir {
namespace scl {


inline String dirSep()
{
#ifdef SCL_WIN
    return toString("\\");
#else
    return toString("/");
#endif
}

inline String tempDir()
{
#ifdef SCL_WIN
	return toString(getenv("TEMP"));
#else
    return toString("/tmp");
#endif
}

inline String homeDir()
{
#ifdef SCL_WIN
    return toString(getenv("HOMEPATH"));
#else
	return toString(getenv("HOME"));
#endif
}

inline String logPath()
{
#ifdef SCL_OSX
    return homeDir() + dirSep() + "Library" + dirSep() + "Logs" + dirSep() + "ScoreCleaner" + dirSep() + SCL_LOG_FILE_NAME;
#else
    return tempDir() + dirSep() + toString(SCL_LOG_FILE_NAME);
#endif                                                            
}

inline std::ostream& logStream()
{
#ifdef SCL_LOG_TO_STDOUT
    return std::cout;
#else
    static std::ofstream *log = new std::ofstream(toSimpleString(logPath()));
    return *log;
#endif
}       

static bool initLogDone = false;
inline void initLog()
{    
    if (!initLogDone)
    {
        logStream() << "ScoreCleaner Audio Engine v" << SCL_VERSION << "\n";
        initLogDone = true;
    }
}

class DefaultLogTraits
{
    static void log(String str)
    {
        // ReentrantLock lock (printMutex());
        logStream() << str;
        logStream().flush();
    }
};

template <class LogTraits = DefaultLogTraits> 
class Logger
{
    void log(String str)
    {
        LogTraits::log(str);
    }
};



} // namespace
} // namespace

#endif
