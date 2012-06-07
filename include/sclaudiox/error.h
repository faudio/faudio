/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef  _SCLAUDIOX_ERROR
#define  _SCLAUDIOX_ERROR

#include "sclaudiox/core.h"

namespace doremir {
namespace scl {


/**
    Generic error class, supporting the message method.
 */
class SCLAUDIO_API Error : public Resource
{
public:  
    virtual String message() const = 0;
};

/**
    Macro for declaring basic error types withoutout specific messages.
    
    The message() method returns the name of the condition.
 */
#define SCL_DECLARE_CONDITION(NAME)                                            \
    class SCLAUDIO_API NAME : public Error                                     \
    {                                                                          \
    public:                                                                    \
        String message() const                                                 \
        {                                                                      \
            return #NAME;                                                      \
        }                                                                      \
                                                                               \
        NAME(){}                                                               \
    }                                                                          \

/**
    Macro for declaring a basic errors with a string constructor.
    
    The message() method returns the name of the condition and a specific
    message.
 */
#define SCL_DECLARE_SIMPLE_ERROR(NAME)                                     \
    class SCLAUDIO_API NAME : public Error                                 \
    {                                                                      \
    public:                                                                \
        NAME(String msg)                                                   \
            : mMsg(msg) {}                                                 \
                                                                           \
        String message() const                                             \
        {                                                                  \
            return fromSimpleString<kDefaultCharSet>(#NAME) + ": " + mMsg; \
        }                                                                  \
                                                                           \
    private:                                                               \
        String mMsg;                                                       \
    }                                                                      \


/**
    Conditions that are not supposed to happen. Used as sanity checks for
    missing switch cases etc. 
 */
SCL_DECLARE_CONDITION(Impossible);

/**
    A limitation in the current implementation.
 */
SCL_DECLARE_CONDITION(Limitation);

/**
    A function or method missing in the current implementation. For partial
    implementations, use Limitation.
 */
SCL_DECLARE_CONDITION(Unimplemented);
            
/**
    A template without default initiation.
 */
SCL_DECLARE_CONDITION(NoDefaultInitiation);

/**
    An known but unusual condition that terminates the application.
 */
SCL_DECLARE_SIMPLE_ERROR(Unrecoverable);

} // namespace
} // namespace

#endif