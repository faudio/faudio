/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef  _SCLAUDIOX_STREAM
#define  _SCLAUDIOX_STREAM
                              
#include "sclaudiox/processor.h"

#include "sclaudiox/scheduling/future.h"
#include "sclaudiox/scheduling/action.h"
#include "sclaudiox/scheduling/message.h"
#include "sclaudiox/scheduling/handler.h"


namespace doremir {
namespace scl {


SCL_DECLARE_SIMPLE_ERROR(StreamError);

SCL_DECLARE_SIMPLE_ERROR(InvalidStreamState);



// =============================================================================

enum StreamType 
{
    /** 
        Device streams contain at least one audio or midi device and runs in
        real-time, using external or internal synchronization. 
      */
    kDeviceStream = 0x0,

    /** 
        File streams operate on files and generally run as fast as the file 
        system allows. 
      */
    kFileStream = 0x1,

    /**
        Buffer streams operate on the internal memory and runs as fast as possible.
      */
    kBufferStream = 0x2,

    /**
        Indicates that the stream provides audio.
      */
    kAudioStream = 0x10,

    /**
        Indicates that the stream provides midi.
      */
    kMidiStream = 0x20
};



// =============================================================================

/**
    An abstract description of a stream.
 */
class SCLAUDIO_API StreamDescription : public NonCopyable
{
public:        
    /**
        Constructor.
     */
    StreamDescription(){}

    /**
        Destructor.
     */
    virtual ~StreamDescription(){}

    /**
        Returns a value representing the type of the stream.
     */
    virtual StreamType type() = 0;

    /**
        Whether the stream performs real-time computation.
     */
    virtual bool isRealtime() = 0;
    
    /**
        Whether the stream performs non-real-time computation.
        `isNonRealtime()` implies `!isRealtime()`.
     */
    virtual bool isNonRealtime()
    {
        return !isNonRealtime();
    }

    /**
        Whether the stream supports audio or not.
     */
    bool hasAudio()
    {
        return type() & kAudioStream;
    }

    /**
        Whether the stream supports MIDI or not.
     */
    bool hasMidi()
    {
        return type() & kMidiStream;
    }
};

// =============================================================================


/**
    A synchronous audio computation.

    Each stream contains a specific TimeProvider, which is used by schedulers to synchronize actions
    and messages. Each stream may contain an unspecified numbers of schedulers running in one or
    more threads.
 */
class SCLAUDIO_API Stream : public NonCopyable, public Resource 
{
public:      
    /**
        Constructor.
     */
    Stream(){}

    /**
        Destructor.
     */
    virtual ~Stream(){}

    /**
        Returns a description of this stream.
     */
    virtual StreamDescription* description() = 0;

    /**
        Returns true iff the stream is running.
     */
    virtual bool running() = 0;

    /** 
        Starts scheduling and audio computation.

        \throw PortaudioError
        \throw PortmidiError
        \throw DspError 
        \throw InvalidStreamState
            If the stream has been closed.
     */
    virtual void start() = 0;

    /** 
        Stops scheduling and audio computation.

        \throw PortaudioError
        \throw PortmidiError
        \throw DspError 
        \throw InvalidStreamState
            If the stream has been closed.
     */
    virtual void stop()  = 0;

    /** 
        Stops scheduling and audio computation.

        \throw PortaudioError
        \throw PortmidiError
        \throw DspError 
        \throw InvalidStreamState
            If the stream has been closed.
     */
    virtual void abort() = 0;
    
    /** 
        Returns a scheduler for actions. 
     */
    virtual ActionScheduler*  actionScheduler() = 0;

    /** 
        Returns a scheduler for messages. 
     */
    virtual MessageScheduler* messageScheduler() = 0;

    /** 
        Returns a midi-only scheduler for messages. 
        The messageKind option is ignored by such schedulers. 
     */
    virtual MessageScheduler* midiScheduler() = 0;

    /** 
        Returns an audio-only scheduler for messages.
        The messageKind option is ignored by such schedulers. 
     */
    virtual MessageScheduler* audioScheduler() = 0;

    /**
        Set a handler to be entered whenever the stream is stopped due
        to an error.

        TODO Change to addHandler() and removeHandler() using some kind of
             listener mixin
     */
    virtual void setHandler(Handler<Error>* handler) = 0;
};

// =============================================================================

} // namespace
} // namespace

#endif

