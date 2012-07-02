/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_SCHEDULING_DISPATCHING
#define _SCLAUDIOX_SCHEDULING_DISPATCHING

#include "sclaudiox/stream.h"
#include "sclaudiox/scheduling/scheduler.h"

namespace doremir {
namespace scl {

/**
    An implementation of MessageScheduler that dispatches to the audio/midi schedulers
    of the underlying stream, based on the MessageOptions.kind attribute.
 */
class SCLAUDIO_API DispatchingScheduler : public MessageScheduler
{
public:    
    DispatchingScheduler(Stream* stream) 
        : stream(stream) {}
            
    Future* schedule(Future* future)
    {
        // nothing
		return NULL;
    }             
    
    void execute(Time time)
    {
        // nothing        
    }
    
    Future* sendNow(std::list<Message> messages, 
                    SendOptions options)
    {
        switch(options.kind)
        {
            case kMidiMessage: 
                return stream->midiScheduler()->sendNow(messages, options);
            case kAudioMessage: 
                return stream->audioScheduler()->sendNow(messages, options);
            default: 
                throw Impossible();
        }
    }
    
    Future* sendLater(Time time, 
                      std::list<Message> messages, 
                      SendOptions options)
    {
        switch(options.kind)
        {
            case kMidiMessage: 
                return stream->midiScheduler()->sendLater(time, messages, options);
            case kAudioMessage:
                return stream->audioScheduler()->sendLater(time, messages, options);
            default:
                throw Impossible();
        }
    }
    
    Future* sendAt(Time time, 
                   std::list<Message> messages, 
                   SendOptions options)
    {
        switch(options.kind)
        {
            case kMidiMessage:
                return stream->midiScheduler()->sendAt(time, messages, options);
            case kAudioMessage:
                return stream->audioScheduler()->sendAt(time, messages, options);
            default:
                throw Impossible();
        }
    }

    Future* receive(Receiver* receiver, 
                    ReceiveOptions options)
    {
        switch(options.kind)
        {
            case kMidiMessage:
                return stream->midiScheduler()->receive(receiver, options);
            case kAudioMessage:
                return stream->audioScheduler()->receive(receiver, options);
            default:
                throw Impossible();
        }
    }

private:
    Stream* stream;
};  

} // namespace
} // namespace

#endif
