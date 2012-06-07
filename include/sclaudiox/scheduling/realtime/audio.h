/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_SCHEDULING_REALTIME_AUDIO
#define _SCLAUDIOX_SCHEDULING_REALTIME_AUDIO

#include <boost/tuple/tuple.hpp>

#include "sclaudiox/scheduling/message.h"
#include "sclaudiox/scheduling/realtime/scheduler.h"

#include "sclaudiox/util/queue/atomic_single.h"

namespace doremir {
namespace scl {
    

class SCLAUDIO_API SendAudio : public SendFuture
{
public:
    SendAudio( Queue< boost::tuple< AudioProcessor*, std::list<Message> >* >* queue,
               AudioProcessor*      receiver,
               std::list<Message>   messages,
               Time                 time,
               SendOptions          options )
        : SendFuture(messages, time, options)
        , queue(queue)
        , receiver(receiver) {}
        
    void perform(Time now)
    {
        queue->insert(new boost::tuple< AudioProcessor*, std::list<Message> >(receiver, messages));
    }

private:
    Queue< boost::tuple< AudioProcessor*, std::list<Message> >* >* queue;
    AudioProcessor*                                        receiver;
};


/**
    Implementation of MessageScheduler used to schedule messages to audio processors.
    
        TODO can only send to top-level audio processor
 */
class SCLAUDIO_API AudioScheduler : public MessageScheduler, 
                                    public RealtimeScheduler
{
public:
    AudioScheduler(TimeProvider* timeSource,
                   AudioProcessor* rootProcessor,
                   Queue< boost::tuple< AudioProcessor*, std::list<Message> >* >* queue) 
        : RealtimeScheduler(timeSource)
        , timeSource(timeSource)
        , rootProcessor(rootProcessor)
        , queue(queue) {}

    Future* sendNow(std::list<Message> messages, 
                    SendOptions options)
    {
        Time now = timeSource->millisecondTime();
        return schedule(new SendAudio(queue, rootProcessor, messages, now, options));
    }

    Future* sendLater(Time time, 
                      std::list<Message> messages, 
                      SendOptions options)
    {
        Time now = timeSource->millisecondTime();
        return schedule(new SendAudio(queue, rootProcessor, messages, now + time, options));
    }

    Future* sendAt(Time time, 
                   std::list<Message> messages, 
                   SendOptions options)
    {
        Time now = timeSource->millisecondTime();

        Future* future = new SendAudio(queue, rootProcessor, messages, time, options);
        if (now - latency <= time) schedule(future);
        return future;
    }

    Future* receive(Receiver* receiver, 
                    ReceiveOptions options)
    {
        throw Unimplemented();
    }

protected:
    Future* schedule(Future* future)
    {
        return RealtimeScheduler::schedule(future);
    }
    void execute(Time time)
    {
        return RealtimeScheduler::execute(time);
    }

private:
    TimeProvider* const timeSource;
    AudioProcessor* rootProcessor;
    Queue< boost::tuple< AudioProcessor*, std::list<Message> >* >* queue;
};
         
} // namespace
} // namespace

#endif
