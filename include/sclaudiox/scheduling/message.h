/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_SCHEDULING_MESSAGE
#define _SCLAUDIOX_SCHEDULING_MESSAGE

#define SCL_DEF_RECEIVE_INTERVAL 5

#include "sclaudiox/scheduling/scheduler.h"

namespace doremir {
namespace scl {

enum MessageKind
{
    kAudioMessage,
    kMidiMessage
};

class SCLAUDIO_API AudioProcessor;

/**
    Options passed to a MessageScheduler when sending messages.
 */
struct SendOptions : public ScheduleOptions
{
    SendOptions()
        : kind(kMidiMessage)
        , processors(std::list<AudioProcessor*>(0))
        , devices(std::list<MidiDevice*>(0))
        , channels(list::create(0)){}

    MessageKind                kind;
    std::list<AudioProcessor*> processors;
    std::list<MidiDevice*>     devices;
    std::list<int>             channels;    
};

/**
    Options passed to a MessageScheduler when receiving messages.
 */
struct ReceiveOptions : public ScheduleOptions
{
    static const int repeatForever = -1;

    ReceiveOptions()
        : kind(kMidiMessage)
        , processors(std::list<AudioProcessor*>(0))
        , devices(std::list<MidiDevice*>(0))
        , channels(list::create(0))
    {
        repeats  = repeatForever;
        interval = SCL_DEF_RECEIVE_INTERVAL;
    }

    MessageKind                kind;
    std::list<AudioProcessor*> processors;
    std::list<MidiDevice*>     devices;
    std::list<int>             channels;
};

/**
    Options passed to a MessageScheduler when receiving messages (buffered version).
 */
struct ReceiveBufferingOptions : public ReceiveOptions
{
    int   bufferLimit;
    Time  maxTime;
    bool  flushOnStop;
    
    ReceiveBufferingOptions()
        : bufferLimit(0)
        , maxTime(0)
        , flushOnStop(true){}
};    

/**
    Meta-information about messages passed along to Receivers.
 */
struct MessageInfo
{
public:
    MessageKind     kind;
    AudioProcessor* processor;
    MidiDevice*     Device;
    int             channel;    
};



/**
    An object that receives messages.
 */
class SCLAUDIO_API Receiver
{
public:        
    virtual ~Receiver() {}
    virtual void accept(Time time, Message message, MessageInfo info) const = 0;
};    

/**
    An object that sends messages.
 */
class SCLAUDIO_API Sender
{
public:        
    virtual ~Sender() {}
    virtual void addReceiver(Receiver* recv) const = 0;
    virtual void removeReceiver(Receiver* recv) const = 0;
};    

class SCLAUDIO_API AbstractSender
{
public:
    AbstractSender(){}
    ~AbstractSender(){}
    void addReceiver(Receiver* recv) const {}
    void removeReceiver(Receiver* recv) const {};
};


/**
    Used to send and receive messages. The send methods dispatches the given
    message at the given point in time, while the receive methods retrieves incoming
    messages at the given point in time.
    
    By default, sending messages is non-repeating while receiving is repeating, but
    this can be changed using the options argument.
 */
class SCLAUDIO_API MessageScheduler : public Scheduler
{
public:    
    virtual ~MessageScheduler() {}

    virtual Future* sendNow(std::list<Message>  messages, 
                            SendOptions options = SendOptions()) = 0;
    
    virtual Future* sendLater(Time time, 
                              std::list<Message> messages, 
                              SendOptions options = SendOptions()) = 0;
    
    virtual Future* sendAt(Time time, 
                           std::list<Message> messages, 
                           SendOptions options = SendOptions()) = 0;

    
    virtual Future* receive(Receiver* receiver, 
                            ReceiveOptions options = ReceiveOptions()) = 0;

    // virtual Future& receiveBuffered(BufferedReceiver& receiver, 
    //                                 ReceiveBufferingOptions options = ReceiveBufferingOptions()) = 0;
};
    

/**
    Abstract superclass of futures that send messages.
    
    This class simply keeps a list of messages, implementations must implement
    perform to actually send the message to some external destination.
 */
class SCLAUDIO_API SendFuture : public Future
{
public:
    SendFuture(std::list<Message> messages, 
               Time time, 
               SendOptions options)
        : Future(time, options)
        , messages(messages) {}

    void perform(Time now) = 0;
    
protected:
    std::list<Message> messages;
};


/**
    Abstract superclass of futures that receive messages.

    This class simply a message receiver, implementations must implement
    perform to fetch the message from some extrnal source and pass
    it to the receiver.
 */
class SCLAUDIO_API ReceiveFuture : public Future
{
public:
    ReceiveFuture(Receiver* receiver, 
                  Time time, 
                  ReceiveOptions options)
        : Future(time, options)
        , receiver(receiver) {}

    void perform(Time now) = 0;
    
protected:
    Receiver* receiver;
};


// TODO ReceiveBufferFuture    

} // namespace
} // namespace

#endif
