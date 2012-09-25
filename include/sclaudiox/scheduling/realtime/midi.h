/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_SCHEDULING_REALTIME_MIDI
#define _SCLAUDIOX_SCHEDULING_REALTIME_MIDI

#include "sclaudiox/scheduling/message.h"
#include "sclaudiox/scheduling/realtime/scheduler.h"
#include "sclaudiox/util/foreach.h"

namespace doremir {
namespace scl {

class SCLAUDIO_API SendMidi : public SendFuture
{
public:
    SendMidi(PmStream**         inputStream,
             std::list<Message> messages, 
             Time               time, 
             SendOptions        options)             
        : SendFuture(messages, time, options)
        , inputStream(inputStream)
    {
        channels = options.channels;
    } 

    void perform(Time now)
    {
        int bufferIndex = 0;
        foreach (Message msg, messages)
        {
            // FIXME handle
            // if (bufferIndex < SCL_MIDI_OUTPUT_BUFFER_SIZE)
                // ; 
                
            foreach(int ch, channels)
            {
                events[bufferIndex].timestamp = 0;
                events[bufferIndex].message   = messageToMidi(msg);
            }
            ++bufferIndex;
        }

        PmError result = Pm_Write(*inputStream, events, bufferIndex);
    
        if (result != pmNoError) 
            throw PortmidiError(result);
    }                    
    
private:
    PmStream** const inputStream;   
    PmEvent    events[SCL_MIDI_OUTPUT_BUFFER_SIZE];
    std::list<int>  channels;
};


class SCLAUDIO_API ReceiveMidi : public ReceiveFuture
{
public:
    ReceiveMidi(PmStream**     inputStream,
                Receiver*      receiver, 
                Time           time, 
                ReceiveOptions options)             
        : ReceiveFuture(receiver, time, options) 
        , inputStream(inputStream) {}

    void perform(Time now)
    {
        if (Pm_Poll(*inputStream))
        {
            PmError result = (PmError) Pm_Read(*inputStream, events, SCL_MIDI_INPUT_BUFFER_SIZE);
            
            if (result < 0) 
                throw PortmidiError(result);

            for (int bufferIndex = 0; bufferIndex < result; ++bufferIndex)
            {
                MessageInfo info;

                Time    time    = fromMidiTime(events[bufferIndex].timestamp);
                Message message = midiToMessage(resetMidiChannel(events[bufferIndex].message));
                info.channel    = midiChannel(events[bufferIndex].message);

                SCL_WRITE_LOG(toString(message) << "\n");

                receiver->accept(time, message, info);
            }
        }
    }

private:
    PmStream** const inputStream;
    PmEvent events[SCL_MIDI_INPUT_BUFFER_SIZE];
};

/**
    Like ReceiveMidi but echoes input to output.
 */
class SCLAUDIO_API ForwardReceiveMidi : public ReceiveFuture
{
//    FIXME midi echoing assumes input and output buffer have same size
//    FIXME overflow handling in midi echoing

public:
    ForwardReceiveMidi(PmStream**     inputStream,
                       PmStream**     outputStream,
                       Receiver*      receiver, 
                       Time           time, 
                       ReceiveOptions options)             
        : ReceiveFuture(receiver, time, options) 
        , inputStream(inputStream)
        , outputStream(outputStream) {}


    void perform(Time now)
    {
        if (Pm_Poll(*inputStream))
        {
            PmError result = (PmError) Pm_Read(*inputStream, inputEvents, SCL_MIDI_INPUT_BUFFER_SIZE);
            
            if (result < 0) 
                throw PortmidiError(result);

            for (int bufferIndex = 0; bufferIndex < result; ++bufferIndex)
            {
                outputEvents[bufferIndex] = inputEvents[bufferIndex];
            
                MessageInfo info;
                Time time       = fromMidiTime(inputEvents[bufferIndex].timestamp);
                Message message = midiToMessage(resetMidiChannel(inputEvents[bufferIndex].message));
                info.channel    = midiChannel(inputEvents[bufferIndex].message);

                receiver->accept(time, message, info);
            }
            result = Pm_Write(*outputStream, outputEvents, result);
    
            if (result != pmNoError) 
                throw PortmidiError(result);
        }

    }

private:
    PmStream **const inputStream, **const outputStream;
    
    PmEvent inputEvents[SCL_MIDI_INPUT_BUFFER_SIZE];
    PmEvent outputEvents[SCL_MIDI_OUTPUT_BUFFER_SIZE];
};


class SCLAUDIO_API MidiScheduler : public MessageScheduler, 
                                   public RealtimeScheduler
{
public:
    MidiScheduler(TimeProvider* timeSource, 
                  PmStream** inputStream, 
                  PmStream** outputStream) 
        : RealtimeScheduler(timeSource)
        , timeSource(timeSource)
        , inputStream(inputStream)
        , outputStream(outputStream) {}

    Future* sendNow(std::list<Message> messages, 
                    SendOptions   options)
    {
        Time now = timeSource->millisecondTime();
        if (hasOutput())
        {
            return schedule(new SendMidi(outputStream, messages, now, options));
        }
        else 
        {
            return schedule(new NilFuture(now, options));
        }
    }
    
    Future* sendLater(Time time, 
                      std::list<Message> messages, 
                      SendOptions options)
    {
        Time now = timeSource->millisecondTime();
        if (hasOutput())
        {
            return schedule(new SendMidi(outputStream, messages, now + time, options));
        }
        else
        {
            return schedule(new NilFuture(now, options));
        }
    }
    
    Future* sendAt(Time time, 
                   std::list<Message> messages, 
                   SendOptions options)
    {
        Time now = timeSource->millisecondTime();
        if (hasOutput())
        {
            Future* future = new SendMidi(outputStream, messages, time, options);
            if (now - latency <= time) schedule(future);
            return future;
        }
        else
        {
            return schedule(new NilFuture(now, options));
        }
    }

    Future* receive(Receiver* receiver, 
                    ReceiveOptions options)
    {    
// TODO #SCL-349 make midi echoing optional    

        if (hasInput() && hasOutput())
            return schedule(new ForwardReceiveMidi(inputStream, outputStream, receiver, 0, options));

        else if (hasInput())
            return schedule(new ReceiveMidi(inputStream, receiver, 0, options));

        else 
            return schedule(new NilFuture(0, options));
    }
    
    bool hasInput()
    {
        return *inputStream != NULL;
    }
    
    bool hasOutput()
    {
        return *outputStream != NULL;
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
    PmStream**    const inputStream;    
    PmStream**    const outputStream;    
};
                
} // namespace
} // namespace

#endif
