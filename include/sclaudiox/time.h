/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef  _SCLAUDIOX_TIME
#define  _SCLAUDIOX_TIME

namespace doremir {
namespace scl {


enum TimeUnit
{
    kSamples,
    kMilliseconds
};

/**
    Basic type used in to schedule actions and messages.
    TODO Make into a class, tagged with TimeUnit.
 */
typedef int32_t Time;

// namespace tentative 
// {
//     class SCLAUDIO_API Time
//     {               
//     public:
//         inline Time(){}
//         inline Time(Int32 time){}
//         inline Time(Int64 time){}
//     };
//     Time operator +(const Time &a, const Time& b) {}
//     Time operator -(const Time &a, const Time& b) {}
//     Time operator *(const Time &a, const Time& b) {}
//     Time operator /(const Time &a, const Time& b) {}
//     Time operator %(const Time &a, const Time& b) {}
// 
//     Time operator *(const Time &a, const Int32& b) {}
//     Time operator /(const Time &a, const Int32& b) {}
//     Time operator %(const Time &a, const Int32& b) {}
// 
//     Time operator *(const Int32 &a, const Time& b) {}
//     Time operator /(const Int32 &a, const Time& b) {}
//     Time operator %(const Int32 &a, const Time& b) {}    
// }


/**
    Approximate real-word time in seconds.
  */
typedef double RealTime;



/**
    Provides time information.
    
    Each subclass must override exactly one of the methods sampleTime() and 
    millisecondTime().
 */
class SCLAUDIO_API TimeProvider
{
public:
    TimeProvider(Time sampleRate) : sampleRate() {}

    virtual ~TimeProvider() {}

    /**
        Returns the current time in samples.
     */
    virtual Time sampleTime()
    {
        return millisecondsToSamples(millisecondTime(), sampleRate);
    }

    /**
        Returns the current time in milliseconds.
     */
    virtual Time millisecondTime()
    {
        return samplesToMilliseconds(sampleTime(), sampleRate);
    }


    static inline Time samplesToMilliseconds(Time input, Time rate)
    {
        Time frame  = input / rate;
        Time offset = input % rate;
        return (frame * 1000) + ((offset * 1000) / rate);
    }

    static inline Time millisecondsToSamples(Time input, Time rate)
    {
        Time frame  = input / 1000;
        Time offset = input % 1000;
        return (frame * rate) + ((offset * rate) / rate);
    }

    const Time sampleRate;
};


} // namespace
} // namespace

#endif
