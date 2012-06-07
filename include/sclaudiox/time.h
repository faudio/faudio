/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef  _SCLAUDIOX_TIME
#define  _SCLAUDIOX_TIME

namespace doremir {
namespace scl {

/**
    We measure time in samples or milliseconds. The relation between these two
    depends on the sample rate of the audio stream.
 */
enum TimeUnit
{
    kSamples,
    kMilliseconds
};

/**
    Basic type used in to schedule actions and messages.
    TODO Make into a class, tagged with TimeUnit.
 */
typedef Int32 Time;

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
typedef Float64 RealTime;




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


    static inline SCLAUDIO_API Time samplesToMilliseconds(Time input, Time rate)
    {
        Time frame  = input / rate;
        Time offset = input % rate;
        return (frame * 1000) + ((offset * 1000) / rate);
    }

    static inline SCLAUDIO_API Time millisecondsToSamples(Time input, Time rate)
    {
        Time frame  = input / 1000;
        Time offset = input % 1000;
        return (frame * rate) + ((offset * rate) / rate);
    }


// TODO using both samples and milliseconds in schedulers
    
//    virtual TimeUnit timeUnit() = 0;
//    
//    Time normalizeTime(Time time, TimeUnit unit)
//    {
//        if (timeUnit() == unit)   return time;
//        switch (unit)
//        {
//            case Milliseconds: return millisecondsToSamples(time, sampleRate);
//            case Samples:      return samplesToMilliseconds(time, sampleRate);
//        }
//    }
    
    const Time sampleRate;
};


} // namespace
} // namespace

#endif
