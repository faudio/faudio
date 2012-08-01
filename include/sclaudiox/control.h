/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef  _SCLAUDIOX_CONTROL
#define  _SCLAUDIOX_CONTROL
         
#include "sclaudiox/core.h"
#include "sclaudiox/error.h"
#include "sclaudiox/util/misc.h"
#include "portmidi.h"

namespace doremir {
namespace scl {

#define kMidiNoteOff         0x80
#define kMidiNoteOn          0x90
#define kMidiAfterTouch      0xA0
#define kMidiControlChange   0xB0
#define kMidiProgramChange   0xC0
#define kMidiChannelPressure 0xD0
#define kMidiPitchWheel      0xE0
#define kMidiSysEx           0xF0

inline int getAction(int status)
{
    return status & 0xf0;
}   

inline int getChannel(int status)
{
    return status & 0x0f;
}

inline bool isSysEx(int status)
{
    return getAction(status) == kMidiSysEx;
}


enum AtomType
{
    kIntAtom,
    kDoubleAtom,
    kStringAtom
};

/** 
    A tagged union of primitive values.
 */
class SCLAUDIO_API Atom
{
public:
    Atom() : mType(kIntAtom) 
    { 
        mPrimVal.integer = 0; 
    }

    /**
        Get the type of atom.
     */
    inline AtomType type()
    { 
        return mType; 
    }

    /**
        Extract an integer from this atom.
        The result is undefined unless <code>type() == AtomInt</code>.
     */
    inline int getInt()
    { 
        int x;
        access(x);
        return x;
    }

    /**
        Extract an double from this atom.
        The result is undefined unless <code>type() == AtomInt</code>.
     */
    inline double getDouble()
    { 
        double x;
        access(x);
        return x;
    }

    /**
        Extract a string from this atom.
        The result is undefined unless <code>type() == AtomInt</code>.
     */
    inline String getString()
    { 
        String x;
        access(x);
        return x;
    }

private:  
    inline void access(int& x)
    {
        x = mPrimVal.integer;
    }

    inline void access(double& x)
    {
        x = mPrimVal.doubleFloating;
    }

    inline void access(String& x)
    {
        x = mStringVal;
    }

    template<class T> friend Atom* createAtom(T val);

    template<class T> friend Atom toAtom(T val);

    template<class T> friend T fromAtom(Atom val);


    explicit inline Atom(int value) 
        : mType(kIntAtom) 
    { 
        mPrimVal.integer = value;
    }
    
    explicit inline Atom(double value) 
        : mType(kDoubleAtom) 
    {
        mPrimVal.doubleFloating = value;
    }
    
    explicit inline Atom(String value) 
        : mType(kStringAtom)
        , mStringVal(value) {}

    AtomType mType;

    String mStringVal;
    union { char   character;
            int    integer;
            short  shortInteger;
            long   longInteger;
            float  floating;
            double doubleFloating; } mPrimVal;
};

template<class T> Atom* createAtom(T val)
{
    return new Atom(val);
}

inline void freeAtom(Atom* atom)
{
    delete atom;
} 

template<class T> Atom toAtom(T val)
{
    return Atom(val);
}
    
template<class T> T fromAtom(Atom val)
{
    T result;
    val.access(result);
    return result;
}

template <>
inline
String toString(Atom value)
{
    switch (value.type()) 
    {
        case kIntAtom:    return toString(value.getInt());
        case kDoubleAtom: return toString(value.getDouble());
        case kStringAtom: return value.getString();
        default: 
          throw Impossible();
    }
}


typedef std::list<Atom>           Message;
typedef std::list<AtomType>       MessageType;


template <class A>
Message messageFrom(A a)
{
    return list::create(toAtom(a));
}

template <class A, class B>
Message messageFrom(A a, B b)
{
    return list::create(toAtom(a), toAtom(b));
}

template <class A, class B, class C>
Message messageFrom(A a, B b, C c)
{
    return list::create(toAtom(a), toAtom(b), toAtom(c));
}

template <class A, class B, class C, class D>
Message messageFrom(A a, B b, C c, D d)
{
    return list::create(toAtom(a), toAtom(b), toAtom(c), toAtom(d));
}

template <class A, class B, class C, class D, class E>
Message messageFrom(A a, B b, C c, D d, E e)
{
    return list::create(toAtom(a), toAtom(b), toAtom(c), toAtom(d), toAtom(e));
}


template <class A>
void messageTo(Message msg, A& a)
{
    Atom a2;
    return list::destroy(msg, a2);
    a = fromAtom<A>(a2);
}

template <class A, class B>
void messageTo(Message msg, A& a, B& b)
{
    Atom a2, b2;
    list::destroy(msg, a2, b2);
    a = fromAtom<A>(a2);
    b = fromAtom<B>(b2);
}

template <class A, class B, class C>
void messageTo(Message msg, A& a, B& b, C& c)
{
    Atom a2, b2, c2;
    list::destroy(msg, a2, b2, c2);
    a = fromAtom<A>(a2);
    b = fromAtom<B>(b2);
    c = fromAtom<C>(c2);
}


template <class A, class B, class C, class D>
void messageTo(Message msg, A& a, B& b, C& c, D& d)
{
    Atom a2, b2, c2, d2;
    list::destroy(msg, a2, b2, c2, d2);
    a = fromAtom<A>(a2);
    b = fromAtom<B>(b2);
    c = fromAtom<C>(c2);
    d = fromAtom<D>(d2);
}
                      

// =============================================================================

inline Time fromMidiTime(PmTimestamp time)
{
    return time;
}

inline PmTimestamp toMidiTime(Time time)
{
    return time;
}

/**
    Returns the channel of the given Midi message.
 */
inline int midiChannel(PmMessage midiMsg)
{
    return midiMsg & 0xf;
}

/**
    Updates the channel of the given Midi message.
 */
inline PmMessage setMidiChannel(PmMessage midiMsg, int channel)
{
    return (midiMsg & ~0xf) | (channel & 0xf);
}

/**
    Sets the channel of the given Midi message to 0.
 */
inline PmMessage resetMidiChannel(PmMessage midiMsg)
{
    return midiMsg & ~0xf;
}

/**
    Converts the given Midi message to an internal message.
 */
inline Message midiToMessage(PmMessage midiMsg)
{   
    int status = Pm_MessageStatus(midiMsg);
    int data1  = Pm_MessageData1(midiMsg);
    int data2  = Pm_MessageData2(midiMsg);
    return messageFrom(status, data1, data2); // TODO somthing cleaner...
}

/**
    Converts the given internal message to a Midi message.
 */
inline PmMessage messageToMidi(Message msg)
{
    int status, data1, data2;
    if ((msg.front().getInt() & 0xf0) == 0xC)
    {
        messageTo(msg, status, data1);        // TODO 
        data2 = 0;
    }
    else 
    {
        messageTo(msg, status, data1, data2); // TODO
    }
    return Pm_Message(status, data1, data2);
}

/**
    Converts the given internal message to a Midi message
    on the given channel.
 */
inline PmMessage messageToMidi(Message msg, int channel)
{
    return setMidiChannel(messageToMidi(msg), channel);
}

} // namespace
} // namespace

#endif