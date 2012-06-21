
#include "gtest/gtest.h"
#include "sclaudio.h"
#include "sclaudiox.h"
#include "sclaudiox/util/atomic.h"
#include "sclaudiox/util/logging.h"
#include "sclaudiox/util/symbol.h"
#include "sclaudiox/util/string.h"
#include "sclaudiox/util/single.h"
#include "sclaudiox/processor/plugin/au.h"
#include "sclaudiox/processor/synth/rand.h"

// ASSERT EXPECT:

// EQ NE GT LT LE GE
// FLOAT_EQ DOUBLE_EQ NEAR

// STREQ STRNE STRCASEEQ STRCASENE
// TRUE FALSE
// THROW(s, e) ANY_THROW(s) NO_THROW(s)

// SUCCEED() FAIL() ADD_FAILURE() ADD_FAILURE_AT(file, lineno)

#define EXPECT_NULL(E) EXPECT_EQ ( 0, (int) E )
#define EXPECT_VAL(E)  EXPECT_NE ( 0, (int) E )

#ifdef SCL_OSX
    #define SOUNDFONT_PATH \
        "/Users/hans/Documents/Kod/doremir/modus/app/resources/soundfonts/sound.sf2"
#else
    #define SOUNDFONT_PATH \
        "C:/modus/app/resources/soundfonts/sound.sf2"
#endif


namespace test_c_api
{
/*
    TEST(AudioDevices, AudioInputAvailable)
    {
        SclPortaudioError err = NULL;
        SclAudioDevice in = scl_default_audio_input_device(&err);
        EXPECT_NULL( err );
    }
    TEST(AudioDevices, AudioOutputAvailable)
    {
        SclPortaudioError err = NULL;
        SclAudioDevice in = scl_default_audio_output_device(&err);
        EXPECT_NULL( err );
    }
    
    
    TEST(MidiDevices, MidiInputAvailable)
    {
        SclPortmidiError err = NULL;
        SclMidiDevice in = scl_default_midi_input_device(&err);
        EXPECT_NULL( err );
    }
    TEST(MidiDevices, MidiOutputAvailable)
    {
        SclPortmidiError err = NULL;
        SclMidiDevice in = scl_default_midi_output_device(&err);
        EXPECT_NULL( err );
    }
    TEST(FluidSynth, Base)
    {   
        SclDspError err = NULL;
        // scl_load_fluidsynth(SOUNDFONT_PATH, &err);
        EXPECT_NULL( err );
    }
    
    TEST(Atomic, Base)
    {
    }
    TEST(Error, Base)
    {
    }
    TEST(Queue, Base)
    {
    }
    TEST(Resource, Base)
    {
    }
    TEST(String, Base)
    {
    }
    TEST(ThreadLocal, Base)
    {
    }
*/
}

namespace test_cpp_api
{
    using namespace doremir::scl;


    TEST(AudioDevices, AudioInputAvailable)
    {
        EXPECT_NO_THROW( AudioDevice::defaultInputDevice() );
    }
    TEST(AudioDevices, AudioOutputAvailable)
    {
        EXPECT_NO_THROW( AudioDevice::defaultOutputDevice() );
    }
    
    
    TEST(MidiDevices, MidiInputAvailable)
    {
        EXPECT_NO_THROW( MidiDevice::defaultInputDevice() );
    }
    TEST(MidiDevices, MidiOutputAvailable)
    {
        EXPECT_NO_THROW( MidiDevice::defaultOutputDevice() );
    }
    TEST(MidiDevices, ListMidiDevices)
    {
        std::list<MidiDevice*> devices =  MidiDevice::devices();
        foreach (MidiDevice* device, devices)
        {
            std::cout << ""     << device->name() << "\n";
            std::cout << "    " << device->hostName() << "\n";
            std::cout << "    " 
                << ( device->hasInput() ? "Input" : "-" ) 
                << "/" 
                << ( device->hasOutput() ? "Output" : "-" ) << "\n";
        }
        std::cout << "\n";
    }

    TEST(FluidSynth, Base)
    {
        EXPECT_NO_THROW( FluidSynth fs (SOUNDFONT_PATH) );
    }     
        
    TEST(List, Base)
    {
        EXPECT_EQ ( true, list::isEmpty(list::create<int>()) );
        EXPECT_EQ ( false, list::isEmpty(list::create(1)) );
    }

    TEST(Atomic, AtomicReplace)
    {
        Atomic<int> x (0);
        EXPECT_EQ( x.value(), 0 );
    
        x.replace(1);
        EXPECT_EQ( x.value(), 1 );
    
        x.replace(2312);
        EXPECT_EQ( x.value(), 2312 );
    }
    
    TEST(Atomic, AtomicTryReplace)
    {
        Atomic<int> x (0);
        EXPECT_EQ( x.value(), 0 );
    
        x.tryReplace(0, 1);
        x.tryReplace(0, 2);
        EXPECT_EQ( x.value(), 1 );
    
        x.tryReplace(3, 0);
        x.tryReplace(1, 5);
        EXPECT_EQ( x.value(), 5 );
    }           
    
    TEST(Atomic, AtomicCopy)
    {
        Atomic<int> x (0);
        Atomic<int> y (x);
        EXPECT_EQ( x, y );
        
        x.replace(1);
        EXPECT_NE( x, y);
        
        y.replace(1);
        EXPECT_EQ( x, y );
    }

    TEST(Atomic, AtomicAssign)
    {
        Atomic<int> x (0);
        Atomic<int> y (0);
        EXPECT_EQ( x, y );
        
        x.replace(1);
        EXPECT_NE( x, y);
        
        y = x;
        EXPECT_EQ( x, y );
    }

    TEST(AtomicNumber, AtomicNumberCopy)
    {
        AtomicNumber<int> x (0);
        AtomicNumber<int> y (x);
        EXPECT_EQ( x, y );
        
        x.replace(1);
        EXPECT_NE( x, y);
        
        y.replace(1);
        EXPECT_EQ( x, y );
    }

    TEST(AtomicNumber, AtomicNumberAssign)
    {
        AtomicNumber<int> x (0);
        AtomicNumber<int> y (0);
        EXPECT_EQ( x, y );
        
        x.replace(1);
        EXPECT_NE( x, y);
        
        y = x;
        EXPECT_EQ( x, y );
    }


    TEST(AtomicNumber, AtomicNumberAddSub)
    {
        AtomicNumber<int> x (0);
        EXPECT_EQ( x.value(), 0 );
    
        x.increment(1);
        EXPECT_EQ( x.value(), 1 );

        x.decrement(1);
        EXPECT_EQ( x.value(), 0 );

        x.increment(1232);
        EXPECT_EQ( x.value(), 1232 );

        x.decrement(1232);
        EXPECT_EQ( x.value(), 0 );    
    }
       
    // FIXME
/*
    TEST(AtomicNumber, AtomicNumberInc)
    {
        AtomicNumber<int> x (0);
        AtomicNumber<int> y (0);
                           
        y = ++x;
        EXPECT_EQ ( x.value(), 1 );
        EXPECT_EQ ( y.value(), 1 );

        y = --x;
        EXPECT_EQ ( x.value(), 0 );
        EXPECT_EQ ( y.value(), 0 );

    }*/ 

    TEST(Single, Base)
    {
        Single<int> single(1);
        foreach (int v, single)
        {
            std::cout << "The single value is " << v << "\n";
        }
    }
    
    TEST(List, Range)
    {
        std::list<int> xs = list::fromRange(0, 10);
        std::cout << toString(xs) << "\n";
    }
    
    TEST(String, Conversions)
    {                            
        std::cout << toString(1) << "\n";
        
        std::list<int> xs = list::create(1, 2, 3, 4);
        std::cout << toString(xs) << "\n";

        std::pair<int,int> p = std::make_pair(1, 2);
        std::cout << toString(p) << "\n";
    }
    
    TEST(Logging, Base)
    {           
        std::cout << logPath();
    }
    
    TEST(Error, Base)
    {
    }
    TEST(Queue, Base)
    {
    }
    TEST(Resource, Base)
    {
    }
    TEST(String, Base)
    {
    }
    TEST(ThreadLocal, Base)
    {
    }
    
    TEST(Misc, Base)
    {
        std::list<int> xs = list::create(1, 2, 3);
        std::list<int> ys = list::replace(xs, 1, 2);
        
        EXPECT_EQ( xs, xs );
        EXPECT_EQ( ys, ys );
        EXPECT_NE( xs, ys );
    }
    
    TEST(Symbol, Base)
    {
        Symbol* foo = Symbol::intern("foo");
        Symbol* foo2 = Symbol::intern("foo");
        Symbol* bar = Symbol::intern("bar");
        Symbol* bar2 = Symbol::intern("bar");
     
        EXPECT_EQ( foo, foo2 );
        EXPECT_EQ( bar, bar2 );
        EXPECT_NE( foo, bar );
    }
    
    TEST(AudioUnit, Count)
    {
        std::list<AudioUnit*> units = AudioUnit::audioUnits();
        std::cout << "Number of Audio Units: " << units.size();
        std::cout << "\n";        
    }                       

    TEST(Rand, Play)
    {
        try 
        {            
            AudioProcessor* synth = new Rand();
            AudioDevice*      in     = NULL;
            AudioDevice*      out    = AudioDevice::defaultOutputDevice();
            Stream*           stream = DeviceStream::open(in, out, synth);
            MessageScheduler* sched  = stream->audioScheduler();
                
            stream->start();  
            sleepMillis(2000);
            stream->stop();
            sleepMillis(1000);
        } 
        catch (const Error& e)
        {
            std::cerr << "Error: " << const_cast<Error&>(e).message() << "\n";
        }              
    }              


    TEST(Synth_FluidSynth, PlayNotes)
    {
        AudioProcessor*   synth  = new FluidSynth(SOUNDFONT_PATH);
        AudioDevice*      in     = AudioDevice::defaultInputDevice();
        AudioDevice*      out    = AudioDevice::defaultOutputDevice();
        Stream*           stream = DeviceStream::open(in, out, synth);
        MessageScheduler* sched  = stream->audioScheduler();
    
        stream->start();    
        foreach( int p, list::fromRange(0,24) )
        {
            Message on  = messageFrom(0x90, 60 + p, 70);
            Message off = messageFrom(0x90, 60 + p, 0);    
            sched->sendNow(list::create(on));
            sched->sendLater(90, list::create(off));
            sleepMillis(100);
        }
        stream->stop();
        sleepMillis(1000);
    }
    

    TEST(AudioUnit, List)
    {       
        try
        {
            
        std::list<AudioUnit*> units = AudioUnit::audioUnits();
        // foreach ( AudioUnit* unit, units )
        // {             
        //     std::cout << "" <<   unit->description()->name() << "\n";
        //     std::cout << "    " << ( unit->description()->isPlugin() ? "Plugin" : "Non-plugin" ) << "\n";
        //     std::cout << "    " << ( unit->description()->isAtomic() ? "Atomic" : "Non-atomic" ) << "\n";
        //     std::cout << "    " << ( unit->description()->isStateful() ? "Stateful" : "Non-stateful" ) << "\n";
        //     std::cout << "    Inputs: " << unit->description()->numberOfInputs() << "\n";
        //     std::cout << "    Outputs: " << unit->description()->numberOfOutputs() << "\n";
        // 
        // }                     
        // std::cout << "\n";
        std::cout << "Number of Audio Units: " << units.size();
        std::cout << "\n";

        }   
        catch (const Error& e)
        {
            std::cerr << "Error: " << const_cast<Error&>(e).message() << "\n";
        }              

    }    
 
    TEST(Synth_AudioUnit, Play)
    {   
        try 
        {            
            AudioUnit* unit  = AudioUnit::dlsMusicDevice();
            std::cout << "Address: " << unit << "\n";

            std::cout << "  Name: " << unit->description()->name() << "\n";
            std::cout << "  Atomic: " << unit->description()->isAtomic() << "\n";
            std::cout << "  Stateful: " << unit->description()->isStateful() << "\n";
            std::cout << "  Plugin: " << unit->description()->isPlugin() << "\n";
            std::cout << "  Num inputs: " << unit->description()->numberOfInputs() << "\n";
            std::cout << "  Num outputs: " << unit->description()->numberOfOutputs() << "\n";

            AudioProcessor* synth  = unit->createProcessor();
            

            AudioDevice*      in     = NULL;
            AudioDevice*      out    = AudioDevice::defaultOutputDevice();
            Stream*           stream = DeviceStream::open(in, out, synth);
            MessageScheduler* sched  = stream->audioScheduler();
                
            stream->start();     
            SCL_WRITE_LOG("Sending notes...\n");
            foreach( int p, list::fromRange(0,24) )
            {
                Message on  = messageFrom(0x90, 60 + p, 70);
                Message off = messageFrom(0x90, 60 + p, 0);    
                sched->sendNow(list::create(on));
                sched->sendLater(90, list::create(off));
                sleepMillis(100);
            }
            stream->stop();
            
            sleepMillis(1000);   
        } 
        catch (const Error& e)
        {
            std::cerr << "Error: " << const_cast<Error&>(e).message() << "\n";
        }              
    }

}



int main(int argc, char **argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}