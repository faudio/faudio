/*
 *  sclaudio.cpp
 *  ScoreCleanerAudio
 *
 *  Created by Hans Höglund on 2011-10-19.
 *  Copyright 2011 DoReMIR http://www.doremir.com/. All rights reserved.
 *
 */

#include "sclaudio.h"
#include "sclaudiox.h"

using namespace doremir::scl;


// =============================================================================

// Allocation

template <typename Value>
Value* createArray(int size) 
{
    return new Value[size];
}

template <typename Value>
void freeArray(Value* arr) 
{
    delete arr;
}



// =============================================================================

// Value conversion

template <typename Internal, typename External> 
External ex(Internal internal)
{
    return reinterpret_cast<External> (internal);
}                                               

template <typename Internal, typename External>
Internal in(External external)
{
    return reinterpret_cast<Internal> (external);
}

template <>
int ex(int x)
{
    return x;  
}
template <>
int in(int x)
{
    return x;
}

template<> 
SclString ex(String str)
{
#ifdef SCL_UNICODE
    UErrorCode err = U_ZERO_ERROR;
    int32_t size = str.length() + 1;
    
    SclChar* extStr = new SclChar[size];
    str.extract(reinterpret_cast<UChar*>(extStr), size, err);
    
    return extStr;
#else
    return strcpy(new char[str.length() + 1], str.c_str());
#endif
}

template<>
String in(SclString cstr)
{
#ifdef SCL_UNICODE
    return UnicodeString(reinterpret_cast<UChar*>(cstr));
#else
    return std::string(cstr);
#endif
}

template <typename Internal> 
void* ex(Internal* internal)
{
    return ex<Internal*, void*>(internal);
}
template <typename Internal>
Internal* in(void* external)
{
    return in<Internal*, void*>(external);
}




// List conversion

template <typename Internal, typename External, typename List>
External* exportList(List lst, int* length)
{
    External* arr = createArray<External>(*length = lst.size());
    transform(lst.begin(), lst.end(), arr, ex<Internal, External>);
    return arr;
}

template <typename Internal, typename External>
std::list<Internal> importList(External* arr, int length)
{
    std::list<Internal> lst (length);
    transform(arr, arr + length, lst.begin(), in<Internal, External>);
    return lst;
}

template <typename Internal>
void** exportList(std::list<Internal*> lst, int* length)
{
    return exportList<Internal*,void*>(lst, length);
}
template <typename Internal>
std::list<Internal*> importList(void** arr, int length)
{
    return importList<Internal*,void*>(arr,length);
}


#define WITH_ONE_EXCEPTION(E1, V) \
    try { \
        return V; \
    } catch (E1& e) { \
        *err = new E1(e); \
        return NULL; \
    }

#define WITH_TWO_EXCEPTIONS(E1, E2, V) \
    try { \
        return V; \
    } catch (E1& e1) { \
        *err1 = new E1(e1); \
        return NULL; \
    } catch (E2& e2) { \
        *err2 = new E2(e2); \
        return NULL; \
    }

#define WITH_THREE_EXCEPTIONS(E1, E2, E3, V) \
    try { \
        return V; \
    } catch (E1& e1) { \
        *err1 = new E1(e1); \
        return NULL; \
    } catch (E2& e2) { \
        *err2 = new E2(e2); \
        return NULL; \
    } catch (E3& e3) { \
        *err3 = new E3(e3); \
        return NULL; \
    }
    
#define ONE_EXCEPTION(E1, S) \
    try { \
        S; \
    } catch (E1& e) { \
        *err = new E1(e); \
    }

#define TWO_EXCEPTIONS(E1, E2, S) \
    try { \
        S; \
    } catch (E1& e1) { \
        *err1 = new E1(e1); \
    } catch (E2& e2) { \
        *err2 = new E2(e2); \
    }

#define THREE_EXCEPTIONS(E1, E2, E3, S) \
    try { \
        S; \
    } catch (E1& e1) { \
        *err1 = new E1(e1); \
    } catch (E2& e2) { \
        *err2 = new E2(e2); \
    } catch (E3& e3) { \
        *err3 = new E3(e3); \
    }


#ifdef SCL_DEBUG

namespace 
{
    std::list<int>                   ints;
    std::list<String>                strings;
    std::list<Atom*>                 atoms;
    std::list<AudioDevice*>          audioDevices;
    std::list< std::list<int> >           listInts;
    std::list< std::list<AudioDevice*> >  listAudioDevices;
    std::list< std::list<Atom*> >         listAtoms;
}

#endif // endif SCL_DEBUG



// =============================================================================

extern "C" {

// =============================================================================

SclMessageKind      scl_message_type_audio() { return kAudioMessage; }
SclMessageKind      scl_message_type_midi() { return kMidiMessage; }
SclTimeUnit         scl_time_unit_samples() { return kSamples; }
SclTimeUnit         scl_time_unit_milliseconds() { return kMilliseconds; }
SclAtomType         scl_atom_string() { return kStringAtom; }
SclAtomType         scl_atom_int() { return kIntAtom; }
SclAtomType         scl_atom_double() { return kDoubleAtom; }
SclInterruptionMode scl_interruption_mode_simple() { return kSimple; }
SclInterruptionMode scl_interruption_mode_forcing() { return kForcing; }
SclInterruptionMode scl_interruption_mode_transactional() { return kTransactional; }


// =============================================================================

#ifdef SCL_DEBUG

void scl_test_nothing()
{
    // nothing
}

SCL_DECLARE_SIMPLE_ERROR(TestError);

void scl_test_error(int variant, SclError* err)
{
    switch (variant) {
        case 0:
            *err = ex<Error>(new TestError("Test error"));
            break;
        default:
            break;
    }
}
                                       
void scl_test_two_errors(int variant, SclPortmidiError *merr, SclPortaudioError *aerr)
{
    switch (variant) {
        case 0:
                *merr = ex<Error>(new PortmidiError(1));
            break;
        case 1:
                *aerr = ex<Error>(new PortaudioError(2));
            break;
        case 2:
                *merr = ex<Error>(new PortmidiError(1));
                *aerr = ex<Error>(new PortaudioError(2));
            break;
        default:
            break;
    }
}

                                    
int scl_test_pass_int(int x)
{
    return x;
}

float scl_test_pass_float(float x)
{
    return x;
}

double scl_test_pass_double(double x)
{
    return x;
}

SclTimeUnit scl_test_pass_enum(SclTimeUnit x)
{
    return x;
}

SclString scl_test_pass_string(SclString x)
{
    String s (in<String,SclString>(x));
    return ex<String,SclString>("hello " + s);
}

SclAtom scl_test_pass_atom(SclAtom x)
{
    Atom *a = in<Atom>(x);
    return ex<Atom>(a);
}
    
void* scl_test_pass_object(void* obj)
{
    return obj;
}

void scl_test_pass_list_int(int *x, int len)
{
    std::list<int> lst = importList<int>(x, len);
    ints = lst;
}

void scl_test_pass_list_string(SclChar **x, int len)
{
    std::list<String> lst = importList<String>(x, len);
    strings = lst;
}

void scl_test_pass_list_object(void **x, int len)
{
    std::list<AudioDevice*> lst = importList<AudioDevice>(x, len);
    audioDevices = lst;
}

void scl_test_pass_list_atom(SclAtom *x, int len)
{
    std::list<Atom*> lst = importList<Atom>(x, len);
    atoms = lst;
}


//void scl_test_pass_list_list_int(int **x, int *len1, int len2)
//{
//    list< list<int> > lst = importList<int,int>(x, len1, len2);
//    listInts = lst;
//}

//void scl_test_pass_list_list_object(void ***x, int *len1, int len2)
//{
//    list< list<AudioDevice*> > lst = importList<AudioDevice*,void*>(x, len1, len2);
//    listAudioDevices = lst;
//}

int scl_test_return_int()
{
    return 55;
}

float scl_test_return_float()
{
    return 55.0f;
}

double scl_test_return_double()
{
    return 55.0;
}

SclTimeUnit scl_test_return_enum()
{
    return scl_time_unit_samples();
}

SclString scl_test_return_string()
{
    return ex<String,SclString>(String("test"));
}

SclAtom scl_test_return_atom()
{
    Atom *a = createAtom(123);
    return ex<Atom>(a);
}

void* scl_test_return_object()
{
    SclError err;
    return scl_default_audio_input_device(&err);
}

int* scl_test_return_list_int(int *len)
{
    return exportList<int,int>(ints, len);    
}

SclString* scl_test_return_list_string(int *len)
{
//    list<String> lst;
//    lst.push_back("Hello");
//    lst.push_back("my");
//    lst.push_back("name");
//    lst.push_back("is");
//    lst.push_back("Elder");
//    lst.push_back("Price");
//    return exportList<String,SclString>(lst, len);
    return exportList<String,SclString>(strings, len);
}

SclAudioDevice* scl_test_return_list_object(int *len)
{
    return exportList<AudioDevice>(audioDevices, len);
}

SclAtom* scl_test_return_list_atom(int *len)
{
//    list<Atom*> lst;
//    lst.push_back(createAtom(11));
//    lst.push_back(createAtom(12));
//    lst.push_back(createAtom(13));
//    return exportList<Atom>(lst, len);
    return exportList<Atom>(atoms, len);
}

//int** scl_test_return_list_list_int(int **len1, int *len2)
//{
//    return exportList<int,int>(listInts, len1, len2);
//}
//
//SclAudioDevice** scl_test_return_list_list_object(int** len1, int *len2)
//{
//    return exportList<AudioDevice*,void*>(listAudioDevices, len1, len2);
//}

#endif // SCL_DEBUG


// =================================================================================================

SclString scl_schars_to_string(signed char* val)
{
    return reinterpret_cast<SclString> (val);
}

SclString scl_uchars_to_string(unsigned char* val)
{
    return reinterpret_cast<SclString> (val);
}

signed char* scl_string_to_schars(SclString val)
{
    return reinterpret_cast<signed char*> (val);
}

unsigned char* scl_string_to_uchars(SclString val)
{
    return reinterpret_cast<unsigned char*> (val);
}


// =============================================================================

void scl_free_error(SclError obj)
{
    release(in<Error>(obj));
}

void scl_free_portaudio_error(SclPortaudioError obj)
{
    release(in<PortaudioError>(obj));
}

void scl_free_portmidi_error(SclPortmidiError obj)
{
    release(in<PortmidiError>(obj));
}

void scl_free_audio_host(SclAudioHost obj)
{
    release(in<AudioHost>(obj));
}

void scl_free_audio_device(SclAudioDevice obj)
{
    release(in<AudioDevice>(obj));
}

void scl_free_midi_device(SclMidiDevice obj)
{
    release(in<MidiDevice>(obj));
}

void scl_free_future(SclFuture obj)
{
    release(in<Future>(obj));
}

void scl_free_stream(SclStream obj)
{
    release(in<Stream>(obj));
}

void scl_free_processor(SclAudioProcessor obj)
{
    release(in<AudioProcessor>(obj));
}

void scl_free_future_group(SclFutureGroup obj)
{
    release(in<FutureGroup>(obj));
}

void scl_free_atom(SclAtom obj)
{
    freeAtom(in<Atom>(obj));
}

void scl_free_schedule_options(SclScheduleOptions opts)
{
    delete in<ScheduleOptions*>(opts);
}

void scl_free_send_options(SclSendOptions opts)
{
    delete in<SendOptions*>(opts);
}

void scl_free_receive_options(SclReceiveOptions opts)
{
    delete in<ReceiveOptions>(opts);
}
    
void scl_free_array(void** arr)
{
    freeArray(arr);
}


// =============================================================================

SclString scl_error_message(SclError obj)
{
    return ex<String,SclString>(in<Error>(obj)->message());
}

int scl_portaudio_error_code(SclPortaudioError obj)
{
    return in<PortaudioError>(obj)->errorCode();
}

int scl_portmidi_error_code(SclPortmidiError obj)
{
    return in<PortmidiError>(obj)->errorCode();
}


// =============================================================================

SclString scl_midi_device_name(SclMidiDevice obj)
{
    return ex<String,SclString>(in<MidiDevice>(obj)->name());
}

SclString scl_midi_device_host_name(SclMidiDevice obj)
{
    return ex<String,SclString>(in<MidiDevice>(obj)->hostName());
}

int scl_midi_device_has_input(SclMidiDevice obj)
{
    return in<MidiDevice>(obj)->hasInput();
}

int scl_midi_device_has_output(SclMidiDevice obj)
{
    return in<MidiDevice>(obj)->hasOutput();
}

SclMidiDevice scl_default_midi_input_device(SclPortmidiError *err)
{
    WITH_ONE_EXCEPTION(PortmidiError, ex(acquire(MidiDevice::defaultInputDevice())));
}

SclMidiDevice scl_default_midi_output_device(SclPortmidiError *err)
{
    WITH_ONE_EXCEPTION(PortmidiError, ex(acquire(MidiDevice::defaultOutputDevice())));
}

SclMidiDevice* scl_midi_devices(int *length, SclPortmidiError *err)
{
    WITH_ONE_EXCEPTION(PortmidiError, exportList(acquireAll(MidiDevice::devices()), length));
}


// =============================================================================

SclString scl_audio_host_name(SclAudioHost obj)
{
    return ex<String,SclString>(in<AudioHost>(obj)->name());
}

int scl_audio_host_number_of_devices(SclAudioHost obj)
{
    return in<AudioHost>(obj)->numberOfDevices();
}

SclAudioDevice* scl_audio_host_devices(SclAudioHost obj, int *length)
{
    return exportList<AudioDevice>(acquireAll(in<AudioHost>(obj)->devices()), length);
}

SclAudioHost* scl_audio_hosts(int *length, SclPortaudioError *err)
{
    WITH_ONE_EXCEPTION(PortaudioError, exportList<AudioHost>(acquireAll(AudioHost::hosts()), length));
}

SclAudioHost scl_default_audio_host(SclPortaudioError *err)
{
    WITH_ONE_EXCEPTION(PortaudioError, ex(acquire(AudioHost::defaultHost())));
}


// =============================================================================

SclString scl_audio_device_name(SclAudioDevice obj)
{
    return ex<String,SclString>(in<AudioDevice>(obj)->name());
}

SclAudioHost scl_audio_device_host(SclAudioDevice obj)
{
    return ex(acquire(in<AudioDevice>(obj)->host()));
}

int scl_audio_device_max_input_channels(SclAudioDevice obj)
{
    return in<AudioDevice>(obj)->numberOfInputs();
}

int scl_audio_device_max_output_channels(SclAudioDevice obj)
{
    return in<AudioDevice>(obj)->numberOfOutputs();
}

int scl_audio_device_default_low_input_latency(SclAudioDevice obj)
{
    return in<AudioDevice>(obj)->lowInputLatency();
}

int scl_audio_device_default_high_input_latency(SclAudioDevice obj)
{
    return in<AudioDevice>(obj)->highInputLatency();
}

int scl_audio_device_default_low_output_latency(SclAudioDevice obj)
{
    return in<AudioDevice>(obj)->lowOutputLatency();
}

int scl_audio_device_default_high_output_latency(SclAudioDevice obj)
{
    return in<AudioDevice>(obj)->highOutputLatency();
}

int scl_audio_device_default_sample_rate(SclAudioDevice obj)
{
    return in<AudioDevice>(obj)->sampleRate();
}

SclAudioDevice scl_default_audio_input_device(SclPortaudioError *err)
{
    WITH_ONE_EXCEPTION(PortaudioError, ex(acquire(AudioDevice::defaultInputDevice())));
}

SclAudioDevice scl_default_audio_output_device(SclPortaudioError *err)
{
    WITH_ONE_EXCEPTION(PortaudioError, ex(acquire(AudioDevice::defaultOutputDevice())));
}


// =============================================================================

SclString scl_processor_name(SclAudioProcessor obj)
{
    return ex<String,SclString>(in<AudioProcessor>(obj)->description()->name());
}

int scl_processor_is_atomic(SclAudioProcessor obj)
{   
    return in<AudioProcessor>(obj)->description()->isAtomic();
}

int scl_processor_is_compound(SclAudioProcessor obj)
{   
    return in<AudioProcessor>(obj)->description()->isCompound();
}

int scl_processor_is_stateful(SclAudioProcessor obj)
{
    return in<AudioProcessor>(obj)->description()->isStateful();
}

int scl_processor_is_plugin(SclAudioProcessor obj)
{   
    return in<AudioProcessor>(obj)->description()->isPlugin();
}


int scl_processor_num_inputs(SclAudioProcessor obj)
{
    return in<AudioProcessor>(obj)->description()->numberOfInputs();
}

int scl_processor_num_outputs(SclAudioProcessor obj)
{
    return in<AudioProcessor>(obj)->description()->numberOfOutputs();
}

int scl_processor_num_buses(SclAudioProcessor obj)
{
    return in<AudioProcessor>(obj)->description()->numberOfBuses();
}


SclAudioProcessor scl_sequence(SclAudioProcessor *objs, int len, SclDspError *err)
{
    WITH_ONE_EXCEPTION(DspError, 
        ex(acquire(AudioProcessor::sequence(importList<AudioProcessor>(objs, len)))));
}

SclAudioProcessor scl_parallel(SclAudioProcessor *objs, int len, SclDspError *err)
{
    WITH_ONE_EXCEPTION(DspError, 
        ex(acquire(AudioProcessor::parallel(importList<AudioProcessor>(objs, len)))));
}

// SclAudioProcessor scl_load_fluidsynth(SclString path, SclDspError *err)
// {
//     WITH_ONE_EXCEPTION(FluidSynthError, 
//         ex(acquire(new FluidSynth(in<String,SclString>(path)))));
//  return NULL;
// }

SCLAUDIO_API SclAudioPlugin scl_plugin_from_error(SclAudioPluginError obj)
{
    return ex(acquire(in<AudioPluginError>(obj)->plugin()));
}

SCLAUDIO_API SclAudioPlugin scl_plugin_from_processor(SclAudioProcessor obj)
{
    return ex(acquire(in<AudioPluginProcessor>(obj)->plugin()));
}

SCLAUDIO_API SclString scl_plugin_name(SclAudioPlugin obj)
{
    return ex<String,SclString>(in<AudioPlugin>(obj)->description()->name());
}

SCLAUDIO_API int scl_plugin_num_inputs(SclAudioPlugin obj)
{
    return in<AudioPlugin>(obj)->description()->numberOfInputs();
}

SCLAUDIO_API int scl_plugin_num_outputs(SclAudioPlugin obj)
{
    return in<AudioPlugin>(obj)->description()->numberOfOutputs();
}

SCLAUDIO_API int scl_plugin_num_buses(SclAudioPlugin obj)
{
    return in<AudioPlugin>(obj)->description()->numberOfBuses();
}

SCLAUDIO_API SclAudioProcessor scl_plugin_create_processor(SclAudioPlugin obj, SclAudioPluginError* err)
{
    WITH_ONE_EXCEPTION(AudioPluginError, 
        ex(acquire(in<AudioPlugin>(obj)->createProcessor())));
	return NULL;
}





SclAudioProcessor* scl_load_audio_units(int *length)
{
    return exportList(acquireAll(AudioUnit::audioUnits()), length);
}                                

SclAudioPlugin scl_load_dls_music_device()
{
    return ex(acquire(AudioUnit::dlsMusicDevice()));
}



// =================================================================================================

SclAtomType scl_atom_type(SclAtom atom)
{
    return in<Atom>(atom)->type();
}

int scl_atom_to_int(SclAtom atom)
{
    return in<Atom>(atom)->getInt();
}

double scl_atom_to_double(SclAtom atom)
{
    return in<Atom>(atom)->getDouble();
}

SclString scl_atom_to_string(SclAtom atom)
{
    return ex<String,SclString>(in<Atom>(atom)->getString());
}

SclAtom scl_atom_from_int(int value)
{
    Atom *a = createAtom(value);
    return ex<Atom>(a);
}

SclAtom scl_atom_from_double(double value)
{
    Atom *a = createAtom(value);
    return ex<Atom>(a);
}

SclAtom scl_atom_from_string(SclString value)
{
    Atom *a = createAtom(in<String,SclString>(value));
    return ex<Atom>(a);
}


// =============================================================================

class CallbackErrorHandler : public Handler<Error>
{
public:
    CallbackErrorHandler(SclErrorHandler callback) : callback(callback) {}

    void accept(Time time, Error& error)
    {
        StreamError *error2 = new StreamError(error.message()); // FIXME
        callback(time, ex<Error>(error2));
    }

private:    
    SclErrorHandler callback;
};


int scl_stream_sample_rate(SclStream obj)
{
    // FIXME
    return 0;
}

int scl_stream_audio_buffer_size(SclStream obj)
{
    // FIXME
    return 0;
}

int scl_stream_running(SclStream obj)
{
    return in<Stream>(obj)->running();
}
          
void scl_stream_start(SclStream obj, SclPortaudioError *err1, SclPortmidiError* err2, SclDspError* err3)
{
    THREE_EXCEPTIONS(PortaudioError, PortmidiError, DspError, in<Stream>(obj)->start());
}

void scl_stream_stop(SclStream obj, SclPortaudioError *err1, SclPortmidiError* err2, SclDspError* err3)
{
    THREE_EXCEPTIONS(PortaudioError, PortmidiError, DspError, in<Stream>(obj)->stop());
}

void scl_stream_abort(SclStream obj, SclPortaudioError *err1, SclPortmidiError* err2, SclDspError* err3)
{
    THREE_EXCEPTIONS(PortaudioError, PortmidiError, DspError, in<Stream>(obj)->abort());
}                      

void scl_stream_set_error_handler(SclStream stream, SclErrorHandler callback)
{
    if (callback == NULL) 
        in<Stream>(stream)->setHandler(NULL);

    CallbackErrorHandler* handler = new CallbackErrorHandler(callback);
    in<Stream>(stream)->setHandler(handler);
}


SclStream scl_open_device_stream(SclMidiDevice midi_in,
                                 SclMidiDevice midi_out,
                                 SclAudioDevice audio_in,
                                 SclAudioDevice audio_out,
                                 SclAudioProcessor audio_processor,
                                 SclDeviceStreamOptions options,

                                 SclPortaudioError* err1,
                                 SclPortmidiError* err2,
                                 SclDspError* err3)
{
    DeviceStreamOptions streamOptions;
    if (options != NULL) streamOptions = *in<DeviceStreamOptions>(options);

    WITH_THREE_EXCEPTIONS(PortmidiError, PortaudioError, DspError,
        ex<Stream>(acquire(DeviceStream::open(in<MidiDevice>(midi_in),
                                            in<MidiDevice>(midi_out),
                                            in<AudioDevice>(audio_in),
                                            in<AudioDevice>(audio_out),
                                            in<AudioProcessor>(audio_processor),
                                            streamOptions))));
}


// =============================================================================

void scl_interrupt_future(SclFuture obj)
{
    in<Future>(obj)->interrupt();
}

void scl_interrupt_future_group(SclFutureGroup obj)
{
    in<FutureGroup>(obj)->interrupt();
}


// =============================================================================

SclFutureGroup scl_new_future_group(SclInterruptionMode mode)
{
    FutureGroup* group = new FutureGroup(static_cast<InterruptionMode>(mode));
    return ex<FutureGroup>(acquire(group));
}

SclInterruptionMode scl_future_group_interruption_mode(SclFutureGroup group)
{
    return in<FutureGroup>(group)->interruptionMode;
}


// =================================================================================================

SclDeviceStreamOptions scl_default_device_stream_options()
{
    DeviceStreamOptions* opts = new DeviceStreamOptions;
    return ex<DeviceStreamOptions>(opts);
}

int scl_device_stream_options_get_sample_rate(SclDeviceStreamOptions obj)
{
    return in<DeviceStreamOptions>(obj)->sampleRate;
}

int scl_device_stream_options_get_audio_buffer_size(SclDeviceStreamOptions obj)
{
    return in<DeviceStreamOptions>(obj)->audioBufferSize;
}

SclRealTime scl_device_stream_options_get_audio_latency(SclDeviceStreamOptions obj)
{
    return in<DeviceStreamOptions>(obj)->audioLatency;
}

SclRealTime scl_device_stream_options_get_midi_latency(SclDeviceStreamOptions obj)
{
    return in<DeviceStreamOptions>(obj)->midiLatency;
}

int scl_device_stream_options_is_non_blocking(SclDeviceStreamOptions obj)
{
    return in<DeviceStreamOptions>(obj)->useNonBlocking;
}

int scl_device_stream_options_is_exclusive_mode(SclDeviceStreamOptions obj)
{
    return in<DeviceStreamOptions>(obj)->useExclusiveMode;
}


void scl_device_stream_options_set_sample_rate(SclDeviceStreamOptions obj, int value)
{
    in<DeviceStreamOptions>(obj)->sampleRate = value;
}

void scl_device_stream_options_set_audio_buffer_size(SclDeviceStreamOptions obj, int value)
{
    in<DeviceStreamOptions>(obj)->audioBufferSize = value;
}

void scl_device_stream_options_set_audio_latency(SclDeviceStreamOptions obj, SclRealTime value)
{
    in<DeviceStreamOptions>(obj)->audioLatency = value;
}

void scl_device_stream_options_set_midi_latency(SclDeviceStreamOptions obj, SclRealTime value)
{
    in<DeviceStreamOptions>(obj)->midiLatency = value;
}

SclRealTime scl_device_stream_options_set_non_blocking(SclDeviceStreamOptions obj, int value)
{
    return in<DeviceStreamOptions>(obj)->useNonBlocking = value;
}

SclRealTime scl_device_stream_options_set_exclusive_mode(SclDeviceStreamOptions obj, int value)
{
    return in<DeviceStreamOptions>(obj)->useExclusiveMode = value;
}


// =============================================================================

SclScheduleOptions scl_default_schedule_options()
{
    ScheduleOptions* opts = new ScheduleOptions;
    return ex<ScheduleOptions>(opts);
}

SclScheduleOptions scl_default_send_options()
{
    SendOptions* opts = new SendOptions;
    return ex<SendOptions>(opts);
}

SclScheduleOptions scl_default_receive_options()
{
    ReceiveOptions* opts = new ReceiveOptions;
    return ex<ReceiveOptions>(opts);
}


// Schedule options

void scl_schedule_options_set_unit(SclScheduleOptions obj, SclTimeUnit unit)
{
    in<ScheduleOptions>(obj)->unit = static_cast<TimeUnit>(unit);
}

void scl_schedule_options_set_groups(SclScheduleOptions obj, SclFutureGroup *groups, int len)
{
    in<ScheduleOptions>(obj)->groups = importList<FutureGroup>(groups, len);
}

void scl_schedule_options_set_repeats(SclScheduleOptions obj, int repeats)
{
    in<ScheduleOptions>(obj)->repeats = repeats;
}

void scl_schedule_options_set_interval(SclScheduleOptions obj, SclTime interval)
{
    in<ScheduleOptions>(obj)->interval = interval;
}

// Send options

void scl_send_options_set_kind(SclSendOptions obj, SclMessageKind kind)
{
    in<SendOptions>(obj)->kind = static_cast<MessageKind>(kind);
}

void scl_send_options_set_processors(SclSendOptions obj, SclAudioProcessor *procs, int len)
{
    in<SendOptions>(obj)->processors = importList<AudioProcessor>(procs, len);
}

void scl_send_options_set_devices(SclSendOptions obj, SclMidiDevice *devices, int len)
{
    in<SendOptions>(obj)->devices = importList<MidiDevice>(devices, len);
}

void scl_send_options_set_channels(SclSendOptions obj, int *channels, int len)
{
    in<SendOptions>(obj)->channels = importList<int>(channels, len);
}


// Receive options

void scl_receive_options_set_kind(SclReceiveOptions obj, SclMessageKind kind)
{
    in<ReceiveOptions>(obj)->kind = static_cast<MessageKind>(kind);
}

void scl_receive_options_set_processors(SclReceiveOptions obj, SclAudioProcessor *procs, int len)
{
    in<ReceiveOptions>(obj)->processors = importList<AudioProcessor>(procs, len);
}

void scl_receive_options_set_devices(SclReceiveOptions obj, SclMidiDevice *devices, int len)
{
    in<ReceiveOptions>(obj)->devices = importList<MidiDevice>(devices, len);
}

void scl_receive_options_set_channels(SclReceiveOptions obj, int *channels, int len)
{
    in<ReceiveOptions>(obj)->channels = importList<int>(channels, len);
}


// Schedule options

SclTimeUnit  scl_schedule_options_get_unit(SclScheduleOptions obj)
{
    return in<ScheduleOptions>(obj)->unit;
}

SclFutureGroup* scl_schedule_options_get_groups(SclScheduleOptions obj, int* len)
{
    return exportList<FutureGroup>(in<ScheduleOptions>(obj)->groups, len);
}

int  scl_schedule_options_get_repeats(SclScheduleOptions obj)
{
    return in<ScheduleOptions>(obj)->repeats;
}

SclTime  scl_schedule_options_get_interval(SclScheduleOptions obj)
{
    return in<ScheduleOptions>(obj)->interval;
}


// Send options

SclMessageKind  scl_send_options_get_kind(SclSendOptions obj)
{
    return in<SendOptions>(obj)->kind;
}

SclAudioProcessor* scl_send_options_get_processors(SclSendOptions obj, int* len)
{
    return exportList<AudioProcessor>(in<SendOptions>(obj)->processors, len);
}

SclMidiDevice* scl_send_options_get_devices(SclSendOptions obj, int* len)
{
    return exportList<MidiDevice>(in<SendOptions>(obj)->devices, len);
}

int* scl_send_options_get_channels(SclSendOptions obj, int* len)
{
    return exportList<int,int>(in<SendOptions>(obj)->channels, len);
}


// Receive options

SclMessageKind  scl_receive_options_get_kind(SclReceiveOptions obj)
{
    return in<ReceiveOptions>(obj)->kind;
}

SclAudioProcessor* scl_receive_options_get_processors(SclReceiveOptions obj, int* len)
{
    return exportList<AudioProcessor>(in<ReceiveOptions>(obj)->processors, len);
}

SclMidiDevice* scl_receive_options_get_devices(SclReceiveOptions obj, int* len)
{
    return exportList<MidiDevice>(in<ReceiveOptions>(obj)->devices, len);
}

int* scl_receive_options_get_channels(SclReceiveOptions obj, int* len)
{
    return exportList<int,int>(in<ReceiveOptions>(obj)->channels, len);
}


// =============================================================================

class CallbackAction : public Action
{
public:
    void accept(Time time)
    {
        callback(time);
    }
    
    void accept(Time time, MessageInfo info)
    {
        callback(time);
    }
    
    CallbackAction(SclAction callback) : callback(callback) {}
    SclAction callback;
};

class CallbackReceiver : public Receiver
{
public:
    void accept(Time time, Message msg, MessageInfo info)
    {    
        int len;
        SclAtom* extMsg = exportList(list::copyAll(msg), &len);
        callback(time, extMsg, len);
    }
    
    CallbackReceiver(SclReceiver callback) : callback(callback) {}
private:
    SclReceiver callback;
};


// TODO memory management: reclaim actions
// they should be valid as long as they point to a reachable function




SclFuture scl_schedule_now(SclStream stream, SclAction callback, 
                           SclScheduleOptions opts, SclStreamError* err)
{
    try 
    {
        ActionScheduler* scheduler = in<Stream>(stream)->actionScheduler();
        CallbackAction* action = new CallbackAction(callback); 
        ScheduleOptions options;
        if (opts != NULL) options = *in<ScheduleOptions>(opts);
    
        return ex<Future>(acquire(scheduler->scheduleNow(action, options)));
    }
    catch (StreamError& e) 
    {
        *err = new StreamError(e);
    }
    return NULL;
}

SclFuture scl_schedule_later(SclStream stream, SclTime time, SclAction callback, 
                             SclScheduleOptions opts, SclStreamError* err)
{
    try 
    {
        ActionScheduler* scheduler = in<Stream>(stream)->actionScheduler();
        CallbackAction* action = new CallbackAction(callback); 
        ScheduleOptions options;
        if (opts != NULL) options = *in<ScheduleOptions>(opts);
    
        return ex<Future>(acquire(scheduler->scheduleLater(action, time, options)));
    }
    catch (StreamError& e) 
    {
        *err = new StreamError(e);
    }
    return NULL;
}

SclFuture scl_schedule_at(SclStream stream, SclTime time, SclAction callback, 
                          SclScheduleOptions opts, SclStreamError* err)
{
    try
    {
        ActionScheduler* scheduler = in<Stream>(stream)->actionScheduler();
        CallbackAction* action = new CallbackAction(callback); 
        ScheduleOptions options;
        if (opts != NULL) options = *in<ScheduleOptions>(opts);

        return ex<Future>(acquire(scheduler->scheduleAt(action, time, options)));
    }
    catch (StreamError& e) 
    {
        *err = new StreamError(e);
    }
    return NULL;
}


SclFuture scl_send_now(SclStream stream, SclAtom *message, int len, 
                      SclSendOptions opts, SclStreamError* err)
{    
    try
    {
        MessageScheduler* scheduler = in<Stream>(stream)->messageScheduler();
        SendOptions options;
        if (opts != NULL) options = *in<SendOptions>(opts);
    
        Message msg = list::dereferenceAll(importList<Atom>(message, len));    
        return ex<Future>(acquire(scheduler->sendNow(list::create(msg), options)));
    }
    catch (StreamError& e) 
    {
        *err = new StreamError(e);
    }
    return NULL;
}

SclFuture scl_send_later(SclStream stream, SclTime time, SclAtom *message, int len, 
                         SclSendOptions opts, SclStreamError* err)
{
    try 
    {
        MessageScheduler* scheduler = in<Stream>(stream)->messageScheduler();
        SendOptions options;
        if (opts != NULL) options = *in<SendOptions>(opts);
    
        Message msg = list::dereferenceAll(importList<Atom>(message, len));    
        return ex<Future>(acquire(scheduler->sendLater(time, list::create(msg), options)));
    }
    catch (StreamError& e) 
    {
        *err = new StreamError(e);
    }
    return NULL;
}

SclFuture scl_send_at(SclStream stream, SclTime time, SclAtom *message, int len, 
                      SclSendOptions opts, SclStreamError* err)
{
    try 
    {
        MessageScheduler* scheduler = in<Stream>(stream)->messageScheduler();
        SendOptions options;
        if (opts != NULL) options = *in<SendOptions>(opts);
    
        Message msg = list::dereferenceAll(importList<Atom>(message, len));    
        return ex<Future>(acquire(scheduler->sendAt(time, list::create(msg), options)));
    }
    catch (StreamError& e) 
    {
        *err = new StreamError(e);
    }
    return NULL;
}


SclFuture scl_receive(SclStream stream, SclReceiver callback, 
                      SclReceiveOptions opts, SclStreamError* err)
{
    try 
    {
        MessageScheduler* scheduler = in<Stream>(stream)->messageScheduler();
        CallbackReceiver* receiver = new CallbackReceiver(callback);
        ReceiveOptions options;
        if (opts != NULL) options = *in<ReceiveOptions>(opts);
    
        return ex<Future>(acquire(scheduler->receive(receiver, options)));
    }
    catch (StreamError& e) 
    {
        *err = new StreamError(e);
    }
    return NULL;
}
    


// =============================================================================


} // extern
