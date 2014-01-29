
#ifndef _FA_SIGNAL
#define _FA_SIGNAL

#include <fa.h>
#include <fa/pair.h>
#include <fa/time.h>
#include <fa/buffer.h>
#include <fa/atomic/ring_buffer.h>

/** @addtogroup FaSignal

    Provides signals.

    @par Implements
    - fa_copy_t
    - fa_string_show_t
    - fa_number_t
    - fa_destroy_t
    
 
    @defgroup Fa Fa
    @{
    @defgroup FaSignal Signal
    @{
    */

/** The abstract type of signals.
    
    Each signals denotes a function of time.
*/
typedef struct _fa_signal_t * fa_signal_t;

/** Like fa_unary_t, but speficied on signals.
    
*/
typedef fa_signal_t (* fa_signal_unary_signal_t)(fa_ptr_t,
                                                 fa_signal_t);

/** Like fa_unary_t, but speficied on doubles.
    
*/
typedef double (* fa_signal_unary_double_t)(fa_ptr_t, double);

/** Like fa_binary_t, but speficied on doubles.
    
*/
typedef double (* fa_signal_binary_double_t)(fa_ptr_t,
                                             double,
                                             double);

/**
    Returns a signal representing the elapsed time in seconds.
    
    @par Semantic $$y(t) = t$$.
*/
fa_signal_t fa_signal_time();

/**
    Returns a signal representing white noise.
    
    @par Semantic $$y(t) = random(-1,1)$$.
*/
fa_signal_t fa_signal_random();

/**
    Returns a signal representing the given constant value.

    @param value
        Constant value.
    @par Semantic $$y(c)(t) = c$$.
*/
fa_signal_t fa_signal_constant(double double_);

/**
    Returns a signal that applies the given function to output of the given signal.
    
    @param name          
        Name of resulting processor. This is used for printing and some optimization techniques.
    @param function, data
        Function to lift and pointer to data closed over by the function.
    @param signal
        Signal to apply to the resulting processor.
    @return
        Result of appying the given processor to the given signal.

    @par Semantic $$y(f,a)(t) = f(a(t))$$.
*/
fa_signal_t fa_signal_lift(fa_string_t string,
                           fa_signal_unary_double_t unaryDouble,
                           fa_ptr_t ptr,
                           fa_signal_t signal);

/**
    Returns a signal that applies the given function to output of the given signals.
    
    Returns a signal that applies the given function to output of the given signal.
    
    @param name          
        Name of resulting processor. This is used for printing and some optimization techniques.
    @param function, data
        Function to lift and pointer to data closed over by the function.
    @param signal1, signal2
        Signals to apply to the resulting processor.
    @return
        Result of appying the given processor to the given signals.

    @par Semantic $$y(f,a,b)(t) = f(a(t), b(t))$$.
*/
fa_signal_t fa_signal_lift2(fa_string_t string,
                            fa_signal_binary_double_t binaryDouble,
                            fa_ptr_t ptr,
                            fa_signal_t signal,
                            fa_signal_t signal_);

/**
    Returns a signal that closes over the given signal function in a feedback loop.

    The given signal function receives its own output with an implicit one-sample delay.
    
    @par Semantic $$y(f)(t) = f(y(t-1))$$.
*/
fa_signal_t fa_signal_loop(fa_signal_unary_signal_t unarySignal,
                           fa_ptr_t ptr);

/**
    Returns a signal that delays the given signal by the given number of 
    samples. 
            
    @par Semantic $$y(n,a)(t) = \\begin{cases} 0 & \mbox{if} (t-n) < 0   \\\\   a(t-n) & \mbox{if} (t-n) \\geq 0 \\end{cases}$$.

*/
fa_signal_t fa_signal_delay(int int_, fa_signal_t signal);


typedef fa_string_t fa_signal_name_t;


typedef fa_ptr_t fa_signal_message_t;


typedef struct {
            double * buffer; fa_ptr_t dummy; int count; double rate;
        } fa_signal_state_t;


typedef struct {
            fa_ptr_t (* before)(fa_ptr_t, int, fa_signal_state_t *);
            fa_ptr_t (* after)(fa_ptr_t, int, fa_signal_state_t *);
            fa_ptr_t (* render)(fa_ptr_t, int, fa_signal_state_t *);
            fa_ptr_t (* receive)(fa_ptr_t,
                                 fa_signal_name_t,
                                 fa_signal_message_t);
            fa_ptr_t data;
        } fa_signal_custom_processor_t;

/** Add a custom processor to be executed with the given signal.
    
    A custom processor is simply a routine invoked on the DSP thread. It has access
    to current sampling rate, vector size and all input and output channels.
    The *before* and *after* methods are invoked in non-realtime mode and may allocate
    memory, perform I/O etc. The *receive* and *render* methods are invoked in realtime
    mode, and the usual restrictions apply. If you need to do I/O or similar during 
    processing, use the @ref fa_audio_add_input_output_callback instead.

    Adding a custom processor to a signal does *not* affect its input or output, the
    user must allocate dedicated buses for this purpose: normally a custom processor
    would be wrapped in a higher-level function which uses `fa_signal_input` and
    `fa_signal_output` to read and write to the corresponding channels.
    
    If a processor handles multichannel audio, simply add the processor to *one* of
    the output signals, implying that all channels of the output must always be used
    whether their output is needed or not (see also `fa_signal_former`).

    @warning
        You probably do not want to do this. This is a very low-level function used
        internally in faudio for implementing new I/O backends, plug-in formats. If
        you simply want to lift a pure function into the audio thread, see `fa_signal_lift`.            
*/
fa_signal_t fa_signal_custom(fa_signal_custom_processor_t *,
                             fa_signal_t signal);

/** The primitive input signal, reading from the bus of the given number.
*/
fa_signal_t fa_signal_input(int int_);

/** The primitive output signal, writing to the bus of the given number
    and returning the written value.
*/
fa_signal_t fa_signal_output(int int_,
                             int int__,
                             fa_signal_t signal);

/** Returns a signal that evaluates both of the given signal, and the result of the first.
*/
fa_signal_t fa_signal_former(fa_signal_t signal,
                             fa_signal_t signal_);

/** Returns a signal that evaluates both of the given signal, and returns the result of the second.
*/
fa_signal_t fa_signal_latter(fa_signal_t signal,
                             fa_signal_t signal_);

/** Run the given signal for *n* samples, printing the values to `stdout`.
*/
void fa_signal_print(int int_, fa_list_t list, fa_signal_t signal);

/** Run the given signal for *n* samples, writing the results to the given buffer.

    The given pointer must point to a buffer of at least `samples * sizeof(double)`.

    @param samples
        Number of samples to generate.
    @param controls
        List of control values (must be pairs of @ref fa_time_t and @ref fa_action_t).
        Optionally, a null pointer is interpreted as the empty list.
    @param signal
        Signal to run.
    @param buffer
        Buffer to receive result.
*/
void fa_signal_run(int int_,
                   fa_list_t list,
                   fa_signal_t signal,
                   double *);

/** Run the given signal, writing the results to a freshly created @ref buffer_t.
    The resulting buffer must be freed by the caller.

    @param samples
        Number of samples to generate.
    @param controls
        List of control values (must be pairs of fa_time_t and fa_action_t).
        Optionally, a null pointer is interpreted as the empty list.
    @param signal
        Signal to run.
*/
fa_buffer_t fa_signal_run_buffer(int int_,
                                 fa_list_t list,
                                 fa_signal_t signal);

/** Run the given signal, writing the results to the given file.

    @param samples
        Number of samples to generate.
    @param controls
        List of control values (must be pairs of fa_time_t and fa_action_t).
        Optionally, a null pointer is interpreted as the empty list.
    @param signal
        Signal to run.
    @param path
        Name of file to write.
*/
fa_ptr_t fa_signal_run_file(int int_,
                            fa_list_t list,
                            fa_signal_t signal,
                            fa_string_t string);

/**
    Index a buffer at the given sample.

    This signal writes raw buffer indices, so to read a buffer *b*
    of *n* channels at channel *c* and sample *n*, do `record(b, i*n+c)`.

    The read is performed modulo `length(b)/8`, so negative or larger values wrap
    around. Thus you can loop a buffer by simply incrementing the index or play it
    backwards by decrementing.

    @param buffer
        Buffer to read from.
    @param index
        Sample index to read from.
*/
fa_signal_t fa_signal_play(fa_buffer_t buffer, fa_signal_t signal);

/**
    Index a buffer at the given sample and returns the written value.

    @param buffer
        Buffer to write to.
    @param index
        Index to write to. If negative or larger than buffer size, nothing is written.
    @param value
        Value to write.
*/
fa_signal_t fa_signal_record(fa_buffer_t buffer,
                             fa_signal_t signal,
                             fa_signal_t signal_);


fa_signal_t fa_signal_record_external(fa_string_t string,
                                      fa_signal_t signal);


fa_pair_t fa_signal_record_external2(fa_string_t string,
                                     fa_pair_t pair);

/** Addition lifted to signals. 
*/
fa_signal_t fa_signal_add(fa_signal_t signal, fa_signal_t signal_);

/** Subtraction lifted to signals. 
*/
fa_signal_t fa_signal_subtract(fa_signal_t signal,
                               fa_signal_t signal_);

/** Multiplication lifted to signals. 
*/
fa_signal_t fa_signal_multiply(fa_signal_t signal,
                               fa_signal_t signal_);

/** The exponential function lifted to signals. 
*/
fa_signal_t fa_signal_power(fa_signal_t signal,
                            fa_signal_t signal_);

/** Division function lifted to signals. 
*/
fa_signal_t fa_signal_divide(fa_signal_t signal,
                             fa_signal_t signal_);

/** The modulo function lifted to signals. 
*/
fa_signal_t fa_signal_modulo(fa_signal_t signal,
                             fa_signal_t signal_);

/** The absolute value of a signal. 
*/
fa_signal_t fa_signal_absolute(fa_signal_t signal);

/** Negate a signal, treating 0 as false and all other values as true. 
*/
fa_signal_t fa_signal_not();

/** Logical *and* of two signals, treating 0 as false and all other values as true. 
*/
fa_signal_t fa_signal_and(fa_signal_t signal, fa_signal_t signal_);

/** Logical *or* of two signals, treating 0 as false and all other values as true. 
*/
fa_signal_t fa_signal_or(fa_signal_t signal, fa_signal_t signal_);

/** Logical *exclusive or* of two signals, treating 0 as false and all other values as true. 
*/
fa_signal_t fa_signal_xor(fa_signal_t signal, fa_signal_t signal_);

/** Equality of two signals, generating 1 if equal and 0 otherwise. 
    Beware of floating-point equality. You should only use small integer numbers. 
*/
fa_signal_t fa_signal_equal(fa_signal_t signal,
                            fa_signal_t signal_);

/** Compare two signals `x` and `y`, generating 1 if `x < y` and 0 otherwise. 
*/
fa_signal_t fa_signal_less_than(fa_signal_t signal,
                                fa_signal_t signal_);

/** Compare two signals `x` and `y`, generating 1 if `x > y` and 0 otherwise. 
*/
fa_signal_t fa_signal_greater_than(fa_signal_t signal,
                                   fa_signal_t signal_);

/** Compare two signals `x` and `y`, generating 1 if `x <= y` and 0 otherwise. 
*/
fa_signal_t fa_signal_less_than_equal(fa_signal_t signal,
                                      fa_signal_t signal_);

/** Compare two signals `x` and `y`, generating 1 if `x >= y` and 0 otherwise. 
*/
fa_signal_t fa_signal_greater_than_equal(fa_signal_t signal,
                                         fa_signal_t signal_);

/** The acos function lifted to signals. 
*/
fa_signal_t fa_signal_acos(fa_signal_t signal);

/** The asin function lifted to signals. 
*/
fa_signal_t fa_signal_asin(fa_signal_t signal);

/** The atan function lifted to signals. 
*/
fa_signal_t fa_signal_atan(fa_signal_t signal);

/** The cos function lifted to signals. 
*/
fa_signal_t fa_signal_cos(fa_signal_t signal);

/** The sin function lifted to signals. 
*/
fa_signal_t fa_signal_sin(fa_signal_t signal);

/** The tan function lifted to signals. 
*/
fa_signal_t fa_signal_tan(fa_signal_t signal);

/** The exp function lifted to signals. 
*/
fa_signal_t fa_signal_exp(fa_signal_t signal);

/** The natural logarithm of a signal. 
*/
fa_signal_t fa_signal_log(fa_signal_t signal);

/** The common logarithm of a signal. 
*/
fa_signal_t fa_signal_log10(fa_signal_t signal);

/** The square root of a signal. 
*/
fa_signal_t fa_signal_sqrt(fa_signal_t signal);

/** The minimum of two signals. 
*/
fa_signal_t fa_signal_min(fa_signal_t signal, fa_signal_t signal_);

/** The maximum of two signals. 
*/
fa_signal_t fa_signal_max(fa_signal_t signal, fa_signal_t signal_);

/** The modulo of a signal. 
*/
fa_signal_t fa_signal_fmod(fa_signal_t signal, fa_signal_t signal_);

/** The remainder of a signal. 
*/
fa_signal_t fa_signal_remainder(fa_signal_t signal,
                                fa_signal_t signal_);

/** Round the value of a signal towards negative infinity. 
*/
fa_signal_t fa_signal_floor(fa_signal_t signal,
                            fa_signal_t signal_);

/** Round the value of a signal towards positive infinity. 
*/
fa_signal_t fa_signal_ceil(fa_signal_t signal, fa_signal_t signal_);

/** A signal that counts samples.
    Generates the sequence `[0,1..]`.
*/
fa_signal_t fa_signal_counter();

/** A signal which is one when the number of samples is divisible by the given
    number, and zero otherwise.
    
    For example if the sample rate is 44100, `fa_signal_impulses(44100)` generates an impulse every second.
*/
fa_signal_t fa_signal_impulses(int int_);

/** Run a signal through an external VST plug-in.
    
    @param name
        Name of plug-in.
    @param path
        Path of plug-in.
    @param inputs
        Inputs signals.
    @return
        A list of @ref fa_signal_t (outputs).
    @warning
        Experimental.    
*/
fa_list_t fa_signal_vst(fa_string_t string,
                        fa_string_t string_,
                        fa_list_t list);

/** Returns a pair of signals from the `DLSMusicDevice`.
    You can send messages to it using the name `DLS`.
    
    @return
        A pair of @ref fa_signal_t (outputs).
    @note
        This function is only available on Mac OS X and will
        fail ungracefully on other platforms.
*/
fa_pair_t fa_signal_dls();

/** Returns a pair of signals from FluidSynth (if available).
    You can send messages to it using the name `Fluid`.
    
    @param path
        Path to a SoundFont.
    @return
        A pair of @ref fa_signal_t (outputs).
    @note
        This function is only available on Windows and will
        fail ungracefully on other platforms.
    @warning
        Experimental.    
*/
fa_pair_t fa_signal_synth(fa_string_t string);

/** Convert the signal to a tree represented as set of
    nested pairs of type `(String,[...])`.
    
    This is useful for debugging the signal graph.
*/
fa_pair_t fa_signal_to_tree(fa_signal_t signal);

/** Convert a tree on the form `(String,[...])` to a string,
    suitable for printing.
*/
fa_string_t fa_signal_draw_tree(fa_pair_t pair);

/** Simplify a signal by removing all non-primitive constructors.
    The returned signal must be freed by the caller.

    @returns
        A simplified signal. If the given signal was already simplified,
        a copy of that signal (as per @ref fa_copy) is returned.
*/
fa_signal_t fa_signal_simplify(fa_signal_t signal);


fa_signal_t fa_signal_impulse();


fa_signal_t fa_signal_line(double double_);

/** @}
    @}
    */

#endif // _FA_SIGNAL

