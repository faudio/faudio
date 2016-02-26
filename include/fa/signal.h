
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


typedef struct {
            double * buffer; fa_ptr_t dummy; uint64_t count; double rate;
        } fa_signal_state_t;

/** Type of names.
*/
typedef fa_string_t fa_signal_name_t;

/** Type of messages.
*/
typedef fa_ptr_t fa_signal_message_t;

/** A callback to receive messages.
*/
typedef void (* fa_signal_message_callback_t)(fa_ptr_t,
                                              fa_signal_name_t,
                                              fa_signal_message_t);


typedef struct {
            fa_ptr_t (* before)(fa_ptr_t, int, fa_signal_state_t *);
            fa_ptr_t (* after)(fa_ptr_t, int, fa_signal_state_t *);
            fa_ptr_t (* render)(fa_ptr_t data,
                                int channelOffset,
                                int numberOfSamples,
                                fa_signal_state_t * state);
            fa_ptr_t (* receive)(fa_ptr_t,
                                 fa_signal_name_t,
                                 fa_signal_message_t);
            fa_ptr_t (* send)(fa_ptr_t,
                              fa_signal_message_callback_t,
                              fa_ptr_t);
            fa_ptr_t (* destroy)(fa_ptr_t);
            int64_t channel_offset;
            fa_ptr_t data;
        } fa_signal_custom_processor_t;

/** Add a custom processor to be executed with the given signal.
    A custom processor is simply a routine invoked on the DSP thread.

    Adding a custom processor to a signal does *not* affect its input or output, the
    user must allocate dedicated buses for this purpose using @ref fa_signal_input_local
    or @ref fa_signal_output_local.
    
    If a processor handles multichannel audio, simply add the processor to *one* of
    the output signals, implying that all channels of the output must always be used
    whether their output is needed or not (see also `fa_signal_former`).

    @warning
        This is a low-level function used internally in faudio for implementing new
        I/O backends, plug-in formats. If you simply want to lift a pure function into
        the audio thread, see `fa_signal_lift`.

    @deprecated
        Use XXX instead.
*/
fa_signal_t fa_signal_custom(fa_signal_custom_processor_t *,
                             fa_signal_t signal);

/** An input signal, reading from the global bus of the given number.
    @param channel
        Channel number.
*/
fa_signal_t fa_signal_input(int channel);

/** An output signal, writing to the global bus of the given number.

    Returns a signal that is identical to original signal except that it
    enforces output.

    @param delay
        Delay of output in frames.
    @param channel
        Channel number.
    @param input
        Signal to output.
    @returns
        The original signal, which must be run for output to take place.
*/
fa_signal_t fa_signal_output(int delay,
                             int channel,
                             fa_signal_t input);

/** An output signal, writing to the global bus of the given number.

    Returns a signal that is identical to original signal except that it
    enforces output.

    @proc
        Processor to route to.
    @param channel
        Channel number (local).
*/
fa_signal_t fa_signal_local_input(fa_signal_custom_processor_t * proc,
                                  int channel);

/** The primitive output signal, writing to the bus of the given number
    and returning the written value.

    @param proc
        Processor to route from.
    @param delay
        Delay of output in frames.
    @param channel
        Channel number.
    @param input
        Signal to output.
    @returns
        The original signal, which must be run for output to take place.
*/
fa_signal_t fa_signal_local_output(fa_signal_custom_processor_t * proc,
                                   int delay,
                                   int channel,
                                   fa_signal_t input);

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


fa_signal_t fa_signal_trigger(fa_string_t name, double init);

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

/** A signal that plays multiple audio buffers.
    
    Control the signal by sending pairs to it (using @ref fa_action_send).
    The pair should consist of a slot number and a command, where the command can be either of:
    - A @ref fa_buffer_t -- the buffer is loaded into the signal
    - A @ref fa_file_buffer_t -- the file buffer is loaded into the signal
    - The string "play" -- start playback
    - The string "stop" -- stop playback
    - The string "free" -- unload the current buffer
    - A (wrapped) number -- move to the corresponding frame
    - A pair, consisting of the string "volume" and a float -- set volume for the slot (1.0 = reference volume)
    - A pair, consisting of the string "pan" and a float -- set pan for the slot (-1.0 to 1.0)
    
    @param name
        A name to identify the signal, use with @ref fa_action_send
    @param count
        Number of slots. Each slot is able to play one buffer at a time.
    @return
        A pair of @ref fa_signal_t (left and right output).
    
    @note
        Use @ref fa_action_send_retain rather than @fa_action_send to pass
        buffers to the signal, otherwise the buffer will be destroyed by
        the scheduler. (TODO: This will currently leak the pair and the number)
    @note
        Only one buffer or file_buffer can be loaded in each slot at a time.
        Loading a new buffer automatically unloads the previous.
    @note
        When a buffer or file_buffer is loaded, a reference is taken, which is
        released when the buffer is unloaded. Therefore it is safe calling
        @ref fa_destroy on the buffer even after scheduling.
*/

fa_pair_t fa_signal_play_buffers(fa_string_t name, int count);

/** A signal that plays audio buffers, one at a time.
    
    Control the signal by sending one of the following to it (using @ref fa_action_send):
    - A @ref fa_buffer_t -- the buffer is loaded into the signal
    - The string "play" -- start playback
    - The string "stop" -- stop playback
    - The string "free" -- unload the current buffer
    - A (wrapped) number -- move to the corresponding frame
    - A pair, consisting of the string "volume" and a float -- set volume for the slot (1.0 = reference volume)
    - A pair, consisting of the string "pan" and a float -- set pan for the slot (-1.0 to 1.0)
    
    @param name
        A name to identify the signal, use with @ref fa_action_send
    @return
        A pair of @ref fa_signal_t (left and right output).
    
    @note
        This is a convenience wrapper equivalent to fa_signal_play_buffers(name, 1)
        See @fa_signal_play_buffers for further information.
    
*/
fa_pair_t fa_signal_play_buffer(fa_string_t name);

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
fa_signal_t fa_signal_impulses(size_t samples);

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


void fa_signal_show_vst_gui(fa_string_t string, fa_ptr_t ptr);

/** Returns a pair of signals from the `DLSMusicDevice`.
    You can send messages to it using the name `DLS`.
    
    @return
        A pair of @ref fa_signal_t (outputs).
    @note
        This function is only available on Mac OS X and will
        fail ungracefully on other platforms.
*/
fa_pair_t fa_signal_dls(fa_string_t name);

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
fa_pair_t fa_signal_synth(fa_string_t name,
                          fa_string_t soundfontPath);

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

/** Optimize a signal (destructive).

    @returns
        A simplified signal. If the given signal was already simplified,
        a copy of that signal (as per @ref fa_copy) is returned.
*/
fa_signal_t fa_signal_doptimize(fa_signal_t signal);

/** Verify that the given signal can actually be run (destructive).
    Should be called *after* simplify and optimize.

    @returns
        A simplified signal. If the given signal was already simplified,
        a copy of that signal (as per @ref fa_copy) is returned.
*/
fa_signal_t fa_signal_dverify(fa_signal_t signal);


fa_signal_t fa_signal_impulse();


fa_signal_t fa_signal_line(double double_);

/** @deprecated Renamed to @ref fa_signal_local_input. 
*/
fa_signal_t fa_signal_input_with_custom(fa_signal_custom_processor_t * proc,
                                        int channel);

/** @deprecated Renamed to @ref fa_signal_local_output. 
*/
fa_signal_t fa_signal_output_with_custom(fa_signal_custom_processor_t * proc,
                                         int delay,
                                         int channel,
                                         fa_signal_t input);

/** @}
    @}
    */

#endif // _FA_SIGNAL

