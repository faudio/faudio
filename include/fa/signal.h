
#ifndef _FA_SIGNAL
#define _FA_SIGNAL

#include <fa.h>
#include <fa/pair.h>
#include <fa/time.h>
#include <fa/buffer.h>

/** @addtogroup FaSignal

    @addtogroup FaSignal
    
    Provides signals.

    @par Implements
    - fa_copy_t
    - fa_string_show_t
    - fa_destroy_t
    
 
    @defgroup Fa Fa
    @{
    @defgroup FaSignal Signal
    @{
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
fa_signal_t fa_signal_constant(double);

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
fa_signal_t fa_signal_lift(fa_string_t,
                           fa_signal_unary_double_t,
                           fa_ptr_t,
                           fa_signal_t);

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
fa_signal_t fa_signal_lift2(fa_string_t,
                            fa_signal_binary_double_t,
                            fa_ptr_t,
                            fa_signal_t,
                            fa_signal_t);

/**
    Returns a signal that closes over the given signal function in a feedback loop.

    The given signal function receives its own output with an implicit one-sample delay.
    
    @par Semantic $$y(f)(t) = f(y(t-1))$$.
*/
fa_signal_t fa_signal_loop(fa_signal_unary_signal_t, fa_ptr_t);

/**
    Returns a signal that delays the given signal by the given number of 
    samples. 
            
    @par Semantic $$y(n,a)(t) = \\begin{cases} 0 & \mbox{if} (t-n) < 0   \\\\   a(t-n) & \mbox{if} (t-n) \\geq 0 \\end{cases}$$.

*/
fa_signal_t fa_signal_delay(int, fa_signal_t);

/** The primitive input signal, reading from the bus of the given number.
*/
fa_signal_t fa_signal_input(int);

/** The primitive output signal, writing to the bus of the given number
    and returning the written value.
*/
fa_signal_t fa_signal_output(int, int, fa_signal_t);

/** Returns a signal that evaluates both of the given signal, and returns
    the result of the first one.
*/
fa_signal_t fa_signal_latter(fa_signal_t, fa_signal_t);

/** Returns a signal that evaluates both of the given signal, and returns
    the result of the second one.
*/
fa_signal_t fa_signal_former(fa_signal_t, fa_signal_t);

/** Run the given signal for *n* samples, printing the values to `stdout`.
*/
void fa_signal_print(int, fa_list_t, fa_signal_t);

/** Run the given signal for *n* samples, writing the results to the given buffer.

    The given pointer must point to a buffer of at least `samples * sizeof(double)`.

    @param samples
        Number of samples to generate.
    @param controls
        List of control values (must be pairs of @ref fa_time_t and @ref fa_action_t).
        Optional, a null pointer is interpreted as the empty list.
    @param signal
        Signal to run.
    @param buffer
        Buffer to receive result.
*/
void fa_signal_run(int, fa_list_t, fa_signal_t, double *);

/** Run the given signal, writing the results to a freshly created @ref buffer_t.
    The resulting buffer must be freed by the caller.

    @param samples
        Number of samples to generate.
    @param controls
        List of control values (must be pairs of fa_time_t and fa_action_t).
        Optional, a null pointer is interpreted as the empty list.
    @param signal
        Signal to run.
*/
fa_buffer_t fa_signal_run_buffer(int, fa_list_t, fa_signal_t);

/** Run the given signal, writing the results to the given file.

    @param samples
        Number of samples to generate.
    @param controls
        List of control values (must be pairs of fa_time_t and fa_action_t).
        Optional, a null pointer is interpreted as the empty list.
    @param signal
        Signal to run.
    @param path
        Name of file to write.
*/
fa_ptr_t fa_signal_run_file(int,
                            fa_list_t,
                            fa_signal_t,
                            fa_string_t);

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
fa_signal_t fa_signal_play(fa_buffer_t, fa_signal_t);

/**
    Index a buffer at the given sample and returns the written value.


    @param buffer
        Buffer to write to.
    @param index
        Index to write to. If negative or larger than buffer size, nothing is written.
    @param value
        Value to write.
*/
fa_signal_t fa_signal_record(fa_buffer_t, fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_add(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_subtract(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_multiply(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_power(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_divide(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_modulo(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_absolute(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_not();


fa_signal_t fa_signal_and(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_or(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_xor(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_bit_not(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_bit_and(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_bit_or(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_bit_xor(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_shift_left(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_shift_right(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_equal(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_less_than(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_greater_than(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_less_than_equal(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_greater_than_equal(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_acos(fa_signal_t);


fa_signal_t fa_signal_asin(fa_signal_t);


fa_signal_t fa_signal_atan(fa_signal_t);


fa_signal_t fa_signal_cos(fa_signal_t);


fa_signal_t fa_signal_sin(fa_signal_t);


fa_signal_t fa_signal_tan(fa_signal_t);


fa_signal_t fa_signal_exp(fa_signal_t);


fa_signal_t fa_signal_log(fa_signal_t);


fa_signal_t fa_signal_log10(fa_signal_t);


fa_signal_t fa_signal_pow(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_sqrt(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_abs(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_min(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_max(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_fmod(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_remainder(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_floor(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_ceil(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_rint(fa_signal_t, fa_signal_t);

/** Run a signal through an external VST plug-in.
    
    @param name
        Name of plug-in.
    @param path
        Path of plug-in.
    @param inputs
        Inputs signals.
    @return
        A list of @ref fa_signal_t (outputs).
*/
fa_list_t fa_signal_vst(fa_string_t, fa_string_t, fa_list_t);

/** Run a signal through an external VST plug-in.
    
    @param name
        Name of plug-in.
    @return
        A list of @ref fa_signal_t (outputs).
*/
fa_list_t fa_signal_fluid(fa_string_t);

/** Convert the signal to a tree represented as set of
    nested pairs of type `(String,[...])`.
    
    This is useful for debugging the signal graph.
*/
fa_pair_t fa_signal_to_tree(fa_signal_t);

/** Convert a tree on the form `(String,[...])` to a string,
    suitable for printing.
*/
fa_string_t fa_signal_draw_tree(fa_pair_t);

/** Simplify a signal by removing all non-primitive constructors.
    The returned signal must be freed by the caller.

    @returns
        A simplified signal. If the given signal was already simplified,
        a copy of that signal (as per @ref fa_copy) is returned.
*/
fa_signal_t fa_signal_simplify(fa_signal_t);


fa_signal_t fa_signal_impulse();


fa_signal_t fa_signal_line(double);


fa_signal_t fa_signal_low_pass(fa_signal_t,
                               fa_signal_t,
                               fa_signal_t,
                               fa_signal_t,
                               fa_signal_t);


fa_signal_t fa_signal_biquad(fa_signal_t,
                             fa_signal_t,
                             fa_signal_t,
                             fa_signal_t,
                             fa_signal_t,
                             fa_signal_t);

/** @}
    @}
    */

#endif // _FA_SIGNAL

