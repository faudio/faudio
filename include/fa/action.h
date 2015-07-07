
#ifndef _FA_ACTION
#define _FA_ACTION

#include <fa.h>
#include <fa/pair.h>
#include <fa/list.h>
#include <fa/signal.h>

/** @addtogroup FaAction
 
    Scheduling actions.
    
    The action types represent a single interaction with a running stream, such
    as a note or a control change.
    
    Actions can be *scheduled* on both real-time and non-real-time 
    devices: in the first case they are carried out when the internal clock of
    the stream reaches the scheduled time, in the latter case they are simply
    carried out at the appropriate time.
    
    For scheduling actions, see @ref fa_audio_schedule, @ref fa_midi_schedule
    or @ref fa_signal_run.

    @par Literals
    - `set`
    - `get`
    - `accum`
    - `send`

    @par Implements 
    - fa_copy_t
    - fa_destroy_t
    - fa_string_show_t

    @see 

 
    @defgroup Fa Fa
    @{
    @defgroup FaAction Action
    @{
    */



/** The abstract type of actions.
    
*/
typedef struct _fa_action_t * fa_action_t;

/** A nullary function that also receives time.
*/
typedef fa_ptr_t (* fa_action_nullary_with_time_t)(fa_ptr_t,
                                                   fa_time_t);

/** A predicate that also receives time.
*/
typedef bool (* fa_action_pred_with_time_t)(fa_ptr_t,
                                            fa_time_t,
                                            fa_ptr_t);

/** Channel on which to carry out the action.
    
*/
typedef int fa_action_channel_t;

/** Name of external processor to receive the action.
    
*/
typedef fa_string_t fa_action_name_t;

/** Value to send.
    
*/
typedef fa_ptr_t fa_action_value_t;

/** The `null` action that does nothing.
*/
fa_action_t fa_action_null();

/** Copy the given action.
*/
fa_action_t fa_action_copy(fa_action_t action);

/** Copy the given action recursivly.
*/
fa_action_t fa_action_deep_copy(fa_action_t action);

/** Destroy the given action.
*/
void fa_action_destroy(fa_action_t action);

/** Destroy the given action recursivly.
*/
void fa_action_deep_destroy(fa_action_t action, fa_deep_destroy_pred_t pred);


/** The `get` action reads a single global bus.

    The resulting action must be destroyed by the caller.
    
    @param channel
        Channel to read from.
    @param function
        Function to receive value.
    @param data
        Context pointer.
*/
fa_action_t fa_action_get(fa_action_channel_t channel,
                          fa_signal_unary_double_t unaryDouble,
                          fa_ptr_t ptr);

/** The `set` action updates a single global bus.

    The resulting action must be destroyed by the caller.
    
    @param channel
        Channel to update.
    @param value
        Value to set.
*/
fa_action_t fa_action_set(fa_action_channel_t channel,
                          double double_);

/** The `accum` action updates a single global bus by applying
    a function to its previous value.

    The resulting action must be destroyed by the caller.
    
    @param channel
        Channel to update.
    @param function
        Function to apply.
    @param data
        Data closed over by the function.
*/
fa_action_t fa_action_accum(fa_action_channel_t channel,
                            fa_signal_unary_double_t unaryDouble,
                            fa_ptr_t ptr);

/** The `send` action sends a message, the type of which depends on
    the type of receiver. Often this is @ref fa_midi_message_t or 
    a value implementing @ref fa_dynamic_t.

    The resulting action must be destroyed by the caller.
    
    @param name
        Name of receiver.
    @param value
        Value to send.
*/
fa_action_t fa_action_send(fa_action_name_t name,
                           fa_action_value_t value);

/** Return whether the given action is a get action.
      
*/
bool fa_action_is_get(fa_action_t action);

/** Get the channel of a get action.
*/
fa_action_channel_t fa_action_get_channel(fa_action_t action);

/** Get the function of a get action.
*/
fa_signal_unary_double_t fa_action_get_function(fa_action_t action);

/** Get the data of a get action.
*/
fa_ptr_t fa_action_get_data(fa_action_t action);

/** Return whether the given action is a set action.
      
*/
bool fa_action_is_set(fa_action_t action);

/** Get the channel of a set action.
*/
fa_action_channel_t fa_action_set_channel(fa_action_t action);

/** Get the value of a set action.
*/
double fa_action_set_value(fa_action_t action);

/** Return whether the given action is an accumulation action.
      
*/
bool fa_action_is_accum(fa_action_t action);

/** Get the channel of an accumulation action.
*/
fa_action_channel_t fa_action_accum_channel(fa_action_t action);

/** Get the function of an accumulation action.
*/
fa_signal_unary_double_t fa_action_accum_function(fa_action_t action);

/** Get the data of an accumulation action.
*/
fa_ptr_t fa_action_accum_data(fa_action_t action);

/** Return whether the given action is a send action.
      
*/
bool fa_action_is_send(fa_action_t action);

/** Get the name of an send action.
*/
fa_action_name_t fa_action_send_name(fa_action_t action);

/** Get the value of an send action.
*/
fa_action_value_t fa_action_send_value(fa_action_t action);

/** Repeat the given action indefinitely.
*/
fa_action_t fa_action_repeat(fa_time_t interval,
                             fa_action_t action);

/** Join a list of actions into a single compond action.
    TODO document
*/
fa_action_t fa_action_many(fa_list_t actions);

/** Creates a derived action from the given action that executes íf and only given predicate holds.
    The predicate function is called for every occurence.
*/
fa_action_t fa_action_if(fa_pred_t pred,
                         fa_ptr_t predData,
                         fa_action_t action);

/** Creates a derived action from the given action that executes as long as the given predicate holds.
    The predicate function is called for every occurence.
*/
fa_action_t fa_action_while(fa_pred_t pred,
                            fa_ptr_t predData,
                            fa_action_t action);

/** Creates a derived action from the given action that executes as long as the given predicate
    does *not* hold.
    
    The predicate function is called for every occurence.
*/
fa_action_t fa_action_until(fa_pred_t pred,
                            fa_ptr_t predData,
                            fa_action_t action);

/** Creates a derived action from the given action that executes íf and only given predicate holds.
    The predicate function is called for every occurence.
    
    This is exactly like @ref fa_action_if, except that it also includes time.
*/
fa_action_t fa_action_if_with_time(fa_action_pred_with_time_t pred,
                                   fa_ptr_t predData,
                                   fa_action_t action);

/** Creates a derived action from the given action that executes as long as the given predicate holds.
    The predicate function is called for every occurence.
    
    This is exactly like @ref fa_action_while, except that it also includes time.
*/
fa_action_t fa_action_while_with_time(fa_action_pred_with_time_t pred,
                                      fa_ptr_t predData,
                                      fa_action_t action);

/** Creates a derived action from the given action that executes as long as the given predicate
    does *not* hold.
    
    The predicate function is called for every occurence.

    This is exactly like @ref fa_action_until, except that it also includes time.
*/
fa_action_t fa_action_until_with_time(fa_action_pred_with_time_t pred,
                                      fa_ptr_t predData,
                                      fa_action_t action);

/** Convert a unary function to an action.
*/
fa_action_t fa_action_do(fa_nullary_t nullary, fa_ptr_t ptr);

/** Convert a unary function to an action.
    
    This is exactly like @ref fa_action_do, except that it also includes time.
*/
fa_action_t fa_action_do_with_time(fa_action_nullary_with_time_t nullaryWithTime,
                                   fa_ptr_t ptr);

/** Returns whether the given action is simple or not.
*/
bool fa_action_is_simple(fa_action_t action);

/** Returns whether the given action is compound or not.
*/
bool fa_action_is_compound(fa_action_t action);

/** Returns whether the given action is compound or not.
*/
bool fa_action_is_do(fa_action_t action);

// /** Given a compound action, return the minimum offset to its tail.
// */
// fa_time_t fa_action_compound_interval(fa_action_t action);
//
// /** Given a compound action, return its head (nullable).
// */
// fa_action_t fa_action_compound_first(fa_action_t action);
//
// /** Given a compound action, return its tail (nullable).
// */
// fa_action_t fa_action_compound_rest(fa_action_t action);

//void print_all_actions();
void fa_log_action_count();

/** @}
    @}
    */

#endif // _FA_ACTION

