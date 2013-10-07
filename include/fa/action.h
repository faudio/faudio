
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

/** Channel on which to carry out the action.
    
*/
typedef int fa_action_channel_t;

/** Name of external processor to receive the action.
    
*/
typedef fa_string_t fa_action_name_t;

/** Value to send.
    
*/
typedef fa_ptr_t fa_action_value_t;

/** The `set` action updates a single global bus.

    The resulting action must be destroyed by the caller.
    
    @param channel
        Channel to update.
    @param value
        Value to set.
*/
fa_action_t fa_action_set(fa_action_channel_t, double);

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
fa_action_t fa_action_accum(fa_action_channel_t,
                            fa_signal_unary_double_t,
                            fa_ptr_t);

/** The `send` action sends a message, the type of which depends on
    the type of receiver. Often this is @ref fa_midi_message_t or 
    a value implementing @ref fa_dynamic_t.

    The resulting action must be destroyed by the caller.
    
    @param name
        Name of receiver.
    @param value
        Value to send.
*/
fa_action_t fa_action_send(fa_action_name_t, fa_action_value_t);

/** Return whether the given action is a set action.
      
*/
bool fa_action_is_set(fa_action_t);


fa_action_channel_t fa_action_set_channel(fa_action_t);


double fa_action_set_value(fa_action_t);

/** Return whether the given action is an accum action.
      
*/
bool fa_action_is_accum(fa_action_t);


fa_action_channel_t fa_action_accum_channel(fa_action_t);


fa_signal_unary_double_t fa_action_accum_function(fa_action_t);


fa_ptr_t fa_action_accum_data(fa_action_t);

/** Return whether the given action is a send action.
      
*/
bool fa_action_is_send(fa_action_t);


fa_action_name_t fa_action_send_name(fa_action_t);


fa_action_value_t fa_action_send_value(fa_action_t);


fa_action_t fa_action_copy(fa_action_t);


void fa_action_destroy(fa_action_t);

/** @}
    @}
    */

#endif // _FA_ACTION

