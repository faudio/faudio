
#ifndef _FA_MIDI_DEVICE
#define _FA_MIDI_DEVICE

#include <fa/list.h>
#include <fa/pair.h>
#include <fa/action.h>
#include <fa/time.h>
#include <fa/clock.h>
#include <fa/error.h>
#include <fa/midi/session.h>

/** @addtogroup FaMidiDevice

    Provides real-time MIDI.

    @par Sessions, devices and streams implement 
    - fa_destroy_t
    - fa_string_show_t

    @par Devices also implement 
    - fa_equal_t

    @see 
    - @ref Devices
 
    @defgroup Fa Fa
    @{
    @defgroup FaMidi Midi
    @{
    @defgroup FaMidiDevice Device
    @{
    */

/** A MIDI device.
*/
typedef struct _fa_midi_device_t * fa_midi_device_t;

/** Get the standard input device of the given session.
    @param session   The session.
    @return
        A device or an error if there are no input devices available.
*/
fa_midi_device_t fa_midi_default_input(fa_midi_session_t session);

/** Get the standard output device of the given session.
    @param session   The session.
    @return
        A device or an error if there are no output devices available.
*/
fa_midi_device_t fa_midi_default_output(fa_midi_session_t session);

/** Return the name of the given device.
    @param device   The device.
*/
fa_string_t fa_midi_name(fa_midi_device_t device);

/** Return the host name of the given device.
    @param device   The device.
*/
fa_string_t fa_midi_host_name(fa_midi_device_t device);

/** Return whether the given device has input or not.
    @param device   The device.
*/
bool fa_midi_has_input(fa_midi_device_t device);

/** Return whether the given device has output or not.
    @param device   The device.
*/
bool fa_midi_has_output(fa_midi_device_t device);

/** @}
    @}
    @}
    */

#endif // _FA_MIDI_DEVICE

