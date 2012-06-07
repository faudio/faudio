@defgroup sclaudiol Lisp API 
@ingroup sclaudiol

Classes
-------

### object

Root class for object in the audio system.

### session

A set of available devices.

This class abstracts over single state API such as Portaudio and
Portmidi to provide multiple views of the devices available on the
system while not compromising functionality of the APIs.

See [Session](@ref doremir::scl::Session).

### midi-device

Represents a Midi device.

See [MidiDevice](@ref doremir::scl::MidiDevice).

### audio-host

Represents an audio host, i.e, CoreAudio, ASIO.

See [AudioHost](@ref doremir::scl::AudioHost).

### audio-device

Represents an audio device, i.e. a sound card or a virtual device.

See [AudioDevice](@ref doremir::scl::AudioDevice).

### audio-processor

An object that transforms audio signals. May be atomic or created by
combinators.

See [AudioProcessor](@ref doremir::scl::AudioProcessor).

### fluidsynth-processor

See [FluidSynth](@ref doremir::scl::FluidSynth).

### au-processor

See [AudioUnitProcessor](@ref doremir::scl::AudioUnitProcessor).

### vst-processor

See [VstProcessor](@ref doremir::scl::VstProcessor).

### device-stream-options

### stream

A synchronous audio or midi computation.

See [Stream](@ref doremir::scl::Stream).

### future

A future represents a computation that have been scheduled for execution
in an audio stream. All scheduling functions return future objects,
which may be used to interrupt the scheduled action.

Time may be measured in samples or milliseconds. To convert between the
two, see samples-to-milliseconds and milliseconds-to-samples.

Futures are scheduled using the `do`, `send` and `receive` family of
functions.

See [Future](@ref doremir::scl::Future).

### schedule-options

Defines options that can be passed to all scheduling functions.

unit Either :samples or :milliseconds. Default is :milliseconds groups A
list of future-groups to which this future will be added. repeats Number
of times this future will be repeated (default 1). interval Time to wait
between repetitions (default same as time).

### send-options

Defines options that can be passed to the send family of functions. kind
Either :midi or :audio.

### receive-options

### future-group

Provides a way to interrupt a group futures atomically.

Each future group is associated with an interruption-mode, which
specifies how it handles late interruptions. A late interruption is an
interruption that occurs when some but not all members of a future group
have been fired.

The interruption-modes prescribes the following actions:

-   `:simple` Interrupt all remaining futures
-   `:transactional` Execute remaining futures on scheduled time

See [FutureGroup](@ref doremir::scl::FutureGroup).

Conditions
----------

### audio-error

An error in the audio system.

### portmidi-error

An error in portmidi.

### portaudio-error

An error in portaudio.

### stream-error

An error related to a stream.

### dsp-error

An error related to an audio processor.

Generic functions
-----------------

### message

A string describing the error.

### error-code

Portmidi-specific error code

### error-code

Portaudio-specific error code

### name

### host-name

### has-input

### has-output

### name

### devices

### name

### host

### num-inputs

### num-outputs

### low-input-latency

### high-input-latency

### low-output-latency

### high-output-latency

### sample-rate

### controls

### num-inputs

### num-outputs

### sample-rate

### 

### audio-buffer-size

### 

### audio-latency

### 

### midi-latency

### 

### non-blocking

### 

### exclusive-mode

### 

### start

Starts audio processing.

### stop

Stops audio processing once all current buffers have been drained. This
is the normal way of stopping a stream.

### abort

Stops audio processing as soon as possible.

### running

### sample-rate

### audio-buffer-size

### set-error-handler

### unit

### groups

### repeats

### interval

### 

### 

### 

### 

### kind

### processors

### devices

### channels

### 

### 

### 

### 

### copy

### kind

### processors

### devices

### channels

### 

### 

### 

### 

### do-now

Executes the given action as soon as possible. Returns a future object.

### do-later

Executes the given action after the given amount of time. Returns a
future object.

### do-at

Executes the given action at the given point in time. Returns a future
object.

### send-now

Sends the given message as soon as possible. Returns a future object.
May signal stream-error.

### send-later

Sends the given message after the given amount of time. Returns a future
object. May signal stream-error.

### send-at

Sends the given messages at the given point in time. Returns a future
object. May signal stream-error.

### receive

Start passing incoming messages to the given receiver. Returns a future
object.

### interruption-mode

### interrupt

Interrupts this object, preventing it from being fired. If it has
already been fired, this function has no effect.

Note that a future might have been fired concurrently by another thread,
event if its actions has not yet been observed by the caller thread.

### interrupt

### interrupt

Functions
---------

### midi-devices

Returns a list of midi devices currently available. May signal
portmidi-error.

### default-midi-input-device

Returns the current default midi input device or nil if there are no
input devices available. May signal portmidi-error.

### default-midi-output-device

Returns the current default midi output device or nil if there are no
output devices available. May signal portmidi-error.

### midi-sources

Returns a list of midi sources currently available. May signal
portmidi-error.

### midi-destinations

Returns a list of midi sources currently available. May signal
portmidi-error.

### audio-hosts

Returns a list of audio hosts currently available. May signal
portaudio-error.

### default-audio-host

Returns the current default audio host. May signal portaudio-error.

### default-audio-input-device

Returns the current default audio input device. May signal
portaudio-error.

### default-audio-output-device

Returns the current default audio output device. May signal
portaudio-error.

### audio-sources

Returns a list of audio sources currently available. May signal
portaudio-error.

### audio-destinations

Returns a list of audio destinations currently available. May signal
portaudio-error.

### load-fluidsynth

Creates a FluidSynth instance using the given sound font. May signal
dsp-error.

### load-plugins

Load the given set of AU or VST plug-ins.

### sequence

Creates a sequential processor from the given processors. The output
from the first processor is fed into the input of the second and so on.
The number of output and input channels at each such connection point
must match exactly, or a dsp-error is signaled.

### parallel

Creates a parallel processor from the given processors. The resulting
processor has x inputs and y outputs, where x is the total number of
inputs channels in the given list of processors and y is the total
number of outputs.

### default-device-stream-options

### open-device-stream

Opens a real-time audio stream on the given devices.

If no processor or audio devices are provided, a midi-only stream is
returned. If such a stream is passed to one of the `send` or `receive`
methods along with an audio processor or message-type `:audio`, an error
is signaled. If no midi devices are provided, an audio-only stream is
returned. If such a stream is passed to one of the `send` or `receive`
methods, along with a midi device or message-type :midi, an error is
signaled.

May signal stream-error, portaudio-error, portmidi-error or dsp-error.

### default-schedule-options

### default-send-options

### default-receive-options

### send-note-now

### send-note-later

### send-note-at

### make-future-group

Macros
------

### define-handler

### define-action

Defines an schedulable action. Each action must have the form (defaction
my-action (time) ...)

### define-receiver

Defines a message receiver. Each receiver must have the form
(define-receiver my-receiver (time msg) ...)

Constants
---------