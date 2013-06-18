

/** Begin a new session, and retain it for the duration of a call to the given function.

    The given function will be called once after the session has created. The session
    will be ended after the callback function has returned. If an error occurs while
    starting the session, the error callback is invoked in place of the session callback.

    @param callback                     Function to receive the sesssion.
    @param error_callback               Function to receive eventual errors.
    @param error_data, session_data     Data closed over by the callbacks.
 */
void fae_audio_with_session(
    fae_audio_session_callback_t session_callback,
    fae_ptr_t                           session_data,
    fae_error_callback_t                error_callback,
    fae_ptr_t                           error_data
) {}

/** Begin a new audio session.

    @return
        A new session (errable).
    @par Errors
        Returns an error if the session could not be started.
 */
fae_audio_session_t fae_audio_begin_session() {}

/** End the given session.

    @param session
        Session to end.
 */
void fae_audio_end_session(
    fae_audio_session_t session) {}


// --------------------------------------------------------------------------------

/** Get all active audio devices of the given session.

    @param session   The session.
    @return
        A list of @ref fae_audio_device_t.
 */
fae_list_t fae_audio_all(
    fae_audio_session_t session) {}

/** Get the standard devices of the given session.

    @param session   The session.
    @return
        A pair of @ref fae_audio_device_t representing the default input and
        output device, respectively.
 */
fae_pair_t fae_audio_default(
    fae_audio_session_t session) {}

/** Get the standard input device of the given session.
    @param session   The session.
 */
fae_audio_device_t fae_audio_default_input(
    fae_audio_session_t session) {}

/** Get the standard output device of the given session.
    @param session   The session.
 */
fae_audio_device_t fae_audio_default_output(
    fae_audio_session_t session) {}

/** Return the name of the given device.
    @param device   The device.
 */
fae_string_t fae_audio_name(
    fae_audio_device_t device) {}

/** Return the host name of the given device.
    @param device   The device.
 */
fae_string_t fae_audio_host_name(
    fae_audio_device_t device) {}

/** Return whether the given device has input or not.
    @param device   The device.
 */
bool fae_audio_has_input(fae_audio_device_t) {}

/** Return whether the given device has output or not.
    @param device   The device.
 */
bool fae_audio_has_output(fae_audio_device_t) {}

/** Return the input type of the given device.
    @param device   The device.
 */
fae_type_t fae_audio_input_type(fae_audio_device_t) {}

/** Return the output type of the given device.
    @param device   The device.
 */
fae_type_t fae_audio_output_type(fae_audio_device_t) {}

/** Set a callback to be invoked when a status change is detected on the
    given session. This is mainly useful for detecting a change in hardware setup.

    Note that this function will not modify the devices in a session, you have to
    restart the session to get a fresh snapshot.

    @param device   The device.
 */
void fae_audio_set_status_callback(
    fae_audio_status_callback_t callback,
    fae_ptr_t                          function,
    fae_audio_session_t         session) {}

// --------------------------------------------------------------------------------

/**
    Open a stream on the given devices.

    @param input, output    Devices to provide data source and sink.
    @param processor        Processor to run over the devices.
    @return                 A new stream (errable).
    @par Errors
        Returns an error if the session could not be started.
 */
fae_audio_stream_t
fae_audio_open_stream(
    fae_audio_device_t    input,
    fae_processor_t       processor,
    fae_audio_device_t    output) {}

/**
    Close the given stream.
    @param session          Stream to close.
 */
void fae_audio_close_stream(
    fae_audio_stream_t stream) {}

/**
    Run a stream on the given devices.

    @param input
        Input device.
    @param processor        Processor to run over the devices.
    @param input            Output device.
    @param callback         Function to receive the stream.
    @param error_callback   Function to receive eventual errors.
 */
void fae_audio_with_stream(
    fae_audio_device_t                 input,
    fae_processor_t                    processor,
    fae_audio_device_t                 output,
    fae_audio_stream_callback_t stream_callback,
    fae_ptr_t                          stream_data,
    fae_error_callback_t         error_callback,
    fae_ptr_t                          error_data
) {}
