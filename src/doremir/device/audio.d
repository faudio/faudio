

/** Begin a new session, and retain it for the duration of a call to the given function.

    The given function will be called once after the session has created. The session 
    will be ended after the callback function has returned. If an error occurs while
    starting the session, the error callback is invoked in place of the session callback.

    @param callback
        Function to receive the sesssion.
    @param error_callback
        Function to receive eventual errors.
    @param error_data, session_data
        Data closed over by the callbacks.
 */
void doremir_device_audio_with_session(
    doremir_device_audio_session_callback_t session_callback,
    doremir_ptr_t                           session_data,
    doremir_error_callback_t                error_callback,
    doremir_ptr_t                           error_data
) {}

/** Begin a new audio session.

    @return
        A new session.
    @par Errors
        Fails if the session could not be started.
 */
doremir_device_audio_session_t doremir_device_audio_begin_session() {}

/** End the given session.

    @param session
        Session to end.
 */
void doremir_device_audio_end_session(
    doremir_device_audio_session_t session) {}


// --------------------------------------------------------------------------------

/** Get all active audio devices of the given session.

    @param session   The session.
    @return
        A list of @ref doremir_device_audio_t.
 */
doremir_list_t doremir_device_audio_all(
    doremir_device_audio_session_t session) {}

/** Get the standard devices of the given session.

    @param session   The session.
    @return
        A pair of @ref doremir_device_audio_t representing the default input and
        output device, respectively.
 */
doremir_pair_t doremir_device_audio_default(
    doremir_device_audio_session_t session) {}

/** Get the standard input device of the given session.
    @param session   The session.
 */
doremir_device_audio_t doremir_device_audio_default_input(
    doremir_device_audio_session_t session) {}

/** Get the standard output device of the given session.
    @param session   The session.
 */
doremir_device_audio_t doremir_device_audio_default_output(
    doremir_device_audio_session_t session) {}

/** Return the name of the given device.
    @param device   The device.
 */
doremir_string_t doremir_device_audio_name(
    doremir_device_audio_t device) {}

/** Return the host name of the given device.
    @param device   The device.
 */
doremir_string_t doremir_device_audio_host_name(
    doremir_device_audio_t device) {}

/** Return the input type of the given device.
    @param device   The device.
 */
doremir_type_t doremir_device_audio_input_type(doremir_device_audio_t) {}

/** Return the output type of the given device.
    @param device   The device.
 */
doremir_type_t doremir_device_audio_output_type(doremir_device_audio_t) {}

/** Set a callback to be invoked when a status change is detected on the 
    given session. This is mainly useful for detecting a change in hardware setup.

    Note that this function will not modify the devices in a session, you have to
    restart the session to get a fresh snapshot.

    @param device   The device.
 */
void doremir_device_audio_set_status_callback(
    doremir_device_audio_status_callback_t callback,
    doremir_ptr_t                          function,
    doremir_device_audio_session_t         session) {}

// --------------------------------------------------------------------------------

/**
    Open a stream on the given devices.

    @param input, output
        Devices to provide data source and sinks.
    @param processor
        Processor to run over the devices.
    @throw
        TODO
    @return
        A new stream or an error if no stream could be opened.
 */
doremir_device_audio_stream_t
doremir_device_audio_open_stream(
    doremir_device_audio_t    input,
    doremir_processor_t       processor,
    doremir_device_audio_t    output) {}

/**
    Close the given stream.
    @param session
        Stream to close.
 */
void doremir_device_audio_close_stream(
    doremir_device_audio_stream_t stream) {}

/**
    Run a stream on the given devices.

    @param input
        Input device.
    @param processor
        Processor to run over the devices.
    @param input
        Output device.
    @param callback
        Function to receive the stream.
    @param error_callback
        Function to receive eventual errors.
 */
void doremir_device_audio_with_stream(
    doremir_device_audio_t                 input,
    doremir_processor_t                    processor,
    doremir_device_audio_t                 output,
    doremir_device_audio_stream_callback_t stream_callback,
    doremir_ptr_t                          stream_data,
    doremir_error_callback_t         error_callback,
    doremir_ptr_t                          error_data
) {}
