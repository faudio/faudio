

/** 
    Return the name of the given device.
    @param device 
        The device.
 */
doremir_string_t doremir_device_audio_name(doremir_device_audio_t device) {}

/** 
    Return the host name of the given device.
    @param device 
        The device.
 */
doremir_string_t doremir_device_audio_host_name(doremir_device_audio_t device) {}

/** 
    Return the number of channels of the given device.
    @param device 
        The device.
    @return
        A pair of int32_t, denoting number of inputs and outputs, respectively.
 */
doremir_pair_t doremir_device_audio_channels(doremir_device_audio_t device) {}

/** 
    Return whether the given device has input or not.
    @param device 
        The device.
 */
bool doremir_device_audio_has_input(doremir_device_audio_t) {}

/** 
    Return whether the given device has output or not.
    @param device 
        The device.
 */
bool doremir_device_audio_has_output(doremir_device_audio_t) {}

// --------------------------------------------------------------------------------

/** 
    Begin a new session, and retain it for the duration of a call to the given function.
    
    The given function will be called once after the session has created, and should not
    return until the end of the session.

    @param callback 
        Function to receive the sesssion.
    @param error_callback 
        Function to receive eventual errors.
 */
void doremir_device_audio_with_session(doremir_device_audio_session_callback_t callback) {}

/** 
    Begin a new audio session.
    
    @throw
        TODO
    @return 
        A new session, or an error if no session could be started.
 */
doremir_device_audio_session_t doremir_device_audio_begin_session() {}

/** 
    Restart the given audio session.
    
    @throw
        TODO
    @return 
        A new session, or an error if no session could be started.
 */
doremir_device_audio_session_t doremir_device_audio_restart_session(doremir_device_audio_session_t session) {}

/** 
    End the given session.
    
    @param session 
        Session to end.
 */
void doremir_device_audio_end_session(doremir_device_audio_session_t session) {}

/** 
    Get all active audio devices of the given session.
    @return
        A list of @ref doremir_device_audio_t.
 */
doremir_list_t doremir_device_audio_all(doremir_device_audio_session_t session) {}

/** 
    Get the standard devices of the given session.
    @return
        A pair of @ref doremir_device_audio_t representing the default input and output 
        device, respectively.
 */
doremir_pair_t doremir_device_audio_default(doremir_device_audio_session_t session) {}



// --------------------------------------------------------------------------------

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
void doremir_device_audio_with_stream(doremir_device_audio_t                 input,
                                      doremir_processor_t                    processor,
                                      doremir_device_audio_t                 output,
                                      doremir_device_audio_stream_callback_t callback) {}

/** 
    Open a stream on the given devices.

    @param input
        Input device.
    @param processor
        Processor to run over the devices.
    @param input
        Output device.
    @throw
        TODO
    @return
        A new stream or an error if no stream could be started.
 */
doremir_device_audio_stream_t 
doremir_device_audio_start_stream(doremir_device_audio_t    input,
                                  doremir_processor_t       processor,
                                  doremir_device_audio_t    output) {}

/** 
    Restart the given audio stream.
    
    @throw
        TODO
    @return 
        A new session, or an error if no session could be started.
 */
doremir_device_audio_stream_t doremir_device_audio_restart_stream(doremir_device_audio_stream_t stream) {}

/** 
    Close the given stream.
    @param session 
        Stream to close.
 */
void doremir_device_audio_stop_stream(doremir_device_audio_stream_t stream) {}
