
/** Run a session.
 */
void doremir_device_audio_with_session(doremir_device_audio_session_callback_t callback) {}

/** Begin a new session.
 */
doremir_device_audio_session_t doremir_device_audio_begin_session() {}

/** End the given session.
 */
void doremir_device_audio_end_session(doremir_device_audio_session_t session) {}

// --------------------------------------------------------------------------------

/** Get all active audio devices of the given session.
    @return
        A list of \ref doremir_device_audio_t.
 */
doremir_list_t doremir_device_audio_all(doremir_device_audio_session_t session) {}

/** Get the standard devices of the given session.
    @return
        A pair of \ref doremir_device_audio_t.
 */
doremir_pair_t doremir_device_audio_default(doremir_device_audio_session_t session) {}

// --------------------------------------------------------------------------------

/** Run a stream on the given devices.
 */
void doremir_device_audio_with_stream(doremir_device_audio_t                 input,
                                      doremir_processor_t                    processor,
                                      doremir_device_audio_t                 output,
                                      doremir_device_audio_stream_callback_t callback) {}

/** Open a stream on the given devices.
 */
doremir_device_audio_stream_t 
doremir_device_audio_start_stream(doremir_device_audio_t    input,
                                  doremir_processor_t       processor,
                                  doremir_device_audio_t    output) {}

/** Close the given stream.
 */
void doremir_device_audio_stop_stream(doremir_device_audio_stream_t stream) {}
