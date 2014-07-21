


// TODO hide all of these by rewriting signal/audio to use callbacks

/** Create a new DSP state object,
 */
state_t new_state(int sample_rate);

/** Destroy a DSP state object,
 */
void delete_state(state_t state);

void inc_state1(state_t state);           
void inc_state(int n, state_t state);           

typedef enum { custom_proc_before,
       custom_proc_render,
       custom_proc_after,
       custom_proc_destroy
        } custom_proc_when_t;
       
/** Extract a list of fa_signal_custom_processor_t
    To be added with add_custom_proc
 */
fa_list_t fa_signal_get_procs(fa_signal_t signal2);
void add_custom_proc(fa_signal_custom_processor_t* proc, state_t state);
void run_custom_procs(custom_proc_when_t when, int count, state_t state);
void custom_procs_receive(state_t state, fa_signal_message_callback_t cb, fa_ptr_t data);

/** Run a simpel action (send, set or accum) on the processing state. */
fa_ptr_t run_simple_action(state_t state, fa_action_t action);

double step(fa_signal_t signal, state_t state);
void step_vector(fa_signal_t signal, state_t state, int count, double* out);
