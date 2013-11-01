
// TODO formalize better
struct _state_t {
    double *inputs;
    void   *_;
    int     count;
    double  rate;
    // ...
};
typedef struct _state_t *state_t;
list_t fa_signal_get_procs(fa_signal_t signal2);
state_t new_state();
void add_custom_proc(fa_signal_custom_processor_t* proc, state_t state);
void delete_state(state_t state);
void inc_state(state_t state);
void run_actions(priority_queue_t controls2, state_t state);
void run_custom_procs(int when, state_t state);
double step(signal_t signal, state_t state);
fa_signal_t fa_signal_simplify(fa_signal_t signal2);