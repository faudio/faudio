
#include <fa/priority_queue.h>

// TODO formalize better
struct _state_base_t {
    double *inputs;
    void   *_;

    int     count;
    double  rate;
    // ...
};

#ifdef _WIN32
    #define kVectorMode true
#else
    #define kVectorMode false
#endif

#define kMaxVectorSize                  1024
#define kDefVectorSize                  64

#define kDefSampleRate                  48000
#define kDefLatency                     0.002
#define kAudioSchedulerIntervalMillis   1

#define kMaxCustomProcs                 10
#define kMaxInputs                      128
#define kMaxBuses                       64
#define kMaxDelaySeconds                5


typedef struct _state_t *state_t;
typedef struct _state_base_t *state_base_t;







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
       custom_proc_after } custom_proc_when_t;
       
/** Extract a list of fa_signal_custom_processor_t
    To be added with add_custom_proc
 */
list_t fa_signal_get_procs(fa_signal_t signal2);
void add_custom_proc(fa_signal_custom_processor_t* proc, state_t state);
void run_custom_procs(custom_proc_when_t when, state_t state);

/** Run a simpel action (send, set or accum) on the processing state. */
ptr_t run_simple_action(state_t state, fa_action_t action);

double step(signal_t signal, state_t state);
void step_vector(signal_t signal, state_t state, int count, double* out);

