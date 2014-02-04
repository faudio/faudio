
// TODO formalize better
struct _state_base_t {
    double *inputs;
    void   *_;

    int     count;
    double  rate;
    // ...
};

#ifdef _WIN32
    #define kVectorMode false
#else
    #define kVectorMode false
#endif

#define kMaxVectorSize                  1024
#define kDefVectorSize                  64

#define kDefSampleRate                  44100
#define kDefLatency                     0.002
#define kAudioSchedulerIntervalMillis   1

#define kMaxCustomProcs                 10
#define kMaxInputs                      128
#define kMaxBuses                       64
#define kMaxDelaySeconds                5


typedef struct _state_t *state_t;
typedef struct _state_base_t *state_base_t;
