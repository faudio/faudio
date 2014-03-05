
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
#define kMaxSignalTreeDepth             90

#define kDefSampleRate                  44100
#define kDefLatency                     0.030
#define kAudioSchedulerIntervalMillis   1

#define kMaxCustomProcs                 10
#define kMaxInputs                      128
#define kMaxBuses                       64
#define kMaxDelaySeconds                5

#define kOutputOffset                   0
#define kInputOffset                    8
#define kControlOffset                  16

typedef struct _state_t *state_t;
typedef struct _state_base_t *state_base_t;
