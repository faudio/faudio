
// TODO formalize better
struct _state_base_t {
    double *inputs;
    void   *_;

    int     count;
    double  rate;

    double      speed;
    double      elapsed_time;

    int     custom_proc_count;
    // ...
};

#ifdef _WIN32
    #define kVectorMode false
#else
    #define kVectorMode false
#endif

#define kAllowVirtualTime               1

#define kMaxVectorSize                  1024
#define kDefVectorSize                  64
#define kMaxSignalTreeDepth             90

#define kDefSampleRate                  44100
#define kDefLatency                     0.030
#define kAudioSchedulerIntervalMillis   1

#define kMaxCustomProcs                 10
#define kMaxInputs                      128
#define kMaxBuses                       256
#define kMaxDelaySeconds                5

#define kAUOffset                       32
#define kFluidOffset                    34
#define kVstOffset                      36
#define kRecExternalOffset              40
#define kTriggerOffset                  64

#define kOutputOffset                   0
#define kInputOffset                    8
#define kControlOffset                  16

typedef struct _state_t *state_t;
typedef struct _state_base_t *state_base_t;
