
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

typedef struct _state_t *state_t;
typedef struct _state_base_t *state_base_t;

/** Enable/disable vector processing.

    Note that vector mode is not fully implemented on Windows.
 */
#ifdef _WIN32
    #define kVectorMode false
#else
    #define kVectorMode false
#endif

#define kAllowVirtualTime               1

/** Maximal allowed vector size.
 */
#define kMaxVectorSize                  1024

/** Maximal allowed vector size.
 */
#define kMaxVectorSize                  1024

/** Default vector size.
 */
#define kDefVectorSize                  64

/** Maximal depth of signal tree.
 */
#define kMaxSignalTreeDepth             90

/** Maximal allowed sample rate.
 */
#define kMaxSignalTreeDepth             90

/** Default sample rate.
 */
#define kDefSampleRate                  44100

/** Default latency in seconds.
 */
#define kDefLatency                     0.030

/** Audio scheduler interval in milliseconds.
 */
#define kAudioSchedulerIntervalMillis   1

/** Maximum number of custom processors.
 */
#define kMaxCustomProcs                 64

/** Maximum number of (global + custom) inputs.
 */
#define kMaxInputs                      128

/** Maximum number of internal buses (for delay and loop).
 */
#define kMaxBuses                       256

/** Maximum delay in seconds.
 */
#define kMaxDelaySeconds                1

#define kOutputOffset                   0
#define kInputOffset                    8

