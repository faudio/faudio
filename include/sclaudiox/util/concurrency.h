/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_UTIL_CONCURRENCY
#define _SCLAUDIOX_UTIL_CONCURRENCY

#ifdef SCL_WIN
    #ifndef _WIN32_DCOM
        #define _WIN32_DCOM
    #endif
    #include <ObjBase.h>
#endif

#include <boost/thread.hpp>
#include <boost/date_time/posix_time/posix_time_types.hpp>

#include "sclaudiox/core.h"
#include "sclaudiox/defines.h"
#include "sclaudiox/error.h"
#include "sclaudiox/util/logging.h"


namespace doremir {
namespace scl {


// =============================================================================

// Threads and Mutexes

typedef boost::thread                       Thread;

typedef boost::mutex                        Mutex;
typedef boost::lock_guard<Mutex>            Lock;

typedef boost::shared_mutex                 SharedMutex;
typedef boost::unique_lock<SharedMutex>     UniqueLock;
typedef boost::shared_lock<SharedMutex>     SharedLock;

typedef boost::recursive_mutex              RecursiveMutex;
typedef boost::recursive_mutex::scoped_lock ReentrantLock;

typedef boost::condition_variable_any       ConditionVariable;
typedef boost::thread_interrupted           ThreadInterrupted;


inline void sleepMillis(long millis)
{
    return boost::this_thread::sleep(boost::posix_time::milliseconds(millis));
}

inline void interruptionPoint()
{
    boost::this_thread::interruption_point();
}

inline bool isCurrentThread(Thread* thread)
{         
    if (thread == NULL)
        return false;
    return thread->get_id() == boost::this_thread::get_id();
}   

inline void waitForThread(Thread* thread)
{
    if (!isCurrentThread(thread))
    {
        thread->interrupt();
        thread->join();
    }
}

static RecursiveMutex *printMutexInst;

/**
    Returns the global mutex object used by the concurrent print operations.
 */
inline RecursiveMutex& printMutex()
{
    if (!printMutexInst)
        printMutexInst = new RecursiveMutex();
    return *printMutexInst;
}

template <class T>
class SendVar
{      
  T s;
  bool full;
  ConditionVariable cond;
  Mutex mutex;
public:       
  SendVar()
    : full(false) {}
  void put(T x)
  {
    boost::unique_lock<Mutex> lock (mutex);
    s = x;      
    full = true;
    cond.notify_one();
  }
  void take(T* x)
  {
    boost::unique_lock<Mutex> lock (mutex);
    while (!full)
      cond.wait(lock);
    full = false;
    *x = s;
  }
};

#ifdef _WIN32_DCOM
/**
    Initializes Windows COM when created and uninitializes it when destructed.	
	This implementation tolerates failure due to changed mode.
	
	Must be created and destroyed in the same thread.
 */
class WindowsComToken : public NonCopyable
{
public:
    WindowsComToken()
    {
#ifdef SCL_AUDIO_ENABLE_LOGGING
        SCL_WRITE_LOG("Calling CoInitializeEx\n");
#endif
        mErr = CoInitializeEx(NULL, COINIT_MULTITHREADED);
        
        if ( mErr == RPC_E_CHANGED_MODE )
        {          
#ifdef SCL_AUDIO_ENABLE_LOGGING
            SCL_WRITE_LOG("CoInitializeEx failed with RPC_E_CHANGED_MODE");
#endif
		}
    }

    ~WindowsComToken() 
	{ 
#ifdef SCL_AUDIO_ENABLE_LOGGING
            SCL_WRITE_LOG("Calling CoUninitialize\n");
#endif
		// TODO when to call safely?
        // if ( mErr != RPC_E_CHANGED_MODE ) 
            // CoUninitialize();
	}

private:
	HRESULT mErr;
};
#endif // _WIN32_DCOM


} // namespace
} // namespace

#endif
