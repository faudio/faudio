
#ifndef USE_BOOST_THREAD
  #warning "Not using boost threads"
  #include <thread>
  #include <condition_variable>
  #include <atomic>
  namespace thread = std;
#else
  #warning "Using boost threads"
  #include <boost/thread.hpp>
  namespace thread = boost;
#endif
