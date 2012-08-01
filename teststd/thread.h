
#ifndef USE_BOOST_THREAD
  #warning "Not using boost threads"
  #include <thread>
  #include <condition_variable>
  #include <atomic>
  #include <exception_ptr>
  namespace thread    = std;
  namespace exception = std;
#else
  #warning "Using boost threads"
  #include <boost/thread.hpp>
  #include <boost/exception/exception.hpp>
  namespace thread    = boost;
  namespace exception = boost;
#endif
