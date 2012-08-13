
#pragma once

#ifndef SCL_USE_BOOST_THREAD

  #warning "Not using boost threads"
  #include <thread>
  #include <mutex>
  #include <condition_variable>
  #include <future>
  #include <exception> // for exception_ptr

  namespace scl
  {
    namespace thread    = std;
    namespace exception = std;
  }

#else

  #warning "Using boost threads"
  #include <boost/thread.hpp>
  #include <boost/exception/exception.hpp>

  namespace scl
  {
    namespace thread    = boost;
    namespace exception = boost;
  }

#endif

