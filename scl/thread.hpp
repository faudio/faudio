
#pragma once

#ifndef SCL_USE_BOOST_THREAD

#warning "Not using boost threads"
#include <thread>
#include <mutex>
#include <condition_variable>
#include <future>
#include <exception> // for exception_ptr
#include <chrono>

namespace scl
{
  namespace thread    = std;
  namespace exception = std;
  namespace chrono	  = std::chrono;
}

#else

#warning "Using boost threads"
#include <boost/thread.hpp>
#include <boost/exception/exception.hpp>
#include <boost/chrono.hpp>

namespace scl
{
  namespace thread    = boost;
  namespace exception = boost;
  namespace chrono    = boost::chrono;
}

#endif

