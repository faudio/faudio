
#pragma once

#ifndef SCL_USE_BOOST_ATOMIC
#warning "Not using boost atomics"
#include <atomic>

namespace scl
{
  namespace atomic = std;
}
#else
#warning "Using boost atomics"
#include <boost/atomic.hpp>
namespace scl
{
  namespace atomic = boost;
}
#endif
