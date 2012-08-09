
#pragma once

#include <boost/lockfree/ringbuffer.hpp>

namespace scl
{
  namespace lockfree
  {
using circular_buffer = boost::lockfree:
                            ringbuffer;
  }
}

