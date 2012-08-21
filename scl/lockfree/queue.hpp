
#pragma once

#include <boost/lockfree/fifo.hpp>

namespace scl
{
  namespace lockfree
  {
    using queue = boost::lockfree::fifo;
  }
}
