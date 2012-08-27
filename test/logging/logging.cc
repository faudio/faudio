
#include <gtest/gtest.h>
#include <scl/logging/logger.hpp>
#include <boost/utility/enable_if.hpp>

using namespace scl;

namespace foo
{
  struct t
  {
    static const auto value = true;
  };

  boost::enable_if<t, void>
  log()
  {
  }


}

TEST(Logging, Base)
{
}

