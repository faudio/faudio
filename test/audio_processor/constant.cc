
#include <gtest/gtest.h>
#include <scl/audio/concept/processor.hpp>
#include <scl/audio/processor/simple/constant.hpp>

using namespace scl;
using namespace scl::audio;
using namespace scl::audio::processor;

TEST(AudioProcessor, Constant)
{
  using type = sample32;

  constant_processor<type, type> p;  

  std::list<nullptr_t> msg;
  type x = 100;
  type c = 200;
  type y;
  p.prepare(c);
  p.process(msg, x, y, msg);
  
  std::cout << "Argument: " << x << "\n";
  std::cout << "Result:   " << y << "\n";
}
