
#include <gtest/gtest.h>
#include <scl/audio/concept/processor.hpp>
#include <scl/audio/processor/simple/unary.hpp>

using namespace scl;
using namespace scl::audio;
using namespace scl::audio::processor;

TEST(AudioProcessor, Unary)
{
  using in  = sample32;
  using out = sample32;  

  // std::function<sample32(sample32)> f { [] (sample32 x) -> sample32 { return 0; } };
  // std::function<sample32(sample32)> g = f;

  std::function<void(const in&, out&)> f = 
    [] (const in& x, out& r) { r = x + 10; };

  unary_processor<in, out> p (f);        

  std::list<nullptr_t> msg;
  in  x = 100;
  out y;
  p.prepare(nullptr);
  p.process(msg, x, y, msg);
  
  std::cout << "Argument: " << x << "\n";
  std::cout << "Result:   " << y << "\n";
}
