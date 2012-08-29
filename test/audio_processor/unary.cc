
#include <gtest/gtest.h>
#include <scl/audio/concept/processor.hpp>
#include <scl/audio/processor/simple/unary.hpp>

using namespace scl;
using namespace scl::audio;
using namespace scl::audio::processor;


TEST(AudioProcessor, UnaryWrap)
{
  std::function<void(const int&,int&)> f 
    { [] (const int& x, int& r) { r = x + 10; } };
  
  dynamic_unary_wrapper<int,int> w (f);
  int x = 10;
  int y;

  w.call((ptr_t)&x, (ptr_t)&y);
  std::cout << "Result: " << y << "\n";
}


TEST(AudioProcessor, Unary)
{
  using in  = sample32;
  using out = sample32;  
  // std::function<sample32(sample32)> f { [] (sample32 x) -> sample32 { return 0; } };
  // std::function<sample32(sample32)> g = f;
  std::function<void(const sample32&,sample32&)> f 
    { [] (const sample32& x, sample32& r) { r = x + 10; } };

  unary_processor<in, out> p (f);        

  std::list<nullptr_t> msg;
  sample32 x = 100;
  sample32 y;
  p.prepare(nullptr);
  p.process(msg, x, y, msg);
  
  std::cout << "Argument: " << x << "\n";
  std::cout << "Result:   " << y << "\n";
}
