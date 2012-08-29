
#include <gtest/gtest.h>
#include <scl/audio/concept/processor.hpp>
#include <scl/audio/processor/simple/split.hpp>

using namespace scl;
using namespace scl::audio;
using namespace scl::audio::processor;

TEST(AudioProcessor, Split)
{
  using in  = sample32;
  using out = audio_pair<sample32,sample32>::type;

  split_processor<in> p;  

  unit_list msg;
  in  x = 100;
  out y;
  
  p.prepare(nullptr);
  p.process(msg, x, y, msg);
  
  std::cout << "Argument: " << x << "\n";
  std::cout << "Result:   " << y.first << "\n";
  std::cout << "Result:   " << y.second << "\n";
}
