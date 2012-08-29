
#include <gtest/gtest.h>
#include <scl/audio/concept/processor.hpp>
#include <scl/audio/processor/simple/identity.hpp>

using namespace scl;
using namespace scl::audio;
using namespace scl::audio::processor;

TEST(AudioProcessor, Identity)
{                        
  using type = sample32;

  identity_processor<type> p;  

  std::list<nullptr_t> msg;
  type x = 100;
  type y;
  p.prepare(nullptr);
  p.process(msg, x, y, msg);
  
  std::cout << "Argument: " << x << "\n";
  std::cout << "Result:   " << y << "\n";
}


TEST(AudioProcessor, IdentityVec)
{                        
  using type = audio_vector<sample32, 10>::type;

  identity_processor<type> p;  

  std::list<nullptr_t> msg;
  type x = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  type y;
  p.prepare(nullptr);
  p.process(msg, x, y, msg);
  
  // std::cout << "Argument: " << x << "\n";
  // std::cout << "Result:   " << y << "\n";
}
