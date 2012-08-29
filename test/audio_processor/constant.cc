
#include <gtest/gtest.h>
#include <scl/audio/concept/processor.hpp>
#include <scl/audio/processor/simple/constant.hpp>

using namespace scl;
using namespace scl::audio;
using namespace scl::audio::processor;

TEST(AudioProcessor, Constant)
{
  using type = audio_vector<sample32,10>::type;

  constant_processor<type, type> p;  
}
