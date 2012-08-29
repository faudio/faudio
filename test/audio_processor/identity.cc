
#include <gtest/gtest.h>
#include <scl/audio/concept/processor.hpp>
#include <scl/audio/processor/simple/identity.hpp>

using namespace scl;
using namespace scl::audio;
using namespace scl::audio::processor;

TEST(AudioProcessor, Identity)
{                        
  using type = audio_vector<sample32,10>::type;

  identity_processor<type> p;  
}
