
#include <gtest/gtest.h>
#include <scl/audio/audio_types.hpp>

TEST(AudioTypes, Basic)
{ 
  using namespace scl::audio;
  
  // std::shared_ptr<audio_type> a {{ audio_type_tag::sample32 }};
  // std::shared_ptr<audio_type> b {{ audio_type_tag::sample32 }};

  audio_type x = audio_types::sample32();
  audio_type y = audio_types::pair(audio_types::list(x), audio_types::vector(x));
  audio_type z = audio_types::pair(x, audio_types::pair(x, x));
  std::cout << x << "\n";
  std::cout << y << "\n";
  std::cout << z << "\n";
  
}
