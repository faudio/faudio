
#include <gtest/gtest.h>
#include <scl/audio/audio_types.hpp>

TEST(AudioTypes, Basic)
{ 
  using namespace scl::audio;
  
  // std::shared_ptr<audio_type> a {{ audio_type_tag::sample32 }};
  // std::shared_ptr<audio_type> b {{ audio_type_tag::sample32 }};

  audio_type x = dynamic::type::sample32();
  audio_type y = dynamic::type::pair(dynamic::type::list(x), dynamic::type::vector(x));
  audio_type z = dynamic::type::pair(x, dynamic::type::pair(x, x));
  std::cout << x << "\n";
  std::cout << y << "\n";
  std::cout << z << "\n";
  
}
