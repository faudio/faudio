
#include <gtest/gtest.h>
#include <scl/audio/audio_types.hpp>

TEST(AudioTypes, Basic)
{
  using namespace scl::audio;
  // std::shared_ptr<audio_type> a {{ audio_type_tag::sample32 }};
  // std::shared_ptr<audio_type> b {{ audio_type_tag::sample32 }};
  audio_type x = type::sample32();
  audio_type y = type::pair(type::list(x), type::vector(x, 10));
  audio_type z = type::pair(x, type::pair(x, x));
  std::cout << "Name: " << x << "\n Levels: " << x.declaration() << "\n";
  std::cout << "Name: " << y << "\n Levels: " << y.declaration() << "\n";
  std::cout << "Name: " << z << "\n Levels: " << z.declaration() << "\n";
  using T =
    audio_pair < audio_vector<sample32, 256>::type,
    sample64
    >::type;
  audio_type t = get_audio_type2<T>();
  std::cout << "Name: " << t << "\n Levels: " << t.levels() << "\n";
}
