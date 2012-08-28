
#include <vector>
#include <gtest/gtest.h>
#include <boost/format.hpp>
#include <scl/audio/audio_types.hpp>

TEST(AudioTypes, Dynamic)
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
    std::pair<sample32,sample32>
    >::type;
  audio_type t = get_audio_type2<T>();
  std::cout << "Name: " << t << "\n Levels: " << t.levels() << "\n";
}

TEST(AudioTypes, DynamicEq)
{
  using namespace scl::audio;

  audio_type a = type::pair(type::list(type::sample32()), type::vector(type::sample32(), 10));
  audio_type b = type::pair(type::list(type::sample32()), type::vector(type::sample32(), 10));
  ASSERT_EQ(a, b);

  audio_type c = type::pair(type::list(type::sample32()), type::vector(type::sample32(), 10));
  audio_type d = type::pair(type::list(type::sample32()), type::vector(type::sample32(), 20));
  ASSERT_NE(c, d);

  audio_type e = type::vector(type::sample32(), 10);
  audio_type f = type::sample32();
  ASSERT_NE(e, f);
}