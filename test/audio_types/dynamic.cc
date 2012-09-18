
#include <vector>
#include <gtest/gtest.h>
#include <boost/format.hpp>
#include <scl/audio/audio_types.hpp>

TEST(AudioTypes, Dynamic)
{
  using namespace scl::audio;
  using typ = audio_type;

  audio_type x = typ::sample32();
  audio_type y = typ::pair(typ::list(x), typ::vector(x, 10));
  audio_type z = typ::pair(x, typ::pair(x, x));
  std::cout << "Name: " << x << "\n Levels: " << x.declaration() << "\n";
  std::cout << "Name: " << y << "\n Levels: " << y.declaration() << "\n";
  std::cout << "Name: " << z << "\n Levels: " << z.declaration() << "\n";
}

TEST(AudioTypes, DynamicGet)
{
  using namespace scl::audio;

  using my_audio_type =
    audio_pair < 
      audio_vector<sample32, 256>::type,
      audio_pair<sample32,sample32>::type
    >::type;

  audio_type t = audio_type::get<my_audio_type>();
  std::cout << "Name: " << t << "\n Levels: " << t.levels() << "\n";

  using my_audio_type2 =
    audio_channels<int8, 8>::type;

  audio_type t2 = audio_type::get<my_audio_type2>();
  std::cout << "Name: " << t2 << "\n Levels: " << t2.levels() << "\n";
}

TEST(AudioTypes, DynamicEq)
{
  using namespace scl::audio;
  using typ = audio_type;

  audio_type a = typ::pair(typ::list(typ::sample32()), typ::vector(typ::sample32(), 10));
  audio_type b = typ::pair(typ::list(typ::sample32()), typ::vector(typ::sample32(), 10));
  ASSERT_EQ(a, b);

  audio_type c = typ::pair(typ::list(typ::sample32()), typ::vector(typ::sample32(), 10));
  audio_type d = typ::pair(typ::list(typ::sample32()), typ::vector(typ::sample32(), 20));
  ASSERT_NE(c, d);

  audio_type e = typ::vector(typ::sample32(), 10);
  audio_type f = typ::sample32();
  ASSERT_NE(e, f);
}