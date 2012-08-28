
#include <vector>
#include <gtest/gtest.h>
#include <boost/format.hpp>
#include <scl/audio/audio_types.hpp>

TEST(AudioTypes, Dynamic)
{
  using namespace scl::audio;
  // std::shared_ptr<audio_type> a {{ audio_type_tag::sample32 }};
  // std::shared_ptr<audio_type> b {{ audio_type_tag::sample32 }};
  audio_type x = audio_type::sample32();
  audio_type y = audio_type::pair(audio_type::list(x), audio_type::vector(x, 10));
  audio_type z = audio_type::pair(x, audio_type::pair(x, x));
  std::cout << "Name: " << x << "\n Levels: " << x.declaration() << "\n";
  std::cout << "Name: " << y << "\n Levels: " << y.declaration() << "\n";
  std::cout << "Name: " << z << "\n Levels: " << z.declaration() << "\n";

  using my_audio_type =
    audio_pair < 
      audio_vector<sample32, 256>::type,
      audio_pair<sample32,sample32>::type
    >::type;

  audio_type t = audio_type::get<my_audio_type>();

  std::cout << "Name: " << t << "\n Levels: " << t.levels() << "\n";
}

TEST(AudioTypes, DynamicEq)
{
  using namespace scl::audio;

  audio_type a = audio_type::pair(audio_type::list(audio_type::sample32()), audio_type::vector(audio_type::sample32(), 10));
  audio_type b = audio_type::pair(audio_type::list(audio_type::sample32()), audio_type::vector(audio_type::sample32(), 10));
  ASSERT_EQ(a, b);

  audio_type c = audio_type::pair(audio_type::list(audio_type::sample32()), audio_type::vector(audio_type::sample32(), 10));
  audio_type d = audio_type::pair(audio_type::list(audio_type::sample32()), audio_type::vector(audio_type::sample32(), 20));
  ASSERT_NE(c, d);

  audio_type e = audio_type::vector(audio_type::sample32(), 10);
  audio_type f = audio_type::sample32();
  ASSERT_NE(e, f);
}