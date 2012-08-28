
#include <iostream>
#include <gtest/gtest.h>
#include <scl/audio/concept/startable.hpp>

struct startable
{
  void abort() {}
  void start() {}
  void stop() {}
  bool is_aborted()
  {
    return false;
  }
  bool is_running()
  {
    return false;
  }
};
struct not_startable
{
};

BOOST_CONCEPT_ASSERT((scl::audio::Startable<startable>));
// BOOST_CONCEPT_ASSERT((scl::audio::Startable<not_startable>)); // should not compile

TEST(AudioConcept, Startable) {}





BOOST_CONCEPT_ASSERT((scl::audio::Startable<scl::audio::any_startable>));





struct foo
{
  void abort()
  {
    std::cout << "foo aborted\n";
  }
  void start()
  {
    std::cout << "foo started\n";
  }
  void stop()
  {
    std::cout << "foo stopped\n";
  }
  bool is_aborted()
  {
    return false;
  }
  bool is_running()
  {
    return false;
  }
};
struct bar
{
  void abort()
  {
    std::cout << "bar aborted\n";
  }
  void start()
  {
    std::cout << "bar started\n";
  }
  void stop()
  {
    std::cout << "bar stopped\n";
  }
  bool is_aborted()
  {
    return false;
  }
  bool is_running()
  {
    return false;
  }
};

BOOST_CONCEPT_ASSERT((scl::audio::Startable<foo>));
BOOST_CONCEPT_ASSERT((scl::audio::Startable<bar>));


TEST(AudioConcept, StartableAny)
{
  foo x;
  bar y;
  scl::audio::any_startable s(x);
  s.start();
  s.stop();
  s.abort();
  scl::audio::any_startable t(y);
  t.start();
  t.stop();
  t.abort();
  std::swap(s, t);
  std::swap(s, t);
  t = s;
  t.start();
  t.stop();
  t.abort();
  s.start();
}
