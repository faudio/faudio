
#include <gtest/gtest.h>
#include <scl/audio/concept/abortable.hpp>

struct abortable
{
  void abort() {}
  bool is_aborted() {}
};
struct not_abortable
{
};

BOOST_CONCEPT_ASSERT((scl::audio::Abortable<abortable>));
// BOOST_CONCEPT_ASSERT((scl::audio::Abortable<not_abortable>)); // should not compile

TEST(AudioConcept, Abortable) {}
