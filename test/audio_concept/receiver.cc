
#include <gtest/gtest.h>
#include <scl/audio/concept/receiver.hpp>

struct receiver
{
  using address_type = int;
  using message_type = int;
  
  template <class IntRange>
  void receive(int a, IntRange xs)
  {
  }
};
struct no_receiver
{
};

BOOST_CONCEPT_ASSERT((scl::audio::Receiver<receiver>));
// BOOST_CONCEPT_ASSERT((scl::audio::Receiver<no_receiver>)); // should not compile

TEST(AudioConcept, Receiver) {}
