
#include <gtest/gtest.h>
#include <boost/format.hpp>
#include <scl/audio/midi_types.hpp>
#include <scl/audio/audio_types.hpp>

TEST(AudioTypes, Size)
{                                                                        
  using namespace scl;
  using namespace scl::audio;
  
  std::cout << boost::format("%d\n") % next_aligned(0, 1);
  std::cout << boost::format("%d\n") % next_aligned(1, 8);
  std::cout << boost::format("%d\n") % next_aligned(-15, 8);
  std::cout << boost::format("%d\n") % next_aligned(1233, 8);

  std::cout << boost::format("%d\n") % sizeof(audio_type);
}