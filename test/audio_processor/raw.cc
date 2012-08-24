
#include <gtest/gtest.h>
#include <scl/audio/processor/simple/identity.hpp>
#include <scl/audio/processor/simple/constant.hpp>
#include <scl/audio/processor/simple/unary.hpp>
// #include <scl/audio/processor/simple/binary.hpp>
// #include <scl/audio/processor/simple/ternary.hpp>
// #include <scl/audio/processor/simple/random.hpp>
#include <scl/audio/processor/compound/sequence.hpp>
#include <scl/audio/processor/compound/parallel.hpp>
#include <scl/audio/processor/compound/split.hpp>
#include <scl/audio/processor/compound/delay.hpp>
// #include <scl/audio/processor/compound/feedback.hpp>

TEST(AudioProcessor, RawIdentity)
{
  using namespace scl::audio::processor;
  
  char in[10]  = "abcdefghi"; 
  char out[10];          
  
  raw_processor_ptr p { new raw_identity_processor(sizeof(char) * 10) };
  p->process(NULL, (intptr_t)in, (intptr_t)out, NULL);
  
  std::cout << "Input:  " << in << "\n";
  std::cout << "Output: " << out << "\n";
}

TEST(AudioProcessor, RawConstant)
{
  using namespace scl::audio::processor;
  
  char in[10]  = "abcdefghi"; 
  char in2[10] = "mnopqrstu"; 
  char out[10];          
  
  raw_processor_ptr p { new raw_constant_processor(sizeof(char) * 10) };
  p->prepare((intptr_t) in2);
  p->process(NULL, (intptr_t)in, (intptr_t)out, NULL);
  
  std::cout << "Input:  " << in << "\n";
  std::cout << "Output: " << out << "\n";
}
