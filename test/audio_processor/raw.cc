
#include <gtest/gtest.h>

#include <scl/audio/processor/simple/identity.hpp>
#include <scl/audio/processor/simple/constant.hpp>
#include <scl/audio/processor/simple/unary.hpp>
// #include <scl/audio/processor/simple/binary.hpp>
// #include <scl/audio/processor/simple/ternary.hpp>
// #include <scl/audio/processor/simple/random.hpp>
#include <scl/audio/processor/simple/split.hpp>
// #include <scl/audio/processor/simple/delay.hpp>

#include <scl/audio/processor/compound/sequence.hpp>
#include <scl/audio/processor/compound/parallel.hpp>
// #include <scl/audio/processor/compound/feedback.hpp>

using namespace scl;
using namespace scl::audio;
using namespace scl::audio::processor;

TEST(AudioProcessorRaw, Identity)
{
  audio_type typ = audio_type::vector(audio_type::int8(), 10);
  char in[11]  = "abcdefghij";
  char out[11] = "__________";                                                      
  
  raw_processor_ptr p { new raw_identity_processor(typ) };
  p->prepare(NULL);
  p->process(NULL, (ptr_t) in, (ptr_t) out, NULL);
  p->cleanup(NULL);
  std::cout << "Input:  " << in << "\n";
  std::cout << "Output: " << out << "\n";
}

TEST(AudioProcessorRaw, Constant)
{
  audio_type typ = audio_type::vector(audio_type::int8(), 10);
  char in[11]  = "abcdefghij";
  char in2[11] = "mnopqrstuv";
  char out[11] = "__________";

  raw_processor_ptr p { new raw_constant_processor(typ, typ) };
  p->prepare((ptr_t) in2);
  p->process(NULL, (ptr_t) in, (ptr_t) out, NULL);
  p->cleanup(NULL);
  std::cout << "Input:  " << in << "\n";
  std::cout << "Output: " << out << "\n";
}

TEST(AudioProcessorRaw, Split)
{
  audio_type typ = audio_type::vector(audio_type::int8(), 10);
  char in[11]  = "abcdefghij";
  char out[21] = "____________________";

  raw_processor_ptr p { new raw_split_processor(typ) };
  p->prepare(NULL);
  p->process(NULL, (ptr_t) in, (ptr_t) out, NULL);
  p->cleanup(NULL);
  std::cout << "Input:  " << in << "\n";
  std::cout << "Output: " << out << "\n";
}


void succ(void* _, char* x, char* r) { for(int i = 0; i < 10; ++i) *(r+i) = *(x+i) + 1; }
void pred(void* _, char* x, char* r) { for(int i = 0; i < 10; ++i) *(r+i) = *(x+i) - 1; }

TEST(AudioProcessorRaw, Unary)
{ 
  using function_type = raw_unary_processor::function_type;
  audio_type typ = audio_type::vector(audio_type::int8(), 10);
  char in[11]  = "abcdefghij";
  char out[11] = "__________";
  
  raw_processor_ptr p { new raw_unary_processor(typ, typ, (function_type) succ, NULL, NULL) };
  raw_processor_ptr q { new raw_unary_processor(typ, typ, (function_type) pred, NULL, NULL) };

  p->prepare(NULL);
  q->process(NULL, (ptr_t) in, (ptr_t) out, NULL);
  p->cleanup(NULL);
  std::cout << "Input:  " << in << "\n";
  std::cout << "Output: " << out << "\n";
}


TEST(AudioProcessorRaw, Seq)
{ 
  using function_type = raw_unary_processor::function_type;
  audio_type typ = audio_type::vector(audio_type::int8(), 10);
  char in[11]  = "abcdefghij";
  char out[11] = "__________";
  
  raw_processor_ptr p { new raw_unary_processor(typ, typ, (function_type) succ, NULL, NULL) };
  raw_processor_ptr q { new raw_unary_processor(typ, typ, (function_type) pred, NULL, NULL) };
  raw_processor_ptr s { new raw_sequence_processor(p, p) };
  raw_processor_ptr t { new raw_sequence_processor(s, s) };

  t->prepare(NULL);
  t->process(NULL, (ptr_t) in, (ptr_t) out, NULL);
  t->cleanup(NULL);
  std::cout << "Input:  " << in << "\n";
  std::cout << "Output: " << out << "\n";
}

TEST(AudioProcessorRaw, Par)
{ 
  using function_type = raw_unary_processor::function_type;
  audio_type typ = audio_type::vector(audio_type::int8(), 10);
  char in[21]  = "abcdefghijabcdefghij";
  char out[21] = "____________________";
  
  raw_processor_ptr p { new raw_unary_processor(typ, typ, (function_type) succ, NULL, NULL) };
  raw_processor_ptr q { new raw_unary_processor(typ, typ, (function_type) pred, NULL, NULL) };
  raw_processor_ptr s { new raw_parallel_processor(p, q) };

  s->prepare(NULL);
  s->process(NULL, (ptr_t) in, (ptr_t) out, NULL);
  s->cleanup(NULL);
  std::cout << "Input:  " << in << "\n";
  std::cout << "Output: " << out << "\n";
}




