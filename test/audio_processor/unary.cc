
#include <gtest/gtest.h>
#include <scl/audio/concept/processor.hpp>
#include <scl/audio/processor/simple/unary.hpp>

using namespace scl;
using namespace scl::audio;
using namespace scl::audio::processor;




/*
  template <class A>
  struct audio_type_info
  {
    static size_t object_size() { return sizeof(A); }
    static size_t binary_size() { return audio_type::get<A>().size(); }
  };

  template <class A>
  struct audio_type_serializer
  {
    struct not_an_audio_type : public fail {};
    static void serialize(A*, ptr_t) { throw not_an_audio_type(); }
    static void unserialize(ptr_t, A*) { throw not_an_audio_type(); }
  };

  template <>
  struct audio_type_serializer<int8>
  {
    static void serialize(int8* x, ptr_t y)
    { 
      y = x;
    }
    static void unserialize(ptr_t x, int8* y)
    {      
      y = x;
    }
  };

  template <>
  struct audio_type_serializer<int16>
  {
    static void serialize(int16* x, ptr_t y)
    { 
      y = x;
    }
    static void unserialize(ptr_t x, int16* y)
    {      
      y = x;
    }
  };

  template <>
  struct audio_type_serializer<int32>
  {
    static void serialize(int32* x, ptr_t y)
    { 
      y = x;
    }
    static void unserialize(ptr_t x, int32* y)
    {      
      y = x;
    }
  };

  template <>
  struct audio_type_serializer<sample32>
  {
    static void serialize(sample32* x, ptr_t y)
    { 
      y = x;
    }
    static void unserialize(ptr_t x, sample32* y)
    {      
      y = x;
    }
  };

  template <>
  struct audio_type_serializer<sample64>
  {
    static void serialize(sample64* x, ptr_t y)
    { 
      y = x;
    }
    static void unserialize(ptr_t x, sample64* y)
    {      
      y = x;
    }
  };

  template <>
  struct audio_type_serializer<unit>
  {
    static void serialize(unit* x, ptr_t y)
    { 
      y = x;
    }
    static void unserialize(ptr_t x, unit* y)
    {      
      y = x;
    }
  };

  template <class A>
  struct audio_type_serializer<std::list<A>>
  {
    static void serialize(int8* x, ptr_t y)
    {
    }
    static void unserialize(ptr_t x, A* y)
    {
    }
  };

  template <class A, class B>
  struct audio_type_serializer<std::pair<A, B>>
  {
    static void serialize(int8* x, ptr_t y)
    { 
    }
    static void unserialize(ptr_t x, int8* y)
    {      
    }
  };

  template <class A, size_t N>
  struct audio_type_serializer<std::array<A, N>>
  {
    static void serialize(int8* x, ptr_t y)
    { 
    }
    static void unserialize(ptr_t x, int8* y)
    {      
    }
  };*/

        


TEST(AudioProcessor, Unary)
{
  using in  = sample32;
  using out = sample32;  

  // std::function<sample32(sample32)> f { [] (sample32 x) -> sample32 { return 0; } };
  // std::function<sample32(sample32)> g = f;

  // Lambda captures addend by reference
  in addend = 500;

  // std::function<void(const in&, out&)> f = 
  //   [&addend] (const in& x, out& r) { r = x + addend; };
  std::function<out(in)> f = 
    [&addend] (in x) -> out { return x + addend; };

  unary_processor<in, out> p (f);        

  unit_list msg;
  in  x = 100;
  out y;
  p.prepare(nullptr);
  p.process(msg, x, y, msg);
  
  std::cout << "Argument: " << x << "\n";
  std::cout << "Result:   " << y << "\n";

  addend = -500;
  p.process(msg, x, y, msg);  
  std::cout << "Argument: " << x << "\n";
  std::cout << "Result:   " << y << "\n";

}
