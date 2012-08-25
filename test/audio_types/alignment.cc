
#include <cstring> // for memcpy
#include <utility>
#include <type_traits>
#include <gtest/gtest.h>
#include <scl/audio/midi_types.hpp>
#include <scl/audio/audio_types.hpp>

template <class A, class B>
struct foo { 
  foo() {}
  foo(const foo&) = default;
  foo(foo&&) = default;
  foo(A first, B second) : first(first), second(second) {} 
  A first; B second; }; 

TEST(AudioTypes, PairCopy)
{
  std::pair<int, int> p;
  p.first = 10;
  p.second = 20;

  char                p2[sizeof(p)];
  std::memcpy(p2, &p, sizeof(p));

  p.first = 80;
  p.second = 90;

  std::memcpy(&p, p2, sizeof(p));
  std::cout << p.first << ", " << p.second << "\n";
}
  

TEST(AudioTypes, Alignment)
{ 
  using namespace scl::audio;
  
  std::cout << "midi_simple_message" << "\n";
  std::cout << "  size:      " << sizeof(midi_simple_message) << "\n";
  std::cout << "  alignment: " << alignof(midi_simple_message) << "\n";

  // std::cout << "foo is trivially copyable: " << 
  //   std::is_trivially_copyable<foo<int,int>>::value << "\n";
  // std::cout << "std::pair is trivially copyable: " << 
  //   std::is_trivially_copyable<std::pair<int,int>>::value << "\n";
  // std::cout << "std::tuple is trivially copyable: " << 
  //   std::is_trivially_copyable<std::tuple<int,int>>::value << "\n";
}