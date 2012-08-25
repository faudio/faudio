
#include <array>
#include <utility>
#include <type_traits>
#include <cstring> // for memcpy
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
  
#define print_name(TYPE) \
  std::cout << TYPE << "\n"

template<class T>
void print_align()
{
  std::cout << "  size:      " << sizeof(T) << "\n";
  std::cout << "  alignment: " << alignof(T) << "\n";
}
template<class T>
void print_offset()
{
  std::cout << "  offset:    " << offsetof(T,second) << "\n";
}
  
TEST(AudioTypes, Alignment)
{                               
  using std::pair;
  using std::array;
  using namespace scl::audio;

  print_name (" midi_simple_message ");
  print_align < midi_simple_message >();
               
  print_name (" midi_sysex_message ");
  print_align < midi_sysex_message >();
               
  print_name (" sample32 ");
  print_align < sample32 >();

  print_name (" pair<sample32,sample32> ");
  print_align < pair<sample32,sample32> >();
  print_offset< pair<sample32,sample32> >();

  print_name (" pair<sample32,sample64> ");
  print_align < pair<sample32,sample64> >();
  print_offset< pair<sample32,sample64> >();

  print_name (" pair<sample64,sample64> ");
  print_align < pair<sample64,sample64> >();
  print_offset< pair<sample64,sample64> >();

  print_name (" array<sample64,10> ");
  print_align < array<sample64,10> >();

  print_name (" pair<array<pair<sample32,sample64>10>,array<sample32,20>> ");
  print_align < pair<array<pair<sample32,sample32>,10>,array<sample32,20>> >();
  print_offset< pair<array<pair<sample32,sample32>,10>,array<sample32,20>> >();

  print_name (" pair<array<pair<sample32,sample32>10>,array<sample32,20>> ");
  print_align < pair<array<pair<sample32,sample32>,10>,array<sample32,20>> >();
  print_offset< pair<array<pair<sample32,sample32>,10>,array<sample32,20>> >();

  // std::cout << "foo is trivially copyable: " << 
  //   std::is_trivially_copyable<foo<int,int>>::value << "\n";
  // std::cout << "std::pair is trivially copyable: " << 
  //   std::is_trivially_copyable<std::pair<int,int>>::value << "\n";
  // std::cout << "std::tuple is trivially copyable: " << 
  //   std::is_trivially_copyable<std::tuple<int,int>>::value << "\n";
}