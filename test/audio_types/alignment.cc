
#include <array>
#include <utility>
#include <type_traits>
#include <cstring> // for memcpy
#include <gtest/gtest.h>
#include <scl/audio/midi_types.hpp>
#include <scl/audio/audio_types.hpp>

// using std::pair;
// using std::array;
using namespace scl::audio;

template <class A, class B>
struct pair2 { 
  // pair2() {}
  // pair2(const pair2&) = default;
  // pair2(pair2&&) = default;
  // pair2(A first, B second) : first(first), second(second) {} 
  alignas(8) A first; 
  alignas(8) B second; }; 

template <class A, int N>
struct array2 { 
  alignas(8) A values[N]; }; 

// #define pair std::pair
// #define array std::array
#define pair pair2
#define array array2
#define sample32 int32_t
#define sample64 int64_t

TEST(AudioTypes, PairCopy)
{
  pair<int, int> p;
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

struct pair32_64
{
  sample32 first;
  sample64 second;
};

  
TEST(AudioTypes, Alignment)
{                               

  print_name (" midi_simple_message ");
  print_align < midi_simple_message >();
               
  print_name (" midi_sysex_message ");
  print_align < midi_sysex_message >();
               
  print_name (" sample32 ");
  print_align < sample32 >();

  print_name (" sample64 ");
  print_align < sample64 >();

  print_name (" pair32_64 ");
  print_align < pair32_64 >();
  print_offset< pair32_64 >();

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

  print_name (" array<pair32_64,10> ");
  print_align < array<pair32_64,10> >();

  print_name (" array<pair<sample32,sample64>,10> ");
  print_align < array<pair<sample32,sample32>,10> >();

  print_name (" array<pair<sample64,sample64>,10> ");
  print_align < array<pair<sample64,sample32>,10> >();

  print_name (" pair<array<pair<sample32,sample64>,10>,array<sample32,20>> ");
  print_align < pair<array<pair<sample32,sample32>,10>,array<sample32,20>> >();
  print_offset< pair<array<pair<sample32,sample32>,10>,array<sample32,20>> >();

  print_name (" pair<array<pair<sample32,sample32>,10>,array<sample32,20>> ");
  print_align < pair<array<pair<sample32,sample32>,10>,array<sample32,20>> >();
  print_offset< pair<array<pair<sample32,sample32>,10>,array<sample32,20>> >();

  // std::cout << "pair2 is trivially copyable: " << 
  //   std::is_trivially_copyable<pair2<int,int>>::value << "\n";
  // std::cout << "std::pair is trivially copyable: " << 
  //   std::is_trivially_copyable<std::pair<int,int>>::value << "\n";
  // std::cout << "std::tuple is trivially copyable: " << 
  //   std::is_trivially_copyable<std::tuple<int,int>>::value << "\n";
}


/*
   OS X
    midi_simple_message 
     size:      3
     alignment: 1
    midi_sysex_message 
     size:      8
     alignment: 4
    sample32 
     size:      4
     alignment: 4
    pair<sample32,sample32> 
     size:      8
     alignment: 4
     offset:    4
    pair<sample32,sample64> 
     size:      12
     alignment: 4
     offset:    4
    pair<sample64,sample64> 
     size:      16
     alignment: 4
     offset:    8
    array<sample64,10> 
     size:      80
     alignment: 4
    pair<array<pair<sample32,sample64>10>,array<sample32,20>> 
     size:      160
     alignment: 4
     offset:    80
    pair<array<pair<sample32,sample32>10>,array<sample32,20>> 
     size:      160
     alignment: 4
     offset:    80
   
   Win
   
    midi_simple_message                                                                               
     size:      3                                                                                     
     alignment: 1                                                                                     
    midi_sysex_message                                                                                
     size:      8                                                                                     
     alignment: 4                                                                                     
    sample32                                                                                          
     size:      4                                                                                     
     alignment: 4                                                                                     
    pair<sample32,sample32>                                                                           
     size:      8                                                                                     
     alignment: 4                                                                                     
     offset:    4                                                                                     
    pair<sample32,sample64>                                                                           
     size:      16                                                                                    
     alignment: 8                                                                                     
     offset:    8                                                                                     
    pair<sample64,sample64>                                                                           
     size:      16                                                                                    
     alignment: 8
     offset:    8
    array<sample64,10>                                                                                
     size:      80                                                                                    
     alignment: 8                                                                                     
    pair<array<pair<sample32,sample64>10>,array<sample32,20>>                                         
     size:      160                                                                                   
     alignment: 4                                                                                     
     offset:    80                                                                                    
    pair<array<pair<sample32,sample32>10>,array<sample32,20>>                                         
     size:      160                                                                                   
     alignment: 4                                                                                     
     offset:    80                                                                                    
    
   
 */