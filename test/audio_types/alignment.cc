
#include <cstdint>
#include <list>
#include <array>
#include <utility>
#include <type_traits>
#include <cstring> // for memcpy
#include <gtest/gtest.h>
#include <boost/format.hpp>
#include <scl/audio/midi_types.hpp>
#include <scl/audio/audio_types.hpp>

#ifdef __alignas_is_defined
#define SCL_ALIGN(N) alignas(N)
#else
#ifdef __GNUC__
#define SCL_ALIGN(N)  __attribute__ ((__aligned__ (N)))
#else
#error "Need either __GNUC__ or alignas"
#endif
#endif


// using std::pair;
// using std::array;
using namespace scl::audio;

template <class A, class B>
struct pair2
{
  A first  SCL_ALIGN(alignof(A)) SCL_ALIGN(alignof(B));
  B second SCL_ALIGN(alignof(A)) SCL_ALIGN(alignof(B));
};

template <class A, int N>
struct array2
{
  A values[N];
};

// #define pair std::pair
// #define array std::array
#define pair pair2
#define array array2
#define list std::list
#define s32 int32_t
#define s64 int64_t

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
  std::cout << "  offset:    " << offsetof(T, second) << "\n";
}

// struct pair32_64
// {
//   s32 first;
//   s64 second;
// };


TEST(AudioTypes, Alignment)
{
  std::cout << boost::format("Max align: %d\n") % alignof(std::max_align_t);
  /*
    print_name (" midi_simple_message ");
    print_align < midi_simple_message >();

    print_name (" midi_sysex_message ");
    print_align < midi_sysex_message >();

    print_name (" s32 ");
    print_align < s32 >();

    print_name (" s64 ");
    print_align < s64 >();

    print_name (" long double ");
    print_align < long double >();

    // print_name (" pair32_64 ");
    // print_align < pair32_64 >();
    // print_offset< pair32_64 >();

    print_name (" pair<s32,s32> ");
    print_align < pair<s32,s32> >();
    print_offset< pair<s32,s32> >();

    print_name (" pair<s32,s64> ");
    print_align < pair<s32,s64> >();
    print_offset< pair<s32,s64> >();

    print_name (" pair<s64,s32> ");
    print_align < pair<s64,s32> >();
    print_offset< pair<s64,s32> >();

    print_name (" pair<s64,s64> ");
    print_align < pair<s64,s64> >();
    print_offset< pair<s64,s64> >();

    print_name (" array<s64,10> ");
    print_align < array<s64,10> >();

    // print_name (" array<pair32_64,10> ");
    // print_align < array<pair32_64,10> >();

    print_name (" array<pair<s32,s64>,10> ");
    print_align < array<pair<s32,s64>,10> >();

    print_name (" array<pair<s64,s32>,10> ");
    print_align < array<pair<s64,s32>,10> >();

    print_name (" array<pair<s64,s64>,10> ");
    print_align < array<pair<s64,s64>,10> >();

    print_name (" pair<array<pair<s32,s64>,10>,array<s32,20>> ");
    print_align < pair<array<pair<s32,s32>,10>,array<s32,20>> >();
    print_offset< pair<array<pair<s32,s32>,10>,array<s32,20>> >();

    print_name (" pair<array<pair<s32,s32>,10>,array<s32,20>> ");
    print_align < pair<array<pair<s32,s32>,10>,array<s32,20>> >();
    print_offset< pair<array<pair<s32,s32>,10>,array<s32,20>> >();
     */
  // std::cout << "pair2 is trivially copyable: " <<
  //   std::is_trivially_copyable<pair2<int,int>>::value << "\n";
  // std::cout << "std::pair is trivially copyable: " <<
  //   std::is_trivially_copyable<std::pair<int,int>>::value << "\n";
  // std::cout << "std::tuple is trivially copyable: " <<
  //   std::is_trivially_copyable<std::tuple<int,int>>::value << "\n";
}


TEST(AudioTypes, GenAlignment)
{
}