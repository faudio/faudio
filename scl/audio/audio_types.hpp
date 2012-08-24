
#pragma once

#include <vector>
#include <list>
#include <utility>
#include <memory>
#include <iostream>
#include <boost/lexical_cast.hpp>

namespace scl
{
  namespace audio
  {
    using sample32 = float;
    using sample64 = double;
    
    constexpr int max_frames = 4096;
    
    template <class A, class B> struct audio_pair
    {
      using type = std::pair<A, B>;
    };
    template <class A> struct audio_list
    {
      using type = std::list<A>;
    };
    template <class A, int MaxFrames = max_frames> struct audio_vector
    {
      using type = std::array<A, MaxFrames>;
    };
    template <class Sample, int Channels> struct audio_channels
    {
      using type = audio_pair<Sample, audio_channels<Sample, Channels - 1>>;
    };
    template <class Sample> struct audio_channels<Sample, 1>
    {
      using type = audio_pair<Sample, void>;
    };
  
  /*
    sample32                  
    sample64
    (T,T)
    [T]
    {T x N}

    identity : () a ~> a
    const    : () b ~> a
    unary    : () a ~> b
    binary   : () (a,b) ~> c
    ternary  : () (a,b,c) ~> d
    random   : () b ~> a
    split    : () a ~> (a,a)
    sequence : () (a ~> b) -> (b ~> c) -> (a ~> c)
    parallel : () (a ~> b) -> (c ~> d) -> ((a,c) ~> (b,d))
    feedback : () ((a,c) ~> (b,c)) -> (a ~> b)
    delay    : () a ~> a
  */

    enum class audio_type_tag
    {
      sample32,
      sample64,
      pair,
      list,
      vector,
    };
    
    struct audio_type
    {                
      using tag_type = audio_type_tag;
      using type_ptr = std::shared_ptr<audio_type>;
      
      tag_type tag;
      type_ptr fst;
      type_ptr snd;
      
      audio_type(tag_type tag) 
        : tag(tag) {}
      
      audio_type(tag_type tag, audio_type fst) 
        : tag(tag)
        , fst(new audio_type(fst)) {}
      
      audio_type(tag_type tag, audio_type fst, audio_type snd) 
        : tag(tag)
        , fst(new audio_type(fst))
        , snd(new audio_type(snd)) {}
      
      std::string str() const
      {   
        using boost::lexical_cast;
        using std::string;

        switch(tag)
        {
          case tag_type::sample32:
            return "f32";
          case tag_type::sample64:
            return "f64";
          case tag_type::pair:
            return "(" + fst->str() + ", " + snd->str() + ")";
          case tag_type::list:
            return "[" + fst->str() + "]";
          case tag_type::vector:
            return "{" + fst->str() + " x " + lexical_cast<string>(max_frames) + "}";
        }
      }
    };

    inline std::ostream& operator<< (std::ostream& a, const audio_type& b)
    {
      return a << b.str();
    }

    namespace dynamic
    {
      namespace type
      {
      using tag_type = audio_type_tag;
      inline audio_type sample32() { return audio_type(tag_type::sample32); }
      inline audio_type sample64() { return audio_type(tag_type::sample64); }
      inline audio_type pair(audio_type fst, audio_type snd) { return audio_type(tag_type::pair, fst, snd); }
      inline audio_type list(audio_type type) { return audio_type(tag_type::list, type); }
      inline audio_type vector(audio_type type) { return audio_type(tag_type::vector, type); }
      }
    }
  
  }
}

