
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
    /*
      sample32
      sample64
      (T,T)
      [T]
      {T x N}

      identity : a ~> a
      const    : b ~> a
      unary    : a ~> b
      binary   : (a,b) ~> c
      ternary  : (a,(b,c)) ~> d
      random   : b ~> a
      split    : a ~> (a,a)
      sequence : (a ~> b) -> (b ~> c) -> (a ~> c)
      parallel : (a ~> b) -> (c ~> d) -> ((a,c) ~> (b,d))
      feedback : ((a,c) ~> (b,c)) -> (a ~> b)
      delay    : a ~> a


      -- how much to add to make the next element align with a
      pad x a = (a - x) % a

      size sample32 = 4
      size sample64 = 8
      size [a]      = <size of ptr>
      size [a x n]  = size a * n
      size (a,b)    = pad (size a) (align b) + pad (size b) (align (a,b))

      align sample32 = 4
      align sample64 = 8
      align [a]      = <align of ptr>
      align [a x n]  = align a
      align (a,b)    = align a `max` align b
    */


    enum class audio_type_tag
    {
      sample32,
      sample64,
      pair,
      list,
      vector,
    };

    class audio_type;
    namespace type
    {   
      inline audio_type sample32();
      inline audio_type sample64();
      inline audio_type pair(audio_type fst, audio_type snd);
      inline audio_type list(audio_type type);
      inline audio_type vector(audio_type type, size_t size);
    }

    class audio_type
    {
      using tag_type = audio_type_tag;
      using type_ptr = std::shared_ptr<audio_type>;

      tag_type tag;
      type_ptr fst;
      type_ptr snd;
      size_t size;

      audio_type(tag_type tag)
        : tag(tag) {}

      audio_type(tag_type tag, audio_type fst)
        : tag(tag)
        , fst(new audio_type(fst)) {}

      audio_type(tag_type tag, audio_type fst, audio_type snd)
        : tag(tag)
        , fst(new audio_type(fst))
        , snd(new audio_type(snd)) {}

      audio_type(tag_type tag, audio_type fst, size_t size)
        : tag(tag)
        , fst(new audio_type(fst))
        , size(size) {}

      friend audio_type type::sample32();
      friend audio_type type::sample64();
      friend audio_type type::pair(audio_type fst, audio_type snd);
      friend audio_type type::list(audio_type type);
      friend audio_type type::vector(audio_type type, size_t size);

    public:

      audio_type(const audio_type& other)
        : tag(other.tag)
        , fst(other.fst)
        , snd(other.snd)
        , size(other.size) {}

      // ~audio_type() { std::cout << "<<<<< Destroying " << this << "\n"; }

      bool is_pair()
      {
        return tag == tag_type::pair;
      }

      int kind() const
      {
        switch (tag)
        {
        case tag_type::sample32:
          return 0;
        case tag_type::sample64:
          return 0;
        case tag_type::pair:
          return 2;
        case tag_type::list:
          return 1;
        case tag_type::vector:
          return 1;
        }
      }

      int levels() const
      {
        if (kind() == 0)
          return 0;
        if (kind() == 1)
          return fst->levels() + 1;
        else
          return std::max(fst->levels(), snd->levels()) + 1;
      }

      std::string name() const
      {
        using boost::lexical_cast;
        using std::string;
        switch (tag)
        {
        case tag_type::sample32:
          return "sample32";
        case tag_type::sample64:
          return "sample64";
        case tag_type::list:
          return "[" + fst->name() + "]";
        case tag_type::pair:
          return "(" + fst->name() + ", " + snd->name() + ")";
        case tag_type::vector:
          return "<" + fst->name() + " x " + lexical_cast<string>(size) + ">";
        }
      }

      std::string declaration() const
      {
        using boost::lexical_cast;
        using std::string;
        switch (tag)
        {
        case tag_type::sample32:
          return "sample32";
        case tag_type::sample64:
          return "sample64";
        case tag_type::list:
          return "list<" + fst->declaration() + ">";
        case tag_type::pair:
          return "pair<" + fst->declaration() + "," + snd->declaration() + ">";
        case tag_type::vector:
          return "array<" + fst->declaration() + "," + lexical_cast<string>(size) + ">";
        }
      }

    };

    inline std::ostream& operator<< (std::ostream& a, const audio_type& b)
    {
      return a << b.name();
    }

    namespace type
    {
      using tag_type = audio_type_tag;
      inline audio_type sample32()
      {
        return audio_type(tag_type::sample32);
      }
      inline audio_type sample64()
      {
        return audio_type(tag_type::sample64);
      }
      inline audio_type pair(audio_type fst, audio_type snd)
      {
        return audio_type(tag_type::pair, fst, snd);
      }
      inline audio_type list(audio_type type)
      {
        return audio_type(tag_type::list, type);
      }
      inline audio_type vector(audio_type type, size_t size)
      {
        return audio_type(tag_type::vector, type, size);
      }
    }

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
      using type = audio_pair < Sample, audio_channels < Sample, Channels - 1 >>;
    };
    template <class Sample> struct audio_channels<Sample, 1>
    {
      using type = audio_pair<Sample, void>;
    };

    template <class A>
    struct get_audio_type
    {
    };

    template <>
    struct get_audio_type<sample32>
    {
      static audio_type value()
      {
        return type::sample32();
      }
    };
    template <>
    struct get_audio_type<sample64>
    {
      static audio_type value()
      {
        return type::sample64();
      }
    };

    template <class A>
    struct get_audio_type<std::list<A>>
    {
      static audio_type value()
      {
        audio_type a = get_audio_type<A>::value();
        return type::list(a);
      }
    };
    template <class A, class B>
    struct get_audio_type<std::pair<A, B>>
    {
      static audio_type value()
      {
        audio_type a = get_audio_type<A>::value();
        audio_type b = get_audio_type<B>::value();
        return type::pair(a, b);
      }
    };
    template <class A, size_t N>
    struct get_audio_type<std::array<A, N>>
    {
      static audio_type value()
      {
        audio_type a = get_audio_type<A>::value();
        return type::vector(a, N);
      }
    };
  }
}

