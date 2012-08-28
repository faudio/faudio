
#pragma once

#include <list>
#include <array>
#include <utility> // for pair
#include <memory> // for shared_ptr
#include <boost/lexical_cast.hpp>
#include <scl/audio/midi_types.hpp>

namespace scl
{
  namespace audio
  {
    /*
      Type system of audio computations:

          sample32      32-bit floating point sample
          sample64      64-bit floating point sample
          midi_message  Midi message

          (A,B)         Pair of A and B
          [A]           Variable sized list of A
          {A x N}       N-sized array of A

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

      The audio_type class representation these types at runtime and provides
      dynamic type checking (but not inference).

      The static version of the audio types:
        sample32;
        sample64;
        audio_pair<A,B>::type;
        audio_list<A>::type;
        audio_vector<A,N>::type;

      The runtime representation of the audio types:
        audio_type::sample32();
        audio_type::sample64();
        audio_type::pair(audio_type fst, audio_type snd);
        audio_type::list(audio_type type);
        audio_type::vector(audio_type type, size_t size);

        audio_type audio_type::get<T>()
          Returns the runtime representation of the given type.

      Conversion functions, specialized for each audio type:
        void audio_read<T>(const raw_buffer&, T&);
        void audio_write<T>(const T&, raw_buffer&);

      Binary format:
        - Concrete types maps to their binary representation
        - (A,B)  maps to the binary representation of A padded with zero to occupy 8n bytes,
          followed by the binary representation of B padded with 0 to occupy 8n bytes
        - <A x N> maps to an array of N elements of type A
        - [A] maps to a chase of A
    */

    constexpr int max_frames = 4096;

    using unit     = nullptr_t;
    using sample32 = float;
    using sample64 = double;

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

    template <class Sample, int N> struct audio_channels
    {
      using type = typename audio_pair<Sample, typename audio_channels<Sample, N - 1>::type>::type;
    };
    template <class Sample> struct audio_channels<Sample, 0>
    {
      using type = unit; // FIXME
    };

    enum class audio_type_tag
    {
      sample32,
      sample64,
      unit,
      pair,
      list,
      vector,
    };

    class audio_type;
    namespace type
    {
      inline audio_type sample32();
      inline audio_type sample64();
      inline audio_type unit();
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
      size_t n;

      audio_type(tag_type tag)
        : tag(tag)
        , fst()
        , snd()
        , n() {}

      audio_type(tag_type tag, audio_type fst)
        : tag(tag)
        , fst(new audio_type(fst))
        , snd()
        , n() {}

      audio_type(tag_type tag, audio_type fst, audio_type snd)
        : tag(tag)
        , fst(new audio_type(fst))
        , snd(new audio_type(snd))
        , n() {}

      audio_type(tag_type tag, audio_type fst, size_t n)
        : tag(tag)
        , fst(new audio_type(fst))
        , snd()
        , n(n) {}

      friend bool operator ==(const audio_type& a, const audio_type& b);
      friend bool operator !=(const audio_type& a, const audio_type& b);
      
    public:
      audio_type(const audio_type& other)
        : tag(other.tag)
        , fst(other.fst)
        , snd(other.snd)
        , n(other.n) {}

      bool is_pair()
      {
        return tag == tag_type::pair;
      }

      size_t size()
      {
        switch (tag)
        {
        case tag_type::sample32:
          return sizeof(sample32);
        case tag_type::sample64:
          return sizeof(sample64);
        case tag_type::unit:
          return sizeof(unit);
        case tag_type::list:
          return sizeof(ptr_t);
        case tag_type::vector:
          return fst->size() * n;
        case tag_type::pair:
          return next_aligned(offset() + snd->size(), align());
        }
      }

      size_t align()
      {
        switch (tag)
        {
        case tag_type::sample32:
          return alignof(sample32);
        case tag_type::sample64:
          return alignof(sample64);
        case tag_type::unit:
          return alignof(unit);
        case tag_type::list:
          return alignof(ptr_t);
        case tag_type::vector:
          return fst->align();
        case tag_type::pair:
          return std::max(fst->align(), snd->align());
        }
      }

      size_t offset()
      {
        switch (tag)
        {
        case tag_type::pair:
          return next_aligned(fst->size(), snd->align());
        default:
          return 0;
        }
      }

      int kind() const
      {
        switch (tag)
        {
        case tag_type::sample32:
          return 0;
        case tag_type::sample64:
          return 0;
        case tag_type::unit:
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
        case tag_type::unit:
          return "()";
        case tag_type::list:
          return "[" + fst->name() + "]";
        case tag_type::pair:
          return "(" + fst->name() + ", " + snd->name() + ")";
        case tag_type::vector:
          return "<" + fst->name() + " x " + lexical_cast<string>(n) + ">";
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
        case tag_type::unit:
          return "unit";
        case tag_type::list:
          return "audio_list<" + fst->declaration() + ">::type";
        case tag_type::pair:
          return "audio_pair<" + fst->declaration() + "," + snd->declaration() + ">::type";
        case tag_type::vector:
          return "audio_vector<" + fst->declaration() + "," + lexical_cast<string>(n) + ">::type";
        }
      }

      static audio_type sample32()
      {
        return audio_type(tag_type::sample32);
      }
      static audio_type sample64()
      {
        return audio_type(tag_type::sample64);
      }
      static audio_type unit()
      {
        return audio_type(tag_type::unit);
      }
      static audio_type pair(audio_type fst, audio_type snd)
      {
        return audio_type(tag_type::pair, fst, snd);
      }
      static audio_type list(audio_type type)
      {
        return audio_type(tag_type::list, type);
      }
      static audio_type vector(audio_type type, size_t n)
      {
        return audio_type(tag_type::vector, type, n);
      }
      
      template <class A> static audio_type get();      
    };

    inline bool operator ==(const audio_type& a, const audio_type& b)
    {
      return a.tag == b.tag
             && compare_ptr(a.fst, b.fst)
             && compare_ptr(a.snd, b.snd)
             && a.n == b.n;
    }

    inline bool operator !=(const audio_type& a, const audio_type& b)
    {
      return !(a == b);
    }

    inline std::ostream& operator<< (std::ostream& a, const audio_type& b)
    {
      return a << b.name();
    }

    namespace detail
    {
      /** @cond internal */

      template <class A>
      struct get_audio_type
      {
        struct not_an_audio_type : public fail {};
        static audio_type value()
        {
          throw not_an_audio_type();
        }
      };

      template <>
      struct get_audio_type<sample32>
      {
        static audio_type value()
        {
          return audio_type::sample32();
        }
      };

      template <>
      struct get_audio_type<sample64>
      {
        static audio_type value()
        {
          return audio_type::sample64();
        }
      };

      template <>
      struct get_audio_type<unit>
      {
        static audio_type value()
        {
          return audio_type::unit();
        }
      };

      template <class A>
      struct get_audio_type<std::list<A>>
      {
        static audio_type value()
        {
          audio_type a = get_audio_type<A>::value();
          return audio_type::list(a);
        }
      };

      template <class A, class B>
      struct get_audio_type<std::pair<A, B>>
      {
        static audio_type value()
        {
          audio_type a = get_audio_type<A>::value();
          audio_type b = get_audio_type<B>::value();
          return audio_type::pair(a, b);
        }
      };

      template <class A, size_t N>
      struct get_audio_type<std::array<A, N>>
      {
        static audio_type value()
        {
          audio_type a = get_audio_type<A>::value();
          return audio_type::vector(a, N);
        }
      };
    }   

    /** @endcond internal */
    
    template <class A>
    audio_type audio_type::get()
    {
      return detail::get_audio_type<A>::value();
    };
  }
}

