
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

          int8          Signed 8-bit integer
          int32         Signed 32-bit integer
          int64         Signed 64-bit integer
          sample32      32-bit floating point sample
          sample64      64-bit floating point sample
          midi_message  Midi message
          ()            Unit value

          (A,B)         Pair of A and B
          [A]           Variable sized list of A
          {A x N}       N-sized array of A
          A ~> B        Processor from A to B

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
        - (A,B)  maps to the binary representation of A followed by a padding, followed by the represention of B folowed by a padding
          - The size and offset of (A,B) is determined by audio_type
        - <A x N> maps to A[N]
        - [A] maps to chase<A>::track_pointer
    */

    constexpr size_t max_frames = 4096;

    using int8     = int8_t;
    using int16    = int16_t;
    using int32    = int32_t;
    using sample32 = float;
    using sample64 = double;
    using unit     = nullptr_t;

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
      using type = typename audio_pair < Sample, typename audio_channels < Sample, N - 1 >::type >::type;
    };

    template <class Sample> struct audio_channels<Sample, 0>
    {
      using type = unit;
    };

    enum class audio_type_tag
    {      
      int8,
      int16,
      int32,
      sample32,
      sample64,
      unit,
      pair,
      list,
      vector,
    };

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
        case tag_type::int8:
          return sizeof(::scl::audio::int8);
        case tag_type::int16:
          return sizeof(::scl::audio::int16);
        case tag_type::int32:
          return sizeof(::scl::audio::int32);
        case tag_type::sample32:
          return sizeof(::scl::audio::sample32);
        case tag_type::sample64:
          return sizeof(::scl::audio::sample64);
        case tag_type::unit:
          return sizeof(::scl::audio::unit);
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
        case tag_type::int8:
          return alignof(::scl::audio::int8);
        case tag_type::int16:
          return alignof(::scl::audio::int16);
        case tag_type::int32:
          return alignof(::scl::audio::int32);
        case tag_type::sample32:
          return alignof(::scl::audio::sample32);
        case tag_type::sample64:
          return alignof(::scl::audio::sample64);
        case tag_type::unit:
          return alignof(::scl::audio::unit);
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
        case tag_type::pair:
          return 2;
        case tag_type::list:
          return 1;
        case tag_type::vector:
          return 1;
        default:
          return 0;
        }
      }

      int levels() const
      {
        switch (kind())
        {
        case 0:
          return 0;
        case 1:
          return fst->levels() + 1;
        default:
          return std::max(fst->levels(), snd->levels()) + 1;
        }
      }

      std::string name() const
      {
        using boost::lexical_cast;
        using std::string;
        switch (tag)
        {
        case tag_type::int8:
          return "int8";
        case tag_type::int16:
          return "int16";
        case tag_type::int32:
          return "int32";
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
        case tag_type::int8:
          return "int8";
        case tag_type::int16:
          return "int16";
        case tag_type::int32:
          return "int32";
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

      static audio_type int8()
      {
        return audio_type(tag_type::int8);
      }
      static audio_type int16()
      {
        return audio_type(tag_type::int16);
      }
      static audio_type int32()
      {
        return audio_type(tag_type::int32);
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

    /** @cond internal */

    namespace detail
    {
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
      struct get_audio_type<int8>
      {
        static audio_type value()
        {
          return audio_type::int8();
        }
      };

      template <>
      struct get_audio_type<int16>
      {
        static audio_type value()
        {
          return audio_type::int16();
        }
      };

      template <>
      struct get_audio_type<int32>
      {
        static audio_type value()
        {
          return audio_type::int32();
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

