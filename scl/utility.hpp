
#pragma once

#include <memory> // for unique_ptr
#include <boost/format.hpp>
#include <boost/optional.hpp>

#define SCL_STANDARD_ASSIGN(THIS_TYPE)    \
  void operator= (const THIS_TYPE& other) \
  {                                       \
    THIS_TYPE b = other;                  \
    return b.swap(*this);                 \
  }                                       \
  \
  void operator= (THIS_TYPE&& other)      \
  {                                       \
    THIS_TYPE b = std::move(other);       \
    return b.swap(*this);                 \
  }                                       \
   
namespace scl
{
  using boost::none;
  using boost::none_t;
  using boost::optional;

  /**
      Counterpart to fail.
   */
  struct succeed {};

  /**
      A class that can be created to force a compile time error.
  */
  struct fail
  {
    fail() = delete;
  };

  template <bool T> struct try_ : public fail {};
  template <> struct try_<true> : public succeed {};

  /**
      Generic size.
   */
  template <class Sized>
  inline typename Sized::size_type size(Sized& x)
  {
    return x.size();
  }

  /**
      Equality of referenced values modulo null.
   */
  template <class A, class B>
  bool compare_ptr(A a, B b)
  {
    if (a)
    {
      if (b)
        return *a == *b;
      else
        return false;
    }
    if (b)
    {
      return false;
    }
    return true;
  }

  using std::intptr_t;
  using std::nullptr_t;

  /**
      Generic pointer type with byte-based arithmetic.
   */
  using ptr_t = unsigned char* ;

  /**
      Generic pointer type without arithmetic.
   */
  using voidptr_t = void* ;

  inline intptr_t pad(intptr_t x, size_t a)
  {
    return (a - x) % a;
  }

  inline intptr_t next_aligned(intptr_t x, size_t a)
  {
    return x + pad(x, a);
  }

  template <class A>
  inline A* next_aligned(A* x, size_t a)
  {
    return (intptr_t) next_aligned((intptr_t) x, a);
  }

  /**
      A moveable, non-copyable byte buffer.
   */
  class raw_buffer
  {
    std::unique_ptr<char> mBuffer;
    size_t mSize;

  public:
    raw_buffer() : mSize(0) {}

    void reset(size_t size)
    {
      if (size > 0)
        mBuffer.reset(new char[size]);
      else
        mBuffer.reset();
      mSize = size;
    }

    void clear()
    {
      reset(0);
    }

    ptr_t begin()
    {
      return (ptr_t) mBuffer.get();
    }

    ptr_t end()
    {
      return begin() + size();
    }

    size_t size()
    {
      return mSize;
    }
  };

  inline ptr_t raw_copy(ptr_t begin, ptr_t end, ptr_t out)
  {
    return (ptr_t) std::copy(begin, end, out);
  }

  inline void raw_dump(ptr_t begin,
                       ptr_t end,
                       bool hex = true,
                       size_t group_size = 4,
                       size_t column_size = 16)
  {
    boost::format fmt;
    if (hex)
      fmt = boost::format("%02x");
    else
      fmt = boost::format("%04d");
    std::cout << boost::format("Dumping range %x to %x\n") % ((voidptr_t) begin) % ((voidptr_t) end);
    ptr_t pos = begin;
    while (pos < end)
    {
      intptr_t off = (intptr_t)(pos - begin);
      if (off % column_size == 0)
        std::cout << "\n ";
      if (off % group_size == 0)
        std::cout << " ";
      std::cout << fmt % ((int) *pos) << " ";
      ++pos;
    }
    std::cout << "\n\n";
  }

  template <class PtrRange>
  inline void raw_dump(PtrRange& range,
                       bool hex = true,
                       size_t group_size = 4,
                       size_t column_size = 16)
  {
    raw_dump(range.begin(), range.end(), hex, group_size, column_size);
  }
}

