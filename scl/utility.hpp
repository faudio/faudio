
#pragma once

#include <memory> // for unique_ptr
#include <boost/format.hpp>

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

  using ptr_t = unsigned char* ;

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
    ptr_t pos = begin;
    boost::format fmt;
    if (hex)
      fmt = boost::format("%02x");
    else
      fmt = boost::format("%04d");
    std::cout << boost::format("Dumping range %x to %x\n") % ((void*) begin) % ((void*) end);
    while (pos < end)
    {
      if (((intptr_t)(pos - begin)) % column_size == 0)
        std::cout << "\n ";
      if (((intptr_t)(pos - begin)) % group_size == 0)
        std::cout << " ";
      std::cout << fmt % ((int) *pos) << " ";
      pos++;
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

  template <class Sized>
  inline typename Sized::size_type size(Sized x)
  {
    return x.size();
  }

}