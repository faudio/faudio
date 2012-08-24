
#pragma once

#include <memory> // for unique_ptr

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

  using ptr_t = intptr_t;

  /**
      A moveable, non-copyable byte buffer.
   */
  class raw_buffer
  {
    std::unique_ptr<char> mBuffer;
    size_t mSize;

  public:
    raw_buffer() : mSize(0) {}

    void resize(size_t size)
    {
      if (size > 0)
        mBuffer.reset(new char[size]);
      else
        mBuffer.reset();
      mSize = size;
    }

    void clear()
    {
      resize(0);
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
    return (ptr_t) std::copy((char*)begin, (char*)end, (char*)out);
  }
}