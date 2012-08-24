
#pragma once

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
    
    intptr_t begin()
    {
      return (intptr_t) mBuffer.get();
    }
    
    intptr_t end()
    {
      return begin() + size();
    }
    
    size_t size()
    {
      return mSize;
    }
  };

  inline intptr_t raw_copy(intptr_t begin, intptr_t end, intptr_t out)
  {
    return (intptr_t) std::copy((char*)begin, (char*)end, (char*)out);
  }
}