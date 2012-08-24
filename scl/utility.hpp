
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
  inline intptr_t raw_copy(intptr_t begin, intptr_t end, intptr_t out)
  {
    return (intptr_t) std::copy((char*)begin, (char*)end, (char*)out);
  }
}