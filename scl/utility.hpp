
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
   
