
#pragma once

#include <string>
#include <exception>

namespace scl
{
  template <class Impl>
  class exception : public std::exception
  {
  public:
    const char* what() const noexcept
    {
      auto name = static_cast<const Impl*>(this)->name;
      return name.c_str();
    }
  };

  class simple_exception : public exception<simple_exception>
  {
  public:
    simple_exception(std::string name) : name(name) {}
    std::string name;
  };

  struct bad_state : public simple_exception
  {
    bad_state() : simple_exception("bad_state") {}
  };

  struct bad_argument : public simple_exception
  {
    bad_argument() : simple_exception("bad_argument") {}
  };

  struct bad_allocation : public simple_exception
  {
    bad_allocation() : simple_exception("bad_allocation") {}
  };
}