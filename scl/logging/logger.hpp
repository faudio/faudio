
#pragma once

#include <string>
#include <boost/format.hpp>

namespace scl
{  
  using boost::format;
  using boost::basic_format;
  using u16format = basic_format<char16_t>;
  using u32format = basic_format<char32_t>;
  
  
  template <class Policy>
  class logger
  {
    using policy_type = Policy;
  public:
    void log(std::string);
  };
}

