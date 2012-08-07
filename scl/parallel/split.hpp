
# pragma once

#include <utility>
#include <vector>

namespace scl
{
  template <class T> 
  std::pair<std::vector<T>, std::vector<T>> 
  split(std::vector<T> xs)
  {                        
    auto nx = xs.size();
    auto ny = nx / 2 + nx % 2;
    auto nz = nx / 2;                    
    std::vector<T> ys (ny);
    std::vector<T> zs (nz);
    
    std::copy(xs.begin(),      xs.begin() + ny, ys.begin());
    std::copy(xs.begin() + ny, xs.end(),        zs.begin());
    
    return std::make_pair(ys, zs);
  }

  template <class T> 
  std::pair<std::vector<T>, std::vector<T>> 
  split_equal(std::vector<T> xs)
  {
    auto nx = xs.size();
    auto ny = nx / 2;
    auto nz = nx / 2;                    
    std::vector<T> ys (ny);
    std::vector<T> zs (nz);
    
    std::copy(xs.begin(),      xs.begin() + ny, ys.begin());
    std::copy(xs.begin() + ny, xs.end(),        zs.begin());
    
    return std::make_pair(ys, zs);
  }
}