
#pragma once

#include <functional>
#include <scl/parallel/algorithm/parallel_transform_accumulate.hpp>

namespace scl
{
  template<class InputIterator, class T>
  T parallel_accumulate(InputIterator first, InputIterator last, T init)
  {
    return parallel_accumulate(first, last, init, std::plus<T>);
  }

  template<class InputIterator, class T, class BinaryOperation>

  T parallel_accumulate(InputIterator first, InputIterator last, T init,
                        BinaryOperation op)
  {
    parallel_transform_accumulate(first, last)
  }

}