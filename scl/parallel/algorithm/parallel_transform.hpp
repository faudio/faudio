
#pragma once

#include <functional>
#include <scl/parallel/algorithm/parallel_transform_accumulate.hpp>

template< class InputIterator, class OutputIterator, class UnaryOperation >

OutputIterator transform(InputIterator first1,
                         InputIterator last1,
                         OutputIterator d_first,
                         UnaryOperation unary_op)
{
}
