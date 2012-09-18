
#pragma once

#include <functional>

namespace scl
{

  /*
    mapReduce :: (a -> b) -> (b -> b -> b) -> b -> [a] -> b
    mapReduce f g z []  = z
    mapReduce f g z [x] = f x
    mapReduce g f z [xs:ys] = g (mapReduce g f z xs) (mapReduce g f z ys)
   */
  OutputType
  parallel_transform_accumulate(InputRange range,
                                OutputType init,
                                UnaryOperation unary_op
                                BinaryOperation binary_op
                               )
  {
    if (begin(range) == end(range))
    {
      return init;
    }
    else if (after_begin(range) == end(range))
    {
      return unary_op(begin_value(range));
    }
    else
    {
      std::pair<InputRange, InputRange>     partitions = split(range);
      std::pair<OutputType, OutputType>     results;
      std::pair<async_handle, async_handle> handles;
      handles.first = async([partitions, results, handles]
      {
        results.first = parallel_transform_accumulate(partitions.first,
        init,
        unary_op,
        binary_op);
      });
      handles.second = async([partitions, results, handles]
      {
        results.second = parallel_transform_accumulate(partitions.second,
        init,
        unary_op,
        binary_op);
      });
      wait_for(handles);
      return binary_op(partitions.first, partitions.second);
    }
  }

}