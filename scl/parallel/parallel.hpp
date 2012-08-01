
namespace scl
{
  namespace parallel
  {
    template <
    class ParallelInputIterator
    >
    ParallelInputIterator divide(ParallelInputIterator begin, ParallelInputIterator end)
    {
    }

    template <
    class ParallelInputRange
    >
    std::pair<ParallelInputRange, ParallelInputRange> divide(ParallelInputRange range)
    {
    }

    template <
    class ParallelInputIterator,
          class ParallelInputIteratorIterator,
          int N
          >
    ParallelInputIteratorIterator
    divide_many(ParallelInputIterator begin, ParallelInputIterator end)
    {
    }

    template <
    class ParallelInputRange,
          class ParallelInputRangeRange,
          int N
          >
    ParallelInputRangeRange
    divide_many(ParallelInputRange range)
    {
    }
  }
}

