/*
    ScoreCleaner Audio Engine

    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_UTIL_DIVIDE
#define _SCLAUDIOX_UTIL_DIVIDE

namespace doremir {
namespace scl {



/** Divides the given range.
 */
template <
    class ParallelInputIterator
>
ParallelInputIterator divide(ParallelInputIterator begin, ParallelInputIterator end)
{
}

/** Divides the given range.
 */
template <
    class ParallelInputRange
>
std::pair<ParallelInputRange,ParallelInputRange> divide(ParallelInputRange range)
{
}



/** Divides the given range.
 */
template <
    class ParallelInputIterator, 
    class ParallelInputIteratorIterator,
    int N
>
ParallelInputIteratorIterator 
divide_many(ParallelInputIterator begin, ParallelInputIterator end)
{
}

/** Divides the given range.
 */
template <
    class ParallelInputRange, 
    class ParallelInputRangeRange, 
    int N
>
ParallelInputRangeRange 
divide_many(ParallelInputRange range)
{
}




} // namespace doremir
} // namespace scl

#endif
