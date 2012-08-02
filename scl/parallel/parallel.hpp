
namespace scl
{
  namespace parallel
  {                        
    // concept ParallelForwardIterator
    // concept ParallelForwardRange

    
    template <class InputRange> 
    InputRange::iterator middle(InputRange range)
    {             
      auto _begin = begin(range)
      auto _end   = end(range)
      return _begin + distance(_begin, _end) / 2;
    }


    // Main interface
    
    template <class InputRange>
    std::pair<InputRange, InputRange> split(InputRange range)
    {
      begin  = begin(range)
      middle = middle(range)
      end    = end(range)
      return splitted_range(begin, middle, end);
    }
  }
}

