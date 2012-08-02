
#include "scl/atomic.hpp"

namespace scl
{
  namespace immutable
  {
    template <class Self>
    class reference_counted
    {
    public:
      reference_counted() : reference_count(0) {}
      ~reference_counted() {}
      Self retain();
      void release();
    private:
      atomic_int reference_count;
    };
    
    template <class Self>
    Self retain()
    {
      ++reference_count;
      return this;
    }

    template <class Self>
    void release()
    {
      if (--reference_count == 0)
        delete this;
    }
  }
}

#ifndef SCL_REFERENCE_NO_BOOST
namespace boost
{
  void intrusive_ptr_add_ref(reference_counted* p)
  {
    p.retain();
  }
  void intrusive_ptr_release(reference_counted* p)
  {
    p.release();
  }
}
#endif

