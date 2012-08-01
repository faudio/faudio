
namespace scl
{
  namespace immutable
  {
    // Reference counted mixin
    template <class Self>
    class reference_counted
    {
    public:
      reference_counted() : reference_count(0) {}
      ~reference_counted() {}
      Self retain();
      void release();
    private:
      int reference_count;
    };
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

