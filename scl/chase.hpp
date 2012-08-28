
#pragma once

#include <initializer_list>
#include <boost/iterator/iterator_facade.hpp>
#include <scl/utility.hpp>

namespace scl
{

  /** @cond internal */

  template <class A>
  struct track
  {
    track<A>* rest;
    size_t count;
    A elem[1];
  };

  template <class Sequence>
  track<typename Sequence::value_type>* track_create(Sequence xs)
  {
    using size_type = typename Sequence::size_type;
    using value_type = typename Sequence::value_type;
    size_type n = scl::size(xs);
    
    track<value_type>* ptr = (track<value_type>*)
      new char[sizeof(ptr_t) + sizeof(size_t) + sizeof(value_type) * n];
    ptr->rest  = nullptr;
    ptr->count = n;
    
    int i = 0;
    for (value_type x : xs)
    {
      assert(i < n);
      ptr->elem[i++] = x;
    }
    return ptr;
  }

  template <class A>
  void track_destroy(track<A>* xs)
  {
    track<A>* ys;
    while (xs)
    {
      ys = xs->rest;
      delete[] xs;
      xs = ys;
    }
  }

  /*
    Returns the last valid track.
   */
  template <class A>
  track<A>* track_last(track<A>* xs)
  {
    if (!xs) return nullptr;
    while (xs->rest)
      xs = xs->rest;
    return xs;
  }

  /*
    Destructive append
    Future delete will handle both xs and ys
  */
  template <class A>
  track<A>* track_append(track<A>* xs, track<A>* ys)
  {
    if (!xs) return ys;
    track_last(xs)->rest = ys;
    return xs;
  }


  template <class A>
  class chase_iterator
    : public boost::iterator_facade <
    chase_iterator<A>
    , A
    , boost::forward_traversal_tag >
  {
    track<A>* xs;
    int i;

    template <class B>
    bool equal(chase_iterator<B> const& other) const
    {
      return xs == other.xs && i == other.i;
    }

    void increment()
    {
      if (xs)
      {
        if ((i + 1) < xs->count)
        {
          ++i;
        }
        else
        {
          i = 0;
          xs = xs->rest;
        }
      }
    }

    A& dereference() const
    {
      return xs->elem[i]; // TODO
    }

    friend class boost::iterator_core_access;
  public:
    chase_iterator()
      : xs(nullptr), i(0) {}
    chase_iterator(track<A>* xs)
      : xs(xs), i(0) {}
  };
  
  /** @endcond internal */

  /**
    A *chase* is a series of *tracks*, that is, uniquely linked arrays of variable length. It has an
    efficient destructive append operation and can be moved indirectly, by releasing the track pointer.

        chase<int> xs = { 1, 2, 3 };
        auto n = xs.release();      // moves elements out of xs
        chase<int> ys (n);          // moves elements into ys

    Model of Movable, Swappable, ForwardRange.

   */
  template <class A>
  class chase
  {
    track<A>* ptr;
  public:
    using value_type = A;
    using this_type  = chase<A>;
    using track_type = track<A>;
    using track_pointer = track<A>*;
    using iterator = chase_iterator<A>;

    chase() : ptr(nullptr) {}
    explicit chase(track_pointer xs) : ptr(xs) {}
    chase(std::initializer_list<A> elems)
      : ptr(track_create<std::initializer_list<A>>(elems)) {}

    chase(const chase&) = delete;
    chase(chase && other) : ptr(other.release()) {}

    ~chase()
    {
      reset(nullptr);
    }

    void swap(this_type& other)
    {
      std::swap(ptr, other.ptr);
    }

    SCL_STANDARD_ASSIGN(this_type);

    /** Unsafe */
    track_pointer get()
    {
      return ptr;
    }

    track_pointer release()
    {
      track_pointer qtr = ptr;
      ptr = nullptr;
      return qtr;
    }

    void reset(track_pointer val)
    {
      track_destroy(ptr);
      ptr = val;
    }

    /** Append ys to this and clear ys, effectively moving the contents of ys to this. */
    this_type& append(this_type& ys)
    {
      ptr = track_append(ptr, ys.release());
      return *this;
    }

    iterator begin()
    {
      return iterator(ptr);
    }

    iterator end()
    {
      return iterator(nullptr);
    }

    operator bool()
    {
      return ptr != nullptr;
    }
  };
}

/** @cond internal */

namespace std
{
  template <class A>
  scl::chase_iterator<A> begin(scl::track<A>* xs)
  {
    return scl::chase_iterator<A>(xs);
  }
  template <class A>
  scl::chase_iterator<A> end(scl::track<A>* xs)
  {
    return scl::chase_iterator<A>();
  }
}

/** @endcond internal */
