
#pragma once

#include <initializer_list>
#include <boost/iterator/iterator_facade.hpp>
#include <scl/utility.hpp>

namespace scl
{
  template <class A>
  struct chase_node
  {
    chase_node<A>* rest;
    size_t count;
    A elem[1];
  };

// last node or nullptr
  template <class A>
  chase_node<A>* chase_node_last_node(chase_node<A>* xs)
  {
    if (!xs) return nullptr;
    while (xs->rest)
      xs = xs->rest;
    return xs;
  }

// destructive append
// future delete will handle both xs and ys
  template <class A>
  chase_node<A>* chase_node_append(chase_node<A>* xs, chase_node<A>* ys)
  {
    if (!xs) return ys;
    chase_node_last_node(xs)->rest = ys;
    return xs;
  }

  template <class Sequence>
  chase_node<typename Sequence::value_type>* chase_node_new(Sequence xs)
  {
    using size_type = typename Sequence::size_type;
    using value_type = typename Sequence::value_type;
    size_type n = scl::size(xs);
    chase_node<value_type>* node = (chase_node<value_type>*)
                                   new char[sizeof(ptr_t) + sizeof(size_t) + sizeof(value_type) * n];
    node->rest  = nullptr;
    node->count = n;
    int i = 0;
    for (value_type x : xs)
    {
      assert(i < n);
      node->elem[i++] = x;
    }
    return node;
  }

  template <class A>
  void chase_node_free(chase_node<A>* xs)
  {
    chase_node<A>* ys;
    while (xs)
    {
      ys = xs->rest;
      delete[] xs;
      xs = ys;
    }
  }

  template <class A>
  class chase_iterator
    : public boost::iterator_facade <
    chase_iterator<A>
    , A
    , boost::forward_traversal_tag >
  {
    chase_node<A>* xs;
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
    chase_iterator(chase_node<A>* xs)
      : xs(xs), i(0) {}
  };

  /**
    A chase is a series of (uniquely) linked nodes of variable length. It has an O(1)
    destructive append operation and can be moved indirectly, using the release method.

        chase<int> xs = { 1, 2, 3 };
        auto n = xs.release();      // moves elements out of xs
        chase<int> ys (n);          // moves elements into ys

    Model of Movable, Swappable, ForwardRange.

   */
  template <class A>
  class chase
  {
    chase_node<A>* ptr;
  public:
    using value_type = A;
    using this_type = chase<A>;
    using node_type = chase_node<A>;
    using iterator = chase_iterator<A>;

    chase() : ptr(nullptr) {}
    explicit chase(node_type* xs) : ptr(xs) {}
    chase(std::initializer_list<A> elems)
      : ptr(chase_node_new<std::initializer_list<A>>(elems)) {}

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
    node_type* get()
    {
      return ptr;
    }

    node_type* release()
    {
      node_type* qtr = ptr;
      ptr = nullptr;
      return qtr;
    }

    void reset(node_type* val)
    {
      chase_node_free(ptr);
      ptr = val;
    }

    /** Append ys to this and clear ys, effectively moving the contents of ys to this. */
    this_type& append(this_type& ys)
    {
      ptr = chase_node_append(ptr, ys.release());
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

namespace std
{
  template <class A>
  scl::chase_iterator<A> begin(scl::chase_node<A>* xs)
  {
    return scl::chase_iterator<A>(xs);
  }
  template <class A>
  scl::chase_iterator<A> end(scl::chase_node<A>* xs)
  {
    return scl::chase_iterator<A>();
  }
}
