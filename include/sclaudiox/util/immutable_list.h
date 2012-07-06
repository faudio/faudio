/*
    ScoreCleaner Audio Engine

    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_UTIL_IMMUTABLE_LIST
#define _SCLAUDIOX_UTIL_IMMUTABLE_LIST

#include <boost/type_traits.hpp>
#include "sclaudiox/core.h"

namespace doremir {
namespace scl {

using boost::add_const;
using boost::remove_const;

struct ilist_node_base
{
    ilist_node_base * next;
    int reference_count;
};



template <class T>
struct ilist_node :
    public ilist_node_base
{
    T data;
};

template <class T, class Alloc>
ilist_node<T> * ilist_node_nil()
{
    return NULL;
}

template <class T, class Alloc>
ilist_node<T> * ilist_node_cons(T x, ilist_node<T> * xs, Alloc allocator)
{
    // typedef typename Alloc::pointer   pointer;
    // typedef typename Alloc::const_reference const_reference;
    // pointer   ysp;
    // const_reference ys;
    // 
    // ysp = allocator.allocate(1); // FIXME handle bac_alloc
    // allocator.construct(ysp, &ys);

    ilist_node<T> * ys = new ilist_node<T>();
    ys->data = x;
    ys->next = xs;
    if (xs) xs->reference_count++;
    return ys;
}

template <class T, class Alloc>
void ilist_node_free(ilist_node<T> * xs, Alloc allocator)
{
    if (!xs) return;
    ilist_node_free(reinterpret_cast < ilist_node<T>* >(xs->next), allocator);
    xs->reference_count--;
    if (!xs->reference_count)
    {
        delete xs;
        // allocator.destroy(xs);
        // allocator.deallocate(xs, 1);
    }
}



template <class T>
class ilist_iterator
{
public:
    typedef          ilist_iterator<T>      this_type;
    typedef typename remove_const<T>::type  value_type;

    typedef value_type*                 pointer;
    typedef value_type&                 reference;
    typedef std::forward_iterator_tag   iterator_category;
    typedef size_t                      size_type;
    typedef ptrdiff_t                   difference_type;

    ilist_iterator() : mNode(0) {}
    ilist_iterator(ilist_node<value_type> * node) : mNode(node) {}
    ~ilist_iterator() {}

    static this_type singular_iterator()
    {
        return ilist_iterator();
    }

    reference operator*()
    {
        return mNode->data;
    }

    const pointer operator->()
    {
        return mNode->data;
    }

    this_type& operator++()
    {
        increment();
        return *this;
    }
    
    this_type operator++(int)
    {
        this_type tmp = *this;
        increment();
        return tmp;
    }

    bool operator==(const ilist_iterator<T>& y)
    {
        return mNode == y.mNode;
    }
    
    bool operator!=(const ilist_iterator<T>& y)
    {
        return mNode != y.mNode;
    }

private:
    void increment()
    {
        mNode = static_cast< ilist_node<value_type>* >(mNode->next);
    }

    ilist_node<value_type> * mNode;
};


/**
    Immutable singly linked list.
 */
template <class T, class Alloc = std::allocator<T> >
class ilist
{
public:
    typedef          ilist<T, Alloc>        this_type;
    typedef typename remove_const<T>::type  value_type;

    typedef 
        ilist_iterator< typename remove_const<T>::type >
        iterator;
    typedef 
        ilist_iterator< typename add_const<T>::type >
        const_iterator;
    
    typedef value_type&         reference;
    typedef const value_type&   const_reference;

    typedef Alloc               allocator_type;
    
    // FIXME piggyback on allocator definitions instead
    typedef value_type*         pointer;
    typedef const value_type*   const_pointer;
    typedef ptrdiff_t           difference_type;
    typedef size_t              size_type;

private:
    typedef ilist_node<value_type>  Node;
    typedef std::allocator<Node>    NodeAlloc;
    
public:
    /** Default constructor. Creates an empty list. */
    ilist() 
        : mNode(ilist_node_nil<T,Alloc>()) {}

    /** Copy constructor. Creates by appending the given value to the given list. */
    ilist(value_type x, const ilist<T>& xs)
        : mNode(ilist_node_cons(x, xs.mNode, xs.mNodeAlloc)) {}

    /** Copy constructor. Creates a list sharing the elements of the given list. */
    ilist(const ilist<T>& xs) 
        : mNode(xs.mNode) {}

    /** Destroy the given list. */
    ~ilist()
    {
       ilist_node_free(mNode, mNodeAlloc);
    }
    
public:    

    /** Updates this list to have the contents of the given list.
        \note
            O(1) complexity 
      */
    void operator=(const ilist<T>& x) 
    {
        ilist<T> y (x);
        swap(y);
    }
    
    /** Swaps contents with the given list.
        \note
            O(1) complexity 
      */
    void swap(ilist<T>& y)
    {
        Node * n = y.mNode;
        y.mNode  = mNode;
        mNode    = n;
    }
    
    iterator begin()
    {
        return iterator(mNode);
    }

    iterator end()
    {
        return iterator::singular_iterator();
    }

    const_iterator begin() const
    {
        return const_iterator(mNode);
    }

    const_iterator end() const
    {
        return const_iterator::singular_iterator();
    }

    /** Returns the number of elements in this list.
        \note
            O(n) complexity 
      */
    size_type size() const
    {
        return std::distance(begin(), end());
    }

    /** Returns the maximal number of elements in this list.
        \note
            O(n) complexity 
      */
    size_type max_size()  const
    {
        return size();
    }

    /** Returns whether this list is emtpy or not.
        \note
            O(1) complexity 
      */
    size_type empty() const
    {
        return begin() == end();
    }

    /** Returns a list with the given value prepended to this list.
        \note
            O(1) complexity 
      */
    this_type add_before(value_type x)
    {
        return ilist(x, *this);
    }

    /** Returns a list with the given value appended to this list.
        \note
            O(1) complexity 
      */
    this_type add_after(value_type x)
    {
        // FIXME
    }
                   
    template <int N>
    this_type drop()
    {
        // FIXME
    }

    template <int N>
    this_type take()
    {
        // FIXME
    }

    template <int M, int N>
    this_type slice()
    {
        // FIXME
    }

    this_type append(this_type y)
    {
        // FIXME
    }
    

private:
    Node      * mNode;
    NodeAlloc   mNodeAlloc;
};


template <class T, class Alloc>
bool operator== (const ilist<T, Alloc>& x, const ilist<T, Alloc>& y)
{
    return std::equal(x.begin(), x.end(), y.begin());
}

template <class T, class Alloc>
bool operator< (const ilist<T, Alloc>& x, const ilist<T, Alloc>& y)
{
    return std::lexicographical_compare(x.begin(), x.end(), y.begin(), y.end());
}

template <class T, class Alloc>
bool operator!= (const ilist<T, Alloc>& x, const ilist<T, Alloc>& y)
{
    return !(x == y);
}

template <class T, class Alloc>
bool operator> (const ilist<T, Alloc>& x, const ilist<T, Alloc>& y)
{
    return y < x;
}

template <class T, class Alloc>
bool operator<= (const ilist<T, Alloc>& x, const ilist<T, Alloc>& y)
{
    return !(y < x);
}

template <class T, class Alloc>
bool operator>= (const ilist<T, Alloc>& x, const ilist<T, Alloc>& y)
{
    return !(x < y);
}

template <class T, class Alloc>
void swap(const ilist<T, Alloc>& x, const ilist<T, Alloc>& y)
{
    x.swap(y);
}



} // namespace doremir
} // namespace scl

#endif
