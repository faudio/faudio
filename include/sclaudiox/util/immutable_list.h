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
private:
    typedef ilist_iterator<T> Self;

public:
    typedef 
        typename boost::remove_const<T>::type                           
        value_type;
    typedef value_type*                 pointer;
    typedef value_type&                 reference;
    typedef std::forward_iterator_tag   iterator_category;
    typedef size_t                      size_type;
    typedef ptrdiff_t                   difference_type;

    ilist_iterator() : mNode(0) {}
    ilist_iterator(ilist_node<value_type> * node) : mNode(node) {}
    ~ilist_iterator() {}

    static Self singular_iterator()
    {
        return ilist_iterator();
    }

    reference operator*()
    {
        // throw something better if null (?)
        return mNode->data;
    }

    const pointer operator->()
    {
        // throw something better if null (?)
        return mNode->data;
    }

    Self& operator++()
    {
        inc();
        return *this;
    }
    Self operator++(int)
    {
        Self tmp = *this;
        inc();
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
    void inc()
    {
        // throw something better if null (?)
        mNode = static_cast< ilist_node<value_type>* >(mNode->next);
    }

    ilist_node<value_type> * mNode;
};





// Sequence

/**
    Persistent, immutable singly linked list.
 */
template <class T, class Alloc = std::allocator<T> >
class ilist
{
private:
    typedef ilist<T, Alloc>
        Self;
public:
    typedef 
        typename boost::remove_const<T>::type
        value_type;

    typedef 
        ilist_iterator< typename boost::remove_const<T>::type >
        iterator;
    typedef 
        ilist_iterator< typename boost::add_const<T>::type >
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
    typedef ilist_node<value_type> Node;
    typedef std::allocator< ilist_node<value_type> > NodeAlloc;
    
public:

    ilist() 
        : mNode(ilist_node_nil<T,Alloc>()) {}

    ilist(const ilist<T>& x) 
        : mNode(x.mNode) {}
        
    ~ilist()
    {
//        ilist_node_free(mNode, mNodeAlloc); // FIXME
    }

private:
    explicit ilist(Node * node) 
        : mNode(node) {}
    
public:    
    Self prepend(value_type x)
    {
        Node * node = ilist_node_cons(x, mNode, mNodeAlloc);
        return ilist(node);
    }

    void operator=(const ilist<T>& x) 
    {
        // ilist<T> y (x);
        // swap(this, y);
        mNode = x.mNode;
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

    size_type size() const
    {
        return std::distance(begin(), end());
    }

    size_type max_size()  const
    {
        return size();
    }

    size_type empty() const
    {
        return begin() == end();
    }

    void swap(const ilist<T>& y)
    {
        ilist_node<T> temp;
        temp = y.mNode;
        y.mNode = mNode;
        mNode = temp;
    }

private:
    Node * mNode;
    NodeAlloc mNodeAlloc;
};







template <class T, class Alloc>
bool operator==(const ilist<T, Alloc>& x, const ilist<T, Alloc>& y)
{
    typedef typename ilist<T,Alloc>::const_iterator const_iterator;
    const_iterator end1 = x.end();
    const_iterator end2 = y.end();

    const_iterator i1 = x.begin();
    const_iterator i2 = y.begin();
    while (i1 != end1 && i2 != end2 && *i1 == *i2) {
        ++i1;
        ++i2;
    }
    return i1 == end1 && i2 == end2;
}

template <class T, class Alloc>
bool operator!=(const ilist<T, Alloc>& x, const ilist<T, Alloc>& y)
{
    typedef typename ilist<T,Alloc>::const_iterator const_iterator;
    const_iterator end1 = x.end();
    const_iterator end2 = y.end();

    const_iterator i1 = x.begin();
    const_iterator i2 = y.begin();
    while (i1 != end1 && i2 != end2 && *i1 != *i2) {
        ++i1;
        ++i2;
    }
    return i1 != end1 || i2 != end2;
}

template <class T, class Alloc>
bool operator<(const ilist<T, Alloc>& x, const ilist<T, Alloc>& y)
{
    // FIXME
}

template <class T, class Alloc>
bool operator>(const ilist<T, Alloc>& x, const ilist<T, Alloc>& y)
{
    // FIXME
}

template <class T, class Alloc>
bool operator<=(const ilist<T, Alloc>& x, const ilist<T, Alloc>& y)
{
    // FIXME
}

template <class T, class Alloc>
bool operator>=(const ilist<T, Alloc>& x, const ilist<T, Alloc>& y)
{
    // FIXME
}

template <class T, class Alloc>
void swap(const ilist<T, Alloc>& x, const ilist<T, Alloc>& y)
{
    // FIXME
}


} // namespace doremir
} // namespace scl

#endif
