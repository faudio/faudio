/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_UTIL_MISC
#define _SCLAUDIOX_UTIL_MISC

namespace doremir {
namespace scl {

template <class A, class B>
class Function : public std::unary_function<A,B> {};

template <class A, class B, class C>
class Function2 : public std::binary_function<A,B,C> {};

/**
    A function object comparing the values of two pointers.

    Example:
    
        int x = 1, y = 2;
        PointerNotLess pointerNotLess;
        assert(pointerLess(&x, &y));
 */
template <class T>
class PointerLess : public std::binary_function<T, T, bool> 
{    
public:
    bool operator()(const T& left, const T& right) const 
    {
        return (*left) < (*right);
    }
};

/**
    A function object comparing the values of two pointers.

    Example:
    
        int x = 2, y = 1;
        PointerNotLess pointerNotLess;
        assert(pointerNotLess(&x, &y));
 */
template <class T>
class PointerNotLess : public std::binary_function<T, T, bool> 
{    
public:
    bool operator()(const T& left, const T& right) const 
    {
        return !( (*left) < (*right) );
    }
};


// Unions

// template <class A, class B>
// class SCLAUDIO_API Union
// {
// public:
//     enum Index { Left, Right };
//     class WrongIndex : public std::exception {};
// 
//     Union(A value) : mIndex (Left), mVal (value) {}
//     Union(B value) : mIndex (Right) , mVal (value) {}        
// 
//     Index index() { return mIndex; }    
//     bool isLeft() { return mIndex = Left; }
//     bool isRight() { return mIndex = Right; }
//     
//     A forceLeft() 
//     { 
//         if (mIndex != Left) throw WrongIndex();
//         return mVal.leftVal; 
//     }
//     
//     A forceRight() 
//     { 
//         if (mIndex != Right) throw WrongIndex();
//         return mVal.rightVal; 
//     }
//     
// private:
//     bool mIndex;
//     union { A leftVal; 
//             B rightVal; } mVal;
// };  
// 
// template <class A, class B, class C>
// struct SCLAUDIO_API Union3
// {
//     typedef Union< A, Union<B,C> > type;
// }; 
// 
// template <class A, class B, class C, class D>
// struct SCLAUDIO_API Union4
// {
//     typedef Union< A, Union<B, Union <C,D> > > type;
// }; 
// 
// 
// // Maybe
// 
// class Empty {};
// 
// template <class T>
// class SCLAUDIO_API Maybe
// {                 
// public:                       
//     class NoValue : public std::exception {};
//     
//     Maybe() : mHasValue (false) {}        
//     Maybe(T value) : mHasValue (true), mVal (value) {}
// 
//     bool hasValue() { return mHasValue; }
// 
//     T forceValue()
//     {
//         if (!mHasValue) throw NoValue();
//         return mVal.val;
//     }
// 
//     template <class U>
//     U maybe(U empty, U (*nonEmpty)(T value))
//     {
//         if (!mHasValue)
//             return empty;
//         else
//             return nonEmpty(mVal);
//     }   
//     
//     static std::list<T> concat(std::list< Maybe<T> > maybes)
//     {
//         // FIXME
//         // throw Unimplemented();
//     }
//     
// private:
//     bool mHasValue;
//     union { Empty _; 
//             T val; } mVal;
// };   


/**
    @namespace doremir::scl::math

    This namespace contains miscellaneous numerical functions and operators.
 */
namespace math
{
    template <class T>
    T negate(T x)
    {
        return x * -1;
    }          
    
    template <class T>
    T maximum(const T a, const T b)
    {
        if (a > b) return a; else return b;
    }       
    
    template <class T>
    T minimum(const T a, const T b)
    {
        if (a < b) return a; else return b;
    }
}



/**
    @namespace doremir::scl::list

    This namespace contains utility functions for list manipulation.
 */
namespace list
{         
    using std::list;

    /** @name Construct and destruct */
    
    /**@{*/
        
    /**
        Creates an empty list.
        \note            
            O(1) complexity
     */
    template <class T>
    list<T> create()
    {
        list<T> lst;
        return lst;
    }           
    
    /**
        Creates a list containing the given elements.
        \note            
            O(*n*) complexity
     */
    template <class T>
    list<T> create(T a)
    {
        list<T> lst;
        lst.push_back(a);
        return lst;
    }

    /**
        Creates a list containing the given elements.
        \note            
            O(*n*) complexity
     */
    template <class T>
    list<T> create(T a, T b)
    {
        list<T> lst;
        lst.push_back(a);
        lst.push_back(b);
        return lst;
    }

    /**
        Creates a list containing the given elements.
        \note            
            O(*n*) complexity
     */
    template <class T>
    list<T> create(T a, T b, T c)
    {
        list<T> lst;
        lst.push_back(a);
        lst.push_back(b);
        lst.push_back(c);
        return lst;
    }

    /**
        Creates a list containing the given elements.
        \note            
            O(*n*) complexity
     */
    template <class T>
    list<T> create(T a, T b, T c, T d)
    {
        list<T> lst;
        lst.push_back(a);
        lst.push_back(b);
        lst.push_back(c);
        lst.push_back(d);
        return lst;
    }

    /**
        Creates a list containing the given elements.
        \note            
            O(*n*) complexity
     */
    template <class T>
    list<T> create(T a, T b, T c, T d, T e)
    {
        list<T> lst;
        lst.push_back(a);
        lst.push_back(b);
        lst.push_back(c);
        lst.push_back(d);
        lst.push_back(e);
        return lst;
    }

    /**
        Creates a list containing the given elements.
        \note            
            O(*n*) complexity
     */
    template <class T>
    list<T> create(T a, T b, T c, T d, T e, T f)
    {
        list<T> lst;
        lst.push_back(a);
        lst.push_back(b);
        lst.push_back(c);
        lst.push_back(d);
        lst.push_back(e);
        lst.push_back(f);
        return lst;
    }                 
    
    
    /**
        Destructs the given list, storing its elements in the given reference.
        \note            
            O(*n*) complexity
     */
    template <class T>
    void destroy(list<T> lst, T& a)
    {
        typename list<T>::iterator elem = lst.begin();
        typename list<T>::iterator stop = lst.end();
        a = *elem++;
    }

    /**
        Destructs the given list, storing its elements in the given set of references.
        \note            
            O(*n*) complexity
     */
    template <class T>
    void destroy(list<T> lst, T& a, T& b)
    {
        typename list<T>::iterator elem = lst.begin();
        typename list<T>::iterator stop = lst.end();
        a = *elem++; if (elem == stop) return;
        b = *elem++;
    }


    /**
        Destructs the given list, storing its elements in the given set of references.
        \note            
            O(*n*) complexity
     */
    template <class T>
    void destroy(list<T> lst, T& a, T& b, T& c)
    {
        typename list<T>::iterator elem = lst.begin();
        typename list<T>::iterator stop = lst.end();
        a = *elem++; if (elem == stop) return;
        b = *elem++; if (elem == stop) return;
        c = *elem++;
    }

    /**
        Destructs the given list, storing its elements in the given set of references.
        \note            
            O(*n*) complexity
     */
    template <class T>
    void destroy(list<T> lst, T& a, T& b, T& c, T& d)
    {
        typename list<T>::iterator elem = lst.begin();
        typename list<T>::iterator stop = lst.end();
        a = *elem++; if (elem == stop) return;
        b = *elem++; if (elem == stop) return;
        c = *elem++; if (elem == stop) return;
        d = *elem++;
    }

    /**
        Destructs the given list, storing its elements in the given set of references.
        \note            
            O(*n*) complexity
     */
    template <class T>
    void destroy(list<T> lst, T& a, T& b, T& c, T& d, T& e)
    {
        typename list<T>::iterator elem = lst.begin();
        typename list<T>::iterator stop = lst.end();
        a = *elem++; if (elem == stop) return;
        b = *elem++; if (elem == stop) return;
        c = *elem++; if (elem == stop) return;
        d = *elem++; if (elem == stop) return;
        e = *elem++;
    }

    /**
        Destructs the given list, storing its elements in the given set of references.
        \note            
            O(*n*) complexity
     */
    template <class T>
    void destroy(list<T> lst, T& a, T& b, T& c, T& d, T& e, T& f)
    {
        typename list<T>::iterator elem = lst.begin();
        typename list<T>::iterator stop = lst.end();
        a = *elem++; if (elem == stop) return;
        b = *elem++; if (elem == stop) return;
        c = *elem++; if (elem == stop) return;
        d = *elem++; if (elem == stop) return;
        e = *elem++; if (elem == stop) return;
        f = *elem++;
    }
    
    /**
        @}
        
        \name Indexing
        @{
    */ 

    template <class T>
    T index(list<T> lst, int index)
    {
        typename list<T>::iterator it = lst.begin();
        for(int i = 0; i < index; ++i, ++it);
        return *it;
    }

    template <class T>
    void access(list<T> lst, int index, T& v)
    {
        typename list<T>::iterator it = lst.begin();
        for(int i = 0; i < index; ++i, ++it);
        v = *it;
    }

    
    /**
        @}
        
        \name Predicates
        @{
    */ 
    
    template <class T>
    bool isEmpty(list<T> s)
    {
        return s.begin() == s.end();
    }

    template <class T>
    bool isNonEmpty(list<T> s)
    {
        return s.begin() != s.end();
    }
    
    
    /**
        @}
        
        \name List of references
        @{
    */ 
        
    template <typename T>
    list<T> dereferenceAll(list<T*> input)
    {
        list<T> output (input.size());

        typename list<T*>::iterator a = input.begin();
        typename list<T> ::iterator b = output.begin();
        for (; a != input.end(); ++a, ++b) 
            *b = **a;

        return output;
    }

    template <typename T>
    list<T*> copyAll(list<T> input)
    {
        list<T*> output (input.size());

        typename list<T>::iterator  a = input.begin();
        typename list<T*>::iterator b = output.begin();
        for (; a != input.end(); ++a, ++b)
            *b = new T(*a);

        return output;
    }     
    
    /**
        @}
        
        \name Algorithms
        @{
    */ 

    template <class T, class U, class F>
    list<U> transform(F f, list<T> s)
    {
        list<U> t (s.size());
        std::transform(s.begin(), s.end(), t.begin(), f);
        return t;
    }
    template <class T, class U>
    list<U> transform(U (*f)(T), list<T> s)
    {
        list<U> t (s.size());
        std::transform(s.begin(), s.end(), t.begin(), f);
        return t;
    }

    template <class T, class U>
    list<U> foldLeft(U (*f)(U x, T y), list<T> xs)
    {                    
        // FIXME
        // throw Unimplemented();
    }
    
    template <class T, class U>
    list<U> foldRight(U (*f)(T x, U y), list<T> xs)
    {                    
        // FIXME
        // throw Unimplemented();
    }

    template <class T, class U>
    list<U> foldMap(list<U> (*f)(T x), list<T> xs)
    {                    
        // FIXME
        // throw Unimplemented();
    }
        
    template <class T>
    list<T> replace(list<T> s, T x, T y)
    {
        list<T> t (s.size());
        std::replace_copy(s.begin(), s.end(), t.begin(), x, y);
        return t;
    }

    template <class T, class P>
    list<T> replaceIf(list<T> s, P p, T y)
    {
        list<T> t (s.size());
        std::replace_copy_if(s.begin(), s.end(), t.begin(), p, y);
        return t;
    }

    template <class T, class P>
    list<T> remove(list<T> s, P p, T y)
    {
    }

    template <class T, class P>
    list<T> unique(list<T> s, P p, T y)
    {
    }

    template <class T, class P>
    list<T> reverse(list<T> s, P p, T y)
    {
    }

    template <class T, class P>
    list<T> rotate(list<T> s, P p, T y)
    {
    }

    template <class T, class P>
    list<T> partialSort(list<T> s, P p, T y)
    {
    }

    template <class T, class P>
    list<T> merge(list<T> s, P p, T y)
    {
    }

    template <class T, class P>
    list<T> intersection(list<T> s, P p, T y)
    {
    }

    template <class T, class P>
    list<T> difference(list<T> s, P p, T y)
    {
    }       
    
    /**@}*/ 

    template <class T>
    list<T> fromRange(T begin, T end)
    {
        list<T> lst;
        do lst.push_back(begin); while (++begin < end);
        return lst;
    }
    
    namespace predicate
    {
        
    }    
}            

namespace thread
{
    class Thread;
    
    void yield();
    void sleep(long millis);
    Thread* caller();
    bool isCaller(Thread *thread);
    void interruptAndJoin(Thread *thread);
}   

} // namespace
} // namespace

#endif
