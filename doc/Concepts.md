
Concepts
========

The following concepts are used in the audio engine. They can be checked using the Boost concept check library.

### Default <*T*>

    T()

### Copyable <*T*>

    T(const T&)

### Assignable <*T*>

    void operator=(constT&)

### EqualityComparable <*T*>

    T == U

### Ordered <*T*>, EqualityComparable<*T*>
    
    T < U
    T > U

### Semigroup <*T*>
    
    T + T, + is associative

### Monoid <*T*> : Semigroup<*T*>, Default<*T*>
    
### Range <*T*>

    T::iterator begin()
    T::iterator end()

### Writeable <*T*>
                     
    std::ostream << T

### Readable <*T*>

    std::istream >> T


## Standard

### InputIterator
### OutputIterator
### ForwardIterator
### BidirectionalIterator
### RandomAccessIteraetor


## Boost 

    #include "boost/concept_check.hpp"

    template <class T>
    struct Integer; // Is T a built-in integer type?

    template <class T>
    struct SignedInteger; // Is T a built-in signed integer type?

    template <class T>
    struct UnsignedInteger; // Is T a built-in unsigned integer type?

    template <class X, class Y>
    struct Convertible; // Is X convertible to Y?

    template <class T>
    struct Assignable; // Standard ref 23.1

    template <class T>
    struct SGIAssignable;

    template <class T>
    struct DefaultConstructible;

    template <class T> 
    struct CopyConstructible; // Standard ref 20.1.3

    template <class T> 
    struct EqualityComparable; // Standard ref 20.1.1

    template <class T>
    struct LessThanComparable; // Standard ref 20.1.2

    template <class T>
    struct Comparable; // The SGI STL LessThanComparable concept

### Iterators:

    template <class Iter>
    struct InputIterator; // Standard ref 24.1.1 Table 72

    template <class Iter, class T> 
    struct OutputIterator; // Standard ref 24.1.2 Table 73

    template <class Iter> 
    struct ForwardIterator; // Standard ref 24.1.3 Table 74

    template <class Iter> 
    struct Mutable_ForwardIterator;

    template <class Iter> 
    struct BidirectionalIterator; // Standard ref 24.1.4 Table 75

    template <class Iter> 
    struct Mutable_BidirectionalIterator;

    template <class Iter> 
    struct RandomAccessIterator; // Standard ref 24.1.5 Table 76

    template <class Iter> 
    struct Mutable_RandomAccessIterator;
    
### Functions:

    #include "boost/concept_check.hpp"

    template <class Func, class Return>
    struct Generator;

    template <class Func, class Return, class Arg>
    struct UnaryFunction;

    template <class Func, class Return, class First, class Second>
    struct BinaryFunction;

    template <class Func, class Arg>
    struct UnaryPredicate;

    template <class Func, class First, class Second>
    struct BinaryPredicate;

    template <class Func, class First, class Second>
    struct Const_BinaryPredicate;

    template <class Func, class Return>
    struct AdaptableGenerator;

    template <class Func, class Return, class Arg>
    struct AdaptableUnaryFunction;

    template <class Func, class First, class Second>
    struct AdaptableBinaryFunction;

    template <class Func, class Arg>
    struct AdaptablePredicate;

    template <class Func, class First, class Second>
    struct AdaptableBinaryPredicate;
    
### Containers:

    #include "boost/concept_check.hpp"

    template <class C>
    struct Container; // Standard ref 23.1 Table 65

    template <class C>
    struct Mutable_Container;

    template <class C>
    struct ForwardContainer;

    template <class C>
    struct Mutable_ForwardContainer;

    template <class C>
    struct ReversibleContainer; // Standard ref 23.1 Table 66

    template <class C>
    struct Mutable_ReversibleContainer;

    template <class C>
    struct RandomAccessContainer;

    template <class C>
    struct Mutable_RandomAccessContainer;

    template <class C>
    struct Sequence; // Standard ref 23.1.1

    template <class C>
    struct FrontInsertionSequence;

    template <class C>
    struct BackInsertionSequence;

    template <class C>
    struct AssociativeContainer; // Standard ref 23.1.2 Table 69

    template <class C>
    struct UniqueAssociativeContainer;

    template <class C>
    struct MultipleAssociativeContainer;

    template <class C>
    struct SimpleAssociativeContainer;

    template <class C>
    struct PairAssociativeContainer;

    template <class C>
    struct SortedAssociativeContainer;

