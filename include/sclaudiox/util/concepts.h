/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

/**
    \file       util/concepts.h
  */

#ifndef _SCLAUDIOX_UTIL_CONCEPTS
#define _SCLAUDIOX_UTIL_CONCEPTS

#include <boost/concept_check.hpp>

#define SCL_CONCEPT_ASSERT   BOOST_CONCEPT_ASSERT
#define SCL_CONCEPT_REQUIRES BOOST_CONCEPT_REQUIRES
#define SCL_CONCEPT_USAGE    BOOST_CONCEPT_USAGE

namespace doremir {
namespace scl {

using boost::Integer;
using boost::SignedInteger;
using boost::UnsignedInteger;
using boost::Convertible;
using boost::Assignable;
using boost::DefaultConstructible;
using boost::CopyConstructible;
using boost::EqualityComparable;
using boost::LessThanComparable;
using boost::Comparable;

using boost::InputIterator;
using boost::OutputIterator;
using boost::ForwardIterator;
using boost::Mutable_ForwardIterator;
using boost::BidirectionalIterator;
using boost::Mutable_BidirectionalIterator;
using boost::RandomAccessIterator;
using boost::Mutable_RandomAccessIterator;
    
using boost::Generator;
using boost::UnaryFunction;
using boost::BinaryFunction;
using boost::UnaryPredicate;
using boost::BinaryPredicate;
using boost::Const_BinaryPredicate;
using boost::AdaptableGenerator;
using boost::AdaptableUnaryFunction;
using boost::AdaptableBinaryFunction;
using boost::AdaptablePredicate;
using boost::AdaptableBinaryPredicate;
    
using boost::Container;
using boost::Mutable_Container;
using boost::ForwardContainer;
using boost::Mutable_ForwardContainer;
using boost::ReversibleContainer;
using boost::Mutable_ReversibleContainer;
using boost::RandomAccessContainer;
using boost::Mutable_RandomAccessContainer;
using boost::Sequence;
using boost::FrontInsertionSequence;
using boost::BackInsertionSequence;
using boost::AssociativeContainer;
using boost::UniqueAssociativeContainer;
using boost::MultipleAssociativeContainer;
using boost::SimpleAssociativeContainer;
using boost::PairAssociativeContainer;
using boost::SortedAssociativeContainer;

} // namespace
} // namespace

#endif
