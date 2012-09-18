
#pragma once

#include <memory>
#include <scl/concept.hpp>

namespace scl
{
  namespace audio
  {
    /**
      The abortable concept describes types with a one-time abort action.
      The abortion of a task is not reversible.

      ### Synopsis

          concept Abortable<typename X>
          {
            void X::abort();
            bool X::is_aborted();
          }

      ### Semantics
        - `x.abort()`
          - Precondition: None
          - Postcondition: The state of `x` is aborted.
        - `x.is_aborted()`
          - Whether `x` is aborted or not.

      ### Models
        - future
    */
    template <class X>
    struct Abortable
    {
      BOOST_CONCEPT_USAGE(Abortable)
      {
        x.abort();
        bool state = x.is_aborted();
      }
    private:
      X x;
    };

    class dynamic_abortable
    {
    public:
      virtual ~dynamic_abortable() {}
      virtual void abort() = 0;
      virtual bool is_aborted() = 0;
    };

    template<class T>
    class dynamic_abortable_wrapper : public dynamic_abortable
    {
    public:
      dynamic_abortable_wrapper(const T& x)
        : x(x) {}
      ~dynamic_abortable_wrapper() {}
      void abort()
      {
        x.abort();
      }
      bool is_aborted()
      {
        return x.is_aborted();
      }
    private:
      T x;
    };

    class any_abortable
    {
      std::shared_ptr<dynamic_abortable> x;
    public:
      template <class T>
      explicit any_abortable(T x)
        : x(new dynamic_abortable_wrapper<T>(x)) {}
      any_abortable(const any_abortable& other) : x(other.x) {}
      any_abortable(any_abortable && other) : x(std::move(other.x)) {}
      ~any_abortable() {}
      void operator= (const any_abortable& other)
      {
        any_abortable b = other;
        return b.swap(*this);
      }
      void operator= (any_abortable && other)
      {
        any_abortable b = std::move(other);
        return b.swap(*this);
      }
      void swap(any_abortable& other)
      {
        std::swap(this->x, other.x);
      }

      void abort()
      {
        x->abort();
      }
      bool is_aborted()
      {
        return x->is_aborted();
      }
    };
  }
}

