
#pragma once

#include <memory>
#include <scl/concept.hpp>
#include <scl/exception.hpp>
#include <scl/utility.hpp>
#include <scl/audio/concept/abortable.hpp>

namespace scl
{
  /**
    Synopsis:

        concept Startable<typename X> : Abortable<X>
        {
          void X::start();
          void X::stop();
          bool X::is_running();
        }

    Semantics:
      - `x.start()`
        - Precondition: The state of `x` is not aborted
        - Postcondition: The state of `x` is running.
      - `x.stop()`
        - Precondition: The state of `x` is not aborted
        - Postcondition: The state of `x` is not running.
      - `x.is_running()`
        - Whether `x` is running or not.

    Invariants:
      - `x.is_aborted()` implies `!x.is_running()`.

    Models:
      sndfile_stream
  */
  template <class X>
  struct Startable : Abortable<X>
  {
    BOOST_CONCEPT_USAGE(Startable)
    {
      x.start();
      x.stop();
      bool state = x.is_running();
    }
  private:
    X x;
  };

  class dynamic_startable
  {
  public:
    virtual ~dynamic_startable() {}
    virtual void abort() = 0;
    virtual void start() = 0;
    virtual void stop() = 0;
    virtual bool is_running() = 0;
    virtual bool is_aborted() = 0;
  };

  template<class T>
  class dynamic_startable_wrapper : public dynamic_startable
  {
    T x;
  public:
    dynamic_startable_wrapper(const T& x)
      : x(x) {}
    ~dynamic_startable_wrapper() {}
    void start() { x.start(); }
    void stop() { x.stop(); }
    void abort() { x.abort(); }
    bool is_running() { return x.is_running(); }
    bool is_aborted() { return x.is_aborted(); }
  };
  
  class any_startable
  {
    std::shared_ptr<dynamic_startable> x;
    inline void check() { if (!x) throw bad_state(); }
  public:
    template <class T>
    explicit any_startable(T x)
      : x(new dynamic_startable_wrapper<T>(x)) {}
    any_startable(const any_startable& other) : x(other.x) {}
    any_startable(any_startable && other) : x(std::move(other.x)) {}
    void swap(any_startable& other) { std::swap(this->x, other.x); }
    SCL_STANDARD_ASSIGN(any_startable);

    void start() { check(); x->start(); }
    void stop() { check(); x->stop(); }
    void abort() { check(); x->abort(); }
    bool is_running() { check(); return x->is_running(); }
    bool is_aborted() { check(); return x->is_aborted(); }
  };
}
