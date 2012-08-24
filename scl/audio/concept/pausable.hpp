
#pragma once

#include <memory>
#include <scl/concept.hpp>
#include <scl/exception.hpp>
#include <scl/utility.hpp>
#include <scl/audio/concept/abortable.hpp>

namespace scl
{
  /**
    Refines Abortable with pause and resume functions. In constrast to abort,
    these operations are not permanent.

    Synopsis:

        concept Pausable<typename X> : Abortable<X>
        {
          void X::pause();
          void X::resume();
          bool X::is_running();
        }

    Semantics:
      - `x.pause()`
        - Precondition: The state of `x` is not aborted
        - Postcondition: The state of `x` is not running.
      - `x.resume()`
        - Precondition: The state of `x` is not aborted
        - Postcondition: The state of `x` is running.
      - `x.is_running()`
        - Whether `x` is running or not.

    Invariants:
      - `x.is_aborted()` implies `!x.is_running()`.
      - For any statements S, T and U, in the form `{ S; x.pause(); T; x.resume(); U; }`,
        x is running in U, if and only if x is running in S

    Models:
      future
  */
  template <class X>
  struct Pausable : Abortable<X>
  {
    BOOST_CONCEPT_USAGE(Pausable)
    {
      x.start();
      x.stop();
      bool state = x.is_running();
    }
  private:
    X t;
  };

  class dynamic_pausable
  {
  public:
    virtual ~dynamic_pausable() {}
    virtual void abort() = 0;
    virtual void pause() = 0;
    virtual void resume() = 0;
    virtual bool is_running() = 0;
    virtual bool is_aborted() = 0;
  };

  template<class T>
  class dynamic_pausable_wrapper : public dynamic_pausable
  {
    T x;
  public:
    dynamic_pausable_wrapper(const T& x)
      : x(x) {}
    ~dynamic_pausable_wrapper() {}
    void pause()
    {
      x.pause();
    }
    void resume()
    {
      x.resume();
    }
    void abort()
    {
      x.abort();
    }
    bool is_running()
    {
      return x.is_running();
    }
    bool is_aborted()
    {
      return x.is_aborted();
    }
  };

  class any_pausable
  {
    std::shared_ptr<dynamic_pausable> x;
    inline void check()
    {
      if (!x) throw bad_state();
    }
  public:
    template <class T>
    explicit any_pausable(T x)
      : x(new dynamic_pausable_wrapper<T>(x)) {}
    any_pausable(const any_pausable& other) : x(other.x) {}
    any_pausable(any_pausable && other) : x(std::move(other.x)) {}
    void swap(any_pausable& other)
    {
      std::swap(this->x, other.x);
    }
    SCL_STANDARD_ASSIGN(any_pausable);

    void pause()
    {
      check();
      x->pause();
    }
    void resume()
    {
      check();
      x->resume();
    }
    void abort()
    {
      check();
      x->abort();
    }
    bool is_running()
    {
      check();
      return x->is_running();
    }
    bool is_aborted()
    {
      check();
      return x->is_aborted();
    }
  };
}

