
#include <thread>

namespace scl
{   
  using thread::future;

  // Similar to a future, a sequence of lower bounds
  template <class T>
  improving
  {
  public:
    // LowerBound<T>
    // LessThanComparable<T>
    // ==>
    // LessThanComparable<improving<T>>
    // EqualityComparable<improving<T>>
    // Monoid<improving<T>>
    
    improving(); // Fixed at lowest bound
    improving(const T);
    ~improving();

    T value() const;
    T& value() const;   //see future
    void value() const; //see future
    bool known() const;
    bool wait() const;
    future<T> to_future() const;
  
  private:
    thread::atomic<T>    current;
    thread::future<void> computation;
  };             
  
  // (<) :: improving a -> improving a -> improving a
  // x < y  is known to be true  iff x is known and x < y (as y can only grow)
  // x < y  is known to be false iff y is known and !(x < y) (as x can only grow)
  int operator <(const improving<T>& x, const improving<T>& y)
  {
  }
  int operator ==(const improving<T>& x, const improving<T>& y)
  {
  }
  int operator +(const improving<T>& x, const improving<T>& y)
  {
  }
  int operator -(const improving<T>& x, const improving<T>& y)
  {
  }
  

  // FIXME analogue to std::promise to improve and/or fix the bound
  template <class T>
  class cumulative
  {    
    get_improving();

    void fix();
    void increment(const T& amount);
    void increment_and_fix(const T& amount);

    void fix_at_thread_exit();
    void increment_at_thread_exit(const T& amount);
    void increment_and_fix_at_thread_exit(const T& amount);

    void set_exception(std::exception_ptr ptr);
    void set_exception_at_thread_exit(std::exception_ptr ptr);
  };
    
  // FIXME analogue to std::packaged_task
  class packaged_integral
  {};

}
