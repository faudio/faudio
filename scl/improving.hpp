
namespace scl
{


// Similar to a future, a sequence of lower bounds
  template<T>
  improving
  {
    T get() const;
    // T& get() const;   see future
    // void get() const; see future
    bool known() const;
    bool wait() const;
  };

// (<) :: improving a -> improving a -> improving a
// x < y  is known to be true  iff x is known and x < y (as y can only grow)
// x < y  is known to be false iff y is known and !(x < y) (as x can only grow)

}
