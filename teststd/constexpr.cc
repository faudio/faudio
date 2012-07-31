
#include <functional>
#include <initializer_list>
#include <iostream>


template <class T>
struct plus : public std::binary_function<T, T, T>
{
  constexpr T operator()(T x, T y)
  {
    return x + y;
  }
};

template <class T>
struct min : public std::binary_function<T, T, T>
{
  constexpr T operator()(T x, T y)
  {
    return (x < y ? x : y);
  }
};

template <class T>
struct HasZero : public std::binary_function<T, bool, bool>
{
  constexpr bool operator()(T x, bool y)
  {
    return y || (x == 0);
  }
};

template < class F,
           class A = typename F::first_argument_type,
           class B = typename F::result_type >
constexpr B foldr_(const A* xs, int off, int size, const B& z)
{
  return (off >= size 
    ? z 
    : F()(xs[off], foldr_<F, A, B>(xs, off + 1, size, z)));
}

template < class F,
           class A = typename F::first_argument_type,
           class B = typename F::result_type >
constexpr B foldr(const B& z, const std::initializer_list<A>& xs)
{
  return foldr_<F, A, B>(xs.begin(), 0, xs.size(), z);
}

template <class T>
constexpr T sum(const std::initializer_list<T>& xs)
{
  return foldr<plus<T>>(0, xs);
}



int test_foo()
{
  std::cout << "Sum: "           << foldr<plus<double>> (0, {1, 2, 3})      << "\n";
  std::cout << "Contains Zero: " << foldr<HasZero<int>> (false, {1, 2, 0})  << "\n";
  std::cout << "Least value: "   << foldr<min<double>>  (1e9, { -1, -3, 0}) << "\n";
  // static_assert(foldr<plus<double>>(0, {1,2,3}) == 6, "sum");
  // static_assert(foldr<HasZero<int>>(false, {1,2,0}), "zero");
  // static_assert(foldr<min<double>>(1e9, {-1,-3,0}) == -3, "min");
  // static_assert(sum({1,2,3}) == 6, "Sum is not 6");

  return 0;
}

int main(int argc, char const* argv[])
{
  return 0
         || test_foo()
         ;
}
