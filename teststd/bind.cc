
#include <functional>
#include <iostream>


template < typename R1, typename R2, typename T1, typename T2 >
std::function<R1(T2)> operator >> (std::function<R1(T2)> const& f, std::function<R2(T2)> const& g)
{
  return std::bind(f, std::bind(g, std::placeholders::_1));
}


int test_function_composition()
{
  return 0;
}

int test_partial_application()
{
  auto add   = std::plus<int>();
  
  auto add1a = std::bind(add, 1, std::placeholders::_1);
  auto add1b = [] (int x) -> int { return x + 1; };

  std::cout << add1a(2) << "\n";
  std::cout << add1b(2) << "\n";
  return 0;
}


int main(int argc, char const *argv[])
{
  return 0
    || test_partial_application()
    || test_function_composition()
    ;
}
