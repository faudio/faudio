
#include <array>
#include <boost/range/algorithm/transform.hpp>

#include <sprout/array.hpp>
#include <sprout/algorithm/transform.hpp>


int test_runtime()
{
  std::array<int,10> xs = { 1, 2, 3 };
  std::array<int,10> ys;
  boost::transform(xs, ys.begin(), [] (int x) -> int { return x + 10; });
  for (int y : ys)
    std::cout << y << " ";
  std::cout << "\n";
  
  return 0;
}


template<typename T>
struct add_10 {
public:
	typedef T argument_type;
	typedef T result_type;
public:
	constexpr T operator()(T const& x) { return x + 10; }
};

int test_compile_time()
{
  constexpr sprout::array<int,10> xs = { 1, 2, 3 };
  constexpr sprout::array<int,10> ys = {};
  constexpr auto zs = sprout::transform(
    sprout::begin(xs), 
    sprout::end(xs),
    ys,
    add_10<int>()
    );
  for (int z : zs)
    std::cout << z << " ";
  std::cout << "\n";
  
  static_assert(zs[0] == 11, "zs[0] == 11");

  static_assert(zs[0] == 11, "zs[0] == 11");


  return 0;
}



int main(int argc, char const* argv[])
{
  return 0
    || test_runtime()
    || test_compile_time()
    ;
}
