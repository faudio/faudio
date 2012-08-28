
#include <list>
#include <gtest/gtest.h>
#include <scl/chase.hpp>


TEST(AudioUtility, Chase)
{
  using namespace scl;
  chase<int> xs = { 1, 2, 3 };
  chase<int> ys = { 5, 6, 7, 8, 9 };
  
  raw_dump((ptr_t) xs.get(), ((ptr_t) xs.get()) + 32);
  raw_dump((ptr_t) ys.get(), ((ptr_t) ys.get()) + 32);

  std::cout << "size of xs: " << scl::size(xs) << "\n";
  std::cout << "size of ys: " << scl::size(ys) << "\n";

  std::cout << "xs: \n";
  for (int x : xs)
    std::cout << x << "\n";  

  std::cout << "ys: \n";
  for (int y : ys)
    std::cout << y << "\n";
    
  xs = std::move(xs.append(ys));

  raw_dump((ptr_t) xs.get(), ((ptr_t) xs.get()) + 32);

  std::cout << "size of xs: " << scl::size(xs) << "\n";
  std::cout << "size of ys: " << scl::size(ys) << "\n";

  std::cout << "xs: \n";
  for (int x : xs)
    std::cout << x << "\n";
  
  std::cout << "ys: \n";
  for (int y : ys)
    std::cout << y << "\n";
}
       
TEST(AudioUtility, ChaseRestore)
{
  using namespace scl;
  chase<int> xs = { 10, 11, 12 };
  auto n = xs.release();
  chase<int> ys (n);
  
  std::cout << "size of xs: " << scl::size(xs) << "\n";
  std::cout << "size of ys: " << scl::size(ys) << "\n";

  std::cout << "xs: \n";
  for (int x : xs)
    std::cout << x << "\n";
  std::cout << "ys: \n";
  for (int y : ys)
    std::cout << y << "\n";
}

TEST(AudioUtility, ChaseConvert)
{
  using namespace scl;

  chase<long long> xs = { 1, 2, 3, 4, 5 };
  chase<char> ys ((chase<char>::track_pointer) xs.release());
    
  ASSERT_EQ(ys.size(), 5);
} 


