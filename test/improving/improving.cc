
#include <iostream>
#include <chrono>
#include <gtest/gtest.h>
#include <scl/improving.hpp>

namespace scl
{
 // BOOST_CONCEPT_ASSERT((Moveable<improving<int>>));
//   // EqualityComparable<improving<T>>
//   // Monoid<improving<T>>
// 
// 
// void produce(accumulator<int>& x, int interval)
// {
//   for (int i; i < 10; ++i)
//   {
//     thread::sleep_millis(interval);
//     x += 1;
//   }
//   x.fix();
// }         
// 
// void poll(improving<int>& a, improving<int>& b, improving<int>& c, improving<int>& d, int interval)
// {
//   for (int i; i < 10; ++i)
//   {
//     thread::sleep_millis(interval);
//     std::cout << "a: " << a.value() << "\n";
//     std::cout << "b: " << b.value() << "\n";
//     std::cout << "c: " << c.value() << "\n";
//     std::cout << "d: " << d.value() << "\n";
//     std::cout << "\n";
//   }
// }
// 
// 


TEST(Improving, Basic)
{             
  std::cout << "Size of improving<int> is: " << sizeof(improving<int>) << "\n";
  
  // accumulator<int> x;
  // accumulator<int> y;
  // x.increment(1);
  
  // thread::thread(produce(x));
  // thread::thread(produce(y));
  // 
  // improving<int> a.get_improving();
  // improving<int> b.get_improving();
  // improving<int> c = min(a, b);
  // improving<int> d = max(a, b);
  // 
  // produce(x, 10);
  // produce(y, 11);
  // poll(a, b, c, d, 100);
}

                          
        
}