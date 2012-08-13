
#include <iostream>
#include <chrono>       
#include <memory>
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

TEST(Improving, Min)
{             
  std::shared_ptr<accumulator<float>> acc (
    new accumulator<float>(0));

  improving<float> x (acc);
  improving<float> y (10);
  
  ASSERT_EQ( x.value(),   min(x, y).value() );

  acc->increment(6);
  ASSERT_EQ( x.value(),   min(x, y).value() );

  acc->increment(6);
  ASSERT_EQ( y.value(),   min(x, y).value() );
}

TEST(Improving, Min2)
{             
  std::shared_ptr<accumulator<float>> acc (
    new accumulator<float>(0));

  improving<float> x (10);
  improving<float> y (acc);
  
  ASSERT_EQ( y.value(),   min(x, y).value() );

  acc->increment(6);
  ASSERT_EQ( y.value(),   min(x, y).value() );

  acc->increment(6);
  ASSERT_EQ( x.value(),   min(x, y).value() );
}

TEST(Improving, Max)
{             
  std::shared_ptr<accumulator<float>> acc (
    new accumulator<float>(0));

  improving<float> x (acc);
  improving<float> y (10);
  
  ASSERT_EQ( y.value(),   max(x, y).value() );

  acc->increment(6);
  ASSERT_EQ( y.value(),   max(x, y).value() );

  acc->increment(6);
  ASSERT_EQ( x.value(),   max(x, y).value() );
}

TEST(Improving, Max2)
{             
  std::shared_ptr<accumulator<float>> acc (
    new accumulator<float>(0));

  improving<float> x (10);
  improving<float> y (acc);
  
  ASSERT_EQ( x.value(),   max(x, y).value() );

  acc->increment(6);
  ASSERT_EQ( x.value(),   max(x, y).value() );

  acc->increment(6);
  ASSERT_EQ( y.value(),   max(x, y).value() );
}





TEST(Improving, Basic)
{             
  std::shared_ptr<accumulator<float>> acc (new accumulator<float>(0));
  improving<float> x (acc);
  improving<float> y (10);
  
  std::cout << "x is " << x << "\n";
  std::cout << "y is " << y << "\n";
  std::cout << "min(x,y) is " << min(x,y) << "\n";
  
  acc->increment(6);
  std::cout << "x is " << x << "\n";
  std::cout << "y is " << y << "\n";
  std::cout << "min(x,y) is " << min(x,y) << "\n";

  acc->increment(6);
  std::cout << "x is " << x << "\n";
  std::cout << "y is " << y << "\n";
  std::cout << "min(x,y) is " << min(x,y) << "\n";



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