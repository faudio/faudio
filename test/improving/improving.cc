
#include <iostream>
#include <memory>
#include <chrono>       
#include <gtest/gtest.h>
#include <scl/thread.hpp>       
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



void produce(std::shared_ptr<accumulator<float>> x, float a)
{
  for (int i = 0; i < 100; ++i)
  {
    std::chrono::milliseconds dur (50);
    std::this_thread::sleep_for(dur);    
    x->increment(a);
  }
}

std::string print_bar(double x)
{                     
  const double max_bars = 80;
  double amount = x * max_bars;
  for (double d = 0; d < amount; ++d)
    std::cout << "|";
  return "";
}

void poll(improving<float>* a,
          improving<float>* b,
          improving<float>* c,
          improving<float>* d)
{
  for (int i = 0; i < 100; ++i)
  {
    std::chrono::milliseconds dur (50);
    std::this_thread::sleep_for(dur);    

    std::cout << "  a: " << *a << print_bar(a->value() / 100) << "\n";
    std::cout << "  b: " << *b << print_bar(b->value() / 100) << "\n";
    std::cout << "  c: " << *c << print_bar(c->value() / 100) << "\n";
    std::cout << "  d: " << *d << print_bar(d->value() / 100) << "\n";
    std::cout << "\n";
  }
}

TEST(Improving, Poll)
{             
  std::shared_ptr<accumulator<float>> x (new accumulator<float>(0));
  std::shared_ptr<accumulator<float>> y (new accumulator<float>(0));
  
  thread::thread px (produce, x, 1);
  thread::thread py (produce, y, 0.5);
  
  improving<float> a = x->get_improving();
  improving<float> b = y->get_improving();
  improving<float> c = min(a, b);
  improving<float> d = max(a, b);

  thread::thread poller (poll, &a, &b, &c, &d);  
  px.join();
  py.join();
  poller.join();
}

                          
        
}