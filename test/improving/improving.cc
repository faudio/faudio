
#include <iostream>
#include <memory>
#include <chrono>       
#include <gtest/gtest.h>
#include <scl/thread.hpp>       
#include <scl/improving.hpp>

namespace scl
{
  BOOST_CONCEPT_ASSERT((Moveable<improving<int>>));
  
  // These do not hold, as improving<bool> is not convertible to bool:  
  //    BOOST_CONCEPT_ASSERT((EqualityComparable<improving<int>>));
  //    BOOST_CONCEPT_ASSERT((LessThanComparable<improving<int>>));


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
  for (int i = 0; i < 50; ++i)
  {
    std::chrono::milliseconds dur (100);
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
  improving<float> n (30);   
  // FIXME does not go from accumulating to fixed at threshold
  improving<float> c = min(a, n);
  improving<float> d = min(b, n);

  thread::thread poller (poll, &a, &b, &c, &d);  
  px.join();
  py.join();
  poller.join();
}

                          
        
}