
#include <iostream>
#include <chrono>
#include <gtest/gtest.h>
#include <scl/improving.hpp>

namespace scl
{
  // BOOST_CONCEPT_ASSERT((Moveable<wakeup_service<int>>));    

  enum signal 
  {
    foo, bar, baz
  };
  
  using wakeup_ptr = std::shared_ptr<wakeup_service<signal>>;
             
  static atomic::atomic_int count (0);
  void sleep_until(wakeup_ptr wakeup, signal sig)
  {
  
    std::function<bool(signal)> pred = 
      [=] (signal event) { return event == sig; };
    wakeup->sleep(pred);
    count++;
  }  
  
  TEST(Improving, WakeupService)
  { 
    using thread::thread;    
    static const int thread_iterations = 10;

    wakeup_ptr w (new wakeup_service<signal>());
   
    for (int i = 0; i < thread_iterations; ++i)
    {
      thread x (sleep_until, w, foo);
      thread y (sleep_until, w, bar);
      thread z (sleep_until, w, baz);
      x.detach();
      y.detach();
      z.detach();
    }
    
    std::chrono::milliseconds d (1000);
    // std::this_thread::sleep_for(d);

    std::cout << "Beginning wakeup\n";
    w->wake(foo);
    w->wake(foo);
    w->wake(foo);
    w->wake(foo);
    w->wake(bar);
    w->wake(baz);
    std::cout << "Finishing wakeup\n";

    std::this_thread::sleep_for(d);
    std::cout << "Number of woken threads: " << count << "\n";    
    assert(count == thread_iterations * 3);
  }     

  
}


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
// TEST(Improving, Basic)
// {
//   accumulator<int> x;
//   accumulator<int> y;
//   thread::thread(produce(x));
//   thread::thread(produce(y));
//   
//   improving<int> a.get_improving();
//   improving<int> b.get_improving();
//   improving<int> c = min(a, b);
//   improving<int> d = max(a, b);
// 
//   produce(x, 10);
//   produce(y, 11);
//   poll(a, b, c, d, 100);
// }
// 
//                           