
#include <iostream>
#include <gtest/gtest.h>
#include <scl/future/wakeup_service.hpp>

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
  
  TEST(Future, WakeupService)
  { 
	namespace t = thread;
    static const int thread_iterations = 10;

    wakeup_ptr w (new wakeup_service<signal>());
   
    for (int i = 0; i < thread_iterations; ++i)
    {
      t::thread x (sleep_until, w, foo);
      t::thread y (sleep_until, w, bar);
      t::thread z (sleep_until, w, baz);
      x.detach();
      y.detach();
      z.detach();
    }
    
    ::scl::chrono::milliseconds d (1000);
    // std::this_thread::sleep_for(d);

    std::cout << "Beginning wakeup\n";
    w->wake(foo);
    w->wake(foo);
    w->wake(foo);
    w->wake(foo);
    w->wake(bar);
    w->wake(baz);
    std::cout << "Finishing wakeup\n";

    t::this_thread::sleep_for(d);
    std::cout << "Number of woken threads: " << count << "\n";    
    assert(count == thread_iterations * 3);
  }     
}

