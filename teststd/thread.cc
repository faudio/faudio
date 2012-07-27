
#define USE_BOOST_THREAD

#ifndef USE_BOOST_THREAD
  #include <thread>
  #include <condition_variable>
  #include <atomic>
  #define T std
#else
  #include <boost/thread.hpp>
  #define T boost
#endif

#include <iostream>

static T::mutex foo_bar_mutex;
void foo()
{
    for(int i = 0; i < 10; ++i)
    {
        T::lock_guard<T::mutex> foo_bar_lock (foo_bar_mutex);
        std::cout << "This is foo\n";
    }
}
void bar()
{
    for(int i = 0; i < 10; ++i)
    {
        T::lock_guard<T::mutex> foo_bar_lock (foo_bar_mutex);
        std::cout << "This is bar\n";
    }
}
int two_threads()
{
    T::thread x (foo);
    T::thread y (bar);
    x.join();
    y.join();
    return 0;
}



int main (int argc, char const *argv[])
{
    return
       two_threads()
    ;
}
