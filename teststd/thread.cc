
// TODO differences between C++0x and Boost
// Use intersection

#define USE_BOOST_THREAD

#ifndef USE_BOOST_THREAD
    #warning "Not using boost threads"
    #include <thread>
    #include <condition_variable>
    #include <atomic>
    namespace thread = std;
#else
    #warning "Using boost threads"
    #include <boost/thread.hpp>
    namespace thread = boost;
#endif

#include <iostream>

static thread::mutex foo_bar_mutex;
void foo()
{
    for(int i = 0; i < 10; ++i)
    {
        thread::lock_guard<thread::mutex> foo_bar_lock (foo_bar_mutex);
        std::cout << "This is foo\n";
    }
}
void bar()
{
    for(int i = 0; i < 10; ++i)
    {
        thread::lock_guard<thread::mutex> foo_bar_lock (foo_bar_mutex);
        std::cout << "This is bar\n";
    }
}
int two_threads()
{
    thread::thread x (foo);
    thread::thread y (bar);
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
