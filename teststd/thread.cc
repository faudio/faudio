
#include <thread>
#include <boost/thread.hpp>
#include <mutex>
#include <condition_variable>
#include <atomic>
#include <iostream>

static boost::mutex foo_bar_mutex;
void foo()
{
    for(int i = 0; i < 10; ++i)
    {
        boost::lock_guard<boost::mutex> foo_bar_lock (foo_bar_mutex);
        std::cout << "This is foo\n";
    }
}
void bar()
{
    for(int i = 0; i < 10; ++i)
    {
        boost::lock_guard<boost::mutex> foo_bar_lock (foo_bar_mutex);
        std::cout << "This is bar\n";
    }
}
int two_threads()
{
    boost::thread x (foo);
    boost::thread y (bar);
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