
#include <thread>
#include <atomic>
#include <iostream>

static std::mutex foo_bar_mutex;
void foo()
{
    for(int i = 0; i < 10; ++i)
    {
        std::lock_guard<std::mutex> foo_bar_lock (foo_bar_mutex);
        std::cout << "This is foo\n";
    }
}
void bar()
{
    for(int i = 0; i < 10; ++i)
    {
        std::lock_guard<std::mutex> foo_bar_lock (foo_bar_mutex);
        std::cout << "This is bar\n";
    }
}
int two_threads()
{
    std::thread x (foo);
    std::thread y (bar);
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