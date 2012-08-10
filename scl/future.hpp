
#pragma once

#include <list>
#include <boost/optional.hpp>
#include <scl/atomic.hpp>
#include <scl/thread.hpp>
#include <scl/concept.hpp>

// namespace scl
// {
//   template<typename T>
//   class promise
//   {
//   public:
//     promise();
//     promise(promise &&);
//     ~promise();
//     promise& operator=(promise &&);
//
//     template<typename Allocator>
//     promise(std::allocator_arg_t, Allocator const&);
//
//     promise(promise const&) = delete;
//     promise& operator=(promise const&) = delete;
//
//     void swap(promise&);
//
//     std::future<T> get_future();
//
//     void set_value(const T && value)
//     {
// for (auto cv : cond_vars)
//         cv.notify_all();
//     }
//     void set_value(T && value)
//     {
//       set_value(const_cast < T && >(value));
//     }
//
//     void set_exception(std::exception_ptr p);
//
//   private:
//     boost::optional<T> value;
//     std::list<thread::condition_variable> cond_vars;
//     friend class future<T>;
//   };
//
//   template<typename T>
//   class future
//   {
//   public:
//     future();
//     future(future &&);
//     future& operator=(future &&);
//     ~future();
//
//     future(future const&) = delete;
//     future& operator=(future const&) = delete;
//
//     bool valid() const
//     {
//     }
//     bool is_ready() const;
//     bool has_exception() const;
//     bool has_value() const;
//
//     T get()
//     {
//       wait();
//       // TODO transfer exception
//       return promise_handle.value;
//     }
//     void wait();
//     {
//       thread::unique_lock<thread::mutex> lock(promise_handle.mutex);
//       cond_var.wait(lock);
//     }
//   private:
//     promise<T>& promise_handle;
//     thread::condition_variable cond_var;
//   };
//
// }

