
#include <string>
#include <iostream>

// SCL_LOG         Enable all logging
// SCL_LOG_X       Enable logging of x
// SCL_LOG_X_Y     Enable logging of x_y
// SCL_LOG_A       Enable logging of a

struct true_{};
struct false_{};

namespace scl
{
  namespace util
  {
    using std::string;
    
    template <class Predicate> 
    void log_if(const char* str)
    {
      ; // Nothing
    }          

    template <> 
    void log_if<true_>(const char* str)
    {
      std::cout << str;
    }          
  }  
}


// TODO remove the need for bolierplate log copy
// use some kind of callable object?

namespace scl
{
  namespace audio
  {
    namespace stream
    {
      namespace realtime
      {
        namespace util = ::scl::util;
                
        using enable_log = true_;
        
        template <class FormatString> void 
        log(FormatString str)
        { 
          util::log_if<enable_log>(str);
        }

        void start()
        {
          log("Starting realtime stream\n");
        }
      }
      
      namespace nonrealtime
      {
        namespace util = ::scl::util;
                
        using enable_log = true_;
        
        template <class FormatString> void 
        log(FormatString str)
        { 
          util::log_if<enable_log>(str);
        }

        void start()
        {
          log("Starting non-realtime stream\n");
        }
      }
    }
  }  
}


int main (int argc, char const *argv[])
{
  scl::audio::stream::realtime::start();
  scl::audio::stream::nonrealtime::start();
  std::cout << "Finished\n";
  return 0;
}