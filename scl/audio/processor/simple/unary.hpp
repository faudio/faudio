
#pragma once

#include <utility>
#include <list>
#include <functional>
#include <scl/utility.hpp>
#include <scl/audio/audio_types.hpp>
#include <scl/audio/processor.hpp>

namespace scl
{
  namespace audio
  {
    namespace processor
    {
      /** @cond internal */

      // (a -> b) -> (a ~> b)
      class raw_unary_processor : public raw_processor
      {
        audio_type in_type;
        audio_type out_type;
      public:
        using function_type = void (*) (ptr_t data, ptr_t input, ptr_t output);
        using deleter_type  = void (*) (ptr_t data);
        using data_type     = ptr_t;
      private:
        function_type function;
        deleter_type deleter;
        data_type data;
      public:
        raw_unary_processor(audio_type    in_type,
                            audio_type    out_type,
                            function_type function,
                            deleter_type  deleter,
                            data_type     data)
          : in_type(in_type)
          , out_type(out_type)
          , function(function)
          , deleter(deleter)
          , data(data) {}
        
        ~raw_unary_processor()
        {
          if (deleter)
            deleter(data);
        }

        audio_type input_type()
        {
          return in_type;
        }
        audio_type output_type()
        {
          return out_type;
        }

        void prepare(ptr_t argument) {}
        void cleanup(ptr_t argument) {}
        void load(ptr_t argument) {}
        void store(ptr_t argument) {}
        bool is_ready()
        {
          return true;
        }

        void process(ptr_t input_messages,
                     ptr_t input,
                     ptr_t output,
                     ptr_t output_messages)
        {
          function(data, input, output);
        }
      };



      template <class A, class B>
      class unary_ref_closure
      {
        using this_type = unary_ref_closure<A, B>;
        std::function<void(const A&, B&)> f;
      public:
        unary_ref_closure(std::function<void(const A&, B&)> f)
          : f(f) {}
      
        inline void call(ptr_t x, ptr_t y)
        {
          const A& a = *(const A*) (x);
          B&       b = *(B*)       (y);
          f(a, b);
        }
      
        static void caller(ptr_t f, ptr_t x, ptr_t y)
        {
          ((this_type*) f)->call(x, y);
        }
        static void deleter(ptr_t f)
        { 
          delete ((this_type*) f);
        }
        static ptr_t data(std::function<void(const A&, B&)> f)
        {
          return (ptr_t) new this_type(f);
        }
      };   

      template <class A, class B>
      class unary_val_closure
      {
        using this_type = unary_val_closure<A, B>;
        std::function<B(A)> f;
      public:
        unary_val_closure(std::function<B(A)> f)
          : f(f) {}

        inline void call(ptr_t x, ptr_t y)
        {      
          A a;
          scl::raw_copy(x, x + sizeof(A), (ptr_t) &a);
          B b = f(a);
          scl::raw_copy((ptr_t)&b, (ptr_t)&b + sizeof(B), y);
        }

        static void caller(ptr_t f, ptr_t x, ptr_t y)
        {
          ((this_type*) f)->call(x, y);
        }
        static void deleter(ptr_t f)
        { 
          delete ((this_type*) f);
        }
        static ptr_t data(std::function<B(A)> f)
        {
          return (ptr_t) new this_type(f);
        }
      };


      /** @endcond internal */

      template <class A, class B>
      class unary_processor
        : public processor <
            unit, unit, unit,
            unit, unit,
            A, B >
      {
        raw_processor_ptr raw;
      public:
        unary_processor(std::function<B(A)> f)
          : raw(
            new raw_unary_processor(
              audio_type::get<A>(),
              audio_type::get<B>(),
              unary_val_closure<A, B>::caller,
              unary_val_closure<A, B>::deleter,
              unary_val_closure<A, B>::data(f))) {}

        unary_processor(std::function<void(const A&, B&)> f)
          : raw(
            new raw_unary_processor(
              audio_type::get<A>(),
              audio_type::get<B>(),
              unary_ref_closure<A, B>::caller,
              unary_ref_closure<A, B>::deleter,
              unary_ref_closure<A, B>::data(f))) {}

        void load(const unit& state)
        {
          raw->load((ptr_t) &state);
        }
        void store(unit& state)
        {
          raw->store((ptr_t) &state);
        }
        void prepare(const unit& argument)
        {
          raw->prepare((ptr_t) &argument);
        }
        void cleanup(unit& result)
        {
          raw->cleanup((ptr_t) &result);
        }
        bool is_ready()
        {
          return raw->is_ready();
        }
        void process(const std::list<unit>& input_messages,
                     const A& input,
                     A& output,
                     std::list<unit>& output_messages)
        {
          raw->process(
            (ptr_t) &input_messages,
            (ptr_t) &input,
            (ptr_t) &output,
            (ptr_t) &output_messages);
        }
      };

    }
  }
}

