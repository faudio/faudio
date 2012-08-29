
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
      // (a -> b) -> (a ~> b)
      class raw_unary_processor : public raw_processor
      {
        audio_type in_type;
        audio_type out_type;

        void (*function)(ptr_t data, ptr_t input, ptr_t output);
        ptr_t data;

      public:
        using function_type = void (*)(ptr_t data, ptr_t input, ptr_t output);

        raw_unary_processor(audio_type in_type,
                            audio_type out_type,
                            function_type function,
                            ptr_t data)
          : in_type(in_type)
          , out_type(out_type)
          , function(function)
          , data(data) {}

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




      struct dynamic_unary
      {
        virtual void call(ptr_t in, ptr_t out) = 0;
      };
      inline void call_dynamic_unary(ptr_t f, ptr_t x, ptr_t y)
      {
        ((dynamic_unary*) f)->call(x, y);
      }

      template <class A, class B>
      struct dynamic_unary_wrapper : public dynamic_unary
      {
        std::function<void(const A&, B&)> f;
        dynamic_unary_wrapper(std::function<void(const A&, B&)> f) 
          : f(f) {}

        void call(ptr_t in, ptr_t out)
        {
          // TODO translation
          const A& in2  = *(const A*) (in);
          B&       out2 = *(B*)       (out);
          f(in2, out2);
        }
      };

      template <class A, class B>
      class unary_processor
        : public processor <
        unit, unit, unit,
        unit, unit,
        A, B >
      {
        std::function<void(const A&, B&)> function;
        raw_processor_ptr raw;
      public:
        unary_processor(std::function<void(const A&,B&)> function)
          : function(function)
          , raw(
            new raw_unary_processor(
              audio_type::get<A>(),
              audio_type::get<B>(),
              call_dynamic_unary,
              (ptr_t) new dynamic_unary_wrapper<A,B>(function))
          ) {}
        
        void load(const unit& state)
        {
          raw->load(state);
        }
        void store(unit& state)
        {
          raw->load(state);
        }
        void prepare(const unit& argument)
        {
          raw->load(argument);
        }
        void cleanup(unit& result)
        {
          raw->load(result);
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
          raw->process((ptr_t) &input_messages, 
            (ptr_t) &input, 
            (ptr_t) &output, 
            (ptr_t) &output_messages);
        }
      };

    }
  }
}

