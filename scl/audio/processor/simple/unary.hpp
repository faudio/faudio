
#pragma once

#include <utility>
#include <list>
#include <functional>
#include <scl/utility.hpp>
#include <scl/audio/processor.hpp>

namespace scl
{
  namespace audio
  {
    namespace processor
    {
      using std::pair;

      template <class A, class B>
      class unary_processor
        : public processor < unit, unit, unit,
          unit, unit,
          A, B >
      {
      public:
        unary_processor(std::function<B(A)> function)
          : function(function) {}

        void prepare(const unit& argument)
        {
        }
        void cleanup(unit& result)
        {
        }
        void load(const unit& state)
        {
        }
        void store(unit& state)
        {
        }
        bool is_ready()
        {
        }
        void process(const std::list<unit>& input_messages,
                     const A& input,
                     B& output,
                     std::list<unit>& output_messages)
        {
          output = function(input.first, input.second);
        }
      private:
        std::function<B(A)> function;
      };







      class raw_unary_processor : public raw_processor
      {
        audio_type in_type;
        audio_type out_type;
        void (*function)(ptr_t, ptr_t);
      public:
        using function_type = void (*)(ptr_t, ptr_t);

        raw_unary_processor(audio_type in_type,
                            audio_type out_type,
                            function_type function)
          : in_type(in_type)
          , out_type(out_type)
          , function(function) {}

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
          function(input, output);
        }
      };



    }
  }
}

