
#pragma once

#include <utility>
#include <scl/audio/processor.hpp>

namespace scl
{
  namespace audio
  {
    namespace processor
    {
      using std::pair;

      template <class A, class B, class C>
      class binary_processor
        : public processor < void, void, void,
          void, void,
          pair<A, B>, C >
      {
      public:
        using state_type = void;
        using argument_type = void;
        using result_type = void;
        using input_message_type = void;
        using output_message_type = void;
        using input_type = std::pair<A, B>;
        using output_type = C;

        binary_processor(std::function<C(A, B)> function)
          : function(function) {}

        void prepare(const argument_type& argument)
        {
        }
        void cleanup(result_type& result)
        {
        }
        void load(const state_type& state)
        {
        }
        void store(state_type& state)
        {
        }
        bool is_ready()
        {
          return true;
        }
        void process(const list<input_message_type>& input_messages,
                     const input_type& input,
                     output_type& output,
                     list<output_message_type>& output_messages)
        {
          output = function(input.first, input.second);
        }
      private:
        std::function<C(A, B)> function;
      };

      class raw_binary_processor : public raw_processor
      {
        ptr_t function;
      public:
        raw_binary_processor(
          ptr_t function,
          size_t first_size,
          size_t second_size,
          size_t out_size
        )
          : raw_processor(0, 0, 0, first_size + second_size, out_size)
        {
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
          // function(input, output);
        }
      };

    }
  }
}

