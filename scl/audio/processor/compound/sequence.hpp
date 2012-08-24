
#pragma once

#include <algorithm>
#include <utility>
#include <scl/utility.hpp>
#include <scl/audio/processor.hpp>

namespace scl
{
  namespace audio
  {
    namespace processor
    {
      using std::pair;

      // template <class A, class B, class C>
      // class sequence_processor
      //   : public processor < void, void, void,
      //     void, void,
      //     A, C >
      // {
      // public:
      //   sequence_processor(value_type value)
      //     : value(value) {}
      // 
      //   void prepare(const argument_type& argument)
      //   {
      //   }
      //   void cleanup(result_type& result)
      //   {
      //   }
      //   void load(const state_type& state)
      //   {
      //   }
      //   void store(state_type& state)
      //   {
      //   }
      //   bool is_ready()
      //   {
      //   }
      //   void process(const list<input_message_type>& input_messages,
      //                const input_type& input,
      //                output_type& output,
      //                list<output_message_type>& output_messages)
      //   {
      //   }
      // 
      // private:
      // }; 







      class raw_sequence_processor : public raw_processor
      {   
        raw_processor_ptr f; 
        raw_processor_ptr g; 
        std::unique_ptr<char> xs;
        std::unique_ptr<char> ys;
      public:
        raw_sequence_processor(
          raw_processor_ptr f,
          raw_processor_ptr g
        )
          : raw_processor(0, 0, 0, f->input_size, g->output_size)
          , f(f)
          , g(g)
        {
          assert((g->input_size == 0) || f->output_size == g->input_size);
        }
        void prepare(intptr_t argument)
        {
          
        }
        void cleanup(intptr_t argument)
        {
          
        }
        void load(intptr_t argument) {}
        void store(intptr_t argument) {}
        bool is_ready()
        {
          return true;
        }
        void process(intptr_t input_messages,
                     intptr_t input,
                     intptr_t output,
                     intptr_t output_messages)
        {
          // split input
          // run both processors
          // join output
        }
      };
    }
  }
}

