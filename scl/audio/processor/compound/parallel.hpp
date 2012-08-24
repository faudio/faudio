
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

      // template <class A, class B, class C, class D>
      // class parallel_processor
      //   : public processor < void, void, void,
      //     void, void,
      //     pair<A, C>, pair<B, D >>
      // {
      // public:
      //   parallel_processor(value_type value)
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







      class raw_parallel_processor : public raw_processor
      {                        
        raw_processor_ptr fst;
        raw_processor_ptr snd;
      public:
        raw_parallel_processor(
          raw_processor_ptr fst,
          raw_processor_ptr snd
        )
          : raw_processor(0, 0, 0,
                          fst->input_size + snd->input_size,
                          fst->output_size + snd->output_size)
          , fst(fst)
          , snd(snd)
        {}
        void prepare(intptr_t argument) {}
        void cleanup(intptr_t argument) {}
        void load(intptr_t argument) {}
        void store(intptr_t argument) {}
        bool is_ready()
        {
          return fst->is_ready() && snd->is_ready();
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

