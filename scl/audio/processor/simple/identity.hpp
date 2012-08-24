
#pragma once

#include <utility>
#include <scl/audio/audio_types.hpp>
#include <scl/audio/processor.hpp>

namespace scl
{
  namespace audio
  {
    namespace processor
    {
      using std::pair;
      using unit = std::nullptr_t;

      template <class A>
      class identity_processor
        : public processor<unit, unit, unit,
                           unit, unit,
                           A, A>
      {
      public:
        identity_processor() = default;

        unit prepare(const unit& argument)
        {          
        }
        unit cleanup(unit& result)
        {          
        }
        unit load(const unit& state)
        {
        }
        unit store(unit& state)
        {
        }
        bool is_ready()
        {
          return true;
        }
        unit process(const std::list<unit>& input_messages,
                     const A& input,
                           A& output,
                           std::list<unit>& output_messages)
        {
          output = input;
        }
      };

      template <template <class U> class T>
      void* create_dynamic_processor(audio_type type)
      {                       
        using tag = audio_type_tag;
        
        // sample32
        if (type.tag == tag::sample32)
          return new T<sample32>;

        // sample64
        if (type.tag == tag::sample64)
          return new T<sample64>;

        // (sample32, sample32)
        if (type.tag == tag::pair 
            && type.fst->tag == tag::sample32 
            && type.snd->tag == tag::sample32) 
          return new T<audio_pair<sample32, sample32>::type>;

        // [sample32]
        if (type.tag == tag::list
            && type.fst->tag == tag::sample32) 
          return new T<audio_list<sample32>::type>;

        // {sample32 x N}
        if (type.tag == tag::vector
            && type.fst->tag == tag::sample32) 
          return new T<audio_vector<sample32>::type>;
      }



      
      void* create_identity_processor(audio_type type)
      {
        return create_dynamic_processor<identity_processor>(type);
      }

    }
  }
}

