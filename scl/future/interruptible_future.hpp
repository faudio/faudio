
namespace scl
{
  namespace future
  {  
    concept
    class Interrupible
    {
      void interrupt();
      bool is_interrupted();
    };

    concept
    class Interceptable
    {
      void disable();
      void enable();
      void disable(int priorities);
      void enable(int priorities);
    };

    
    class interruptible_future
    {
    };
    
  }
}

