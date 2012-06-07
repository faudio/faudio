
// AtomicHeap
//     empty              :: Heap a
//     insert             :: Int -> a -> Heap a -> IO ()
//     extractAllLessThan :: Int -> Heap a      -> IO [a]


template <class Value, class Priority = Int> class AtomicHeap
{ 
public:    

    /** 
        Insert the given element at the given priority.
        @atomic
        @scales O(n)
     **/
    virtual void        
    insert ( Priority p, Value v );

    /** 
        Fetch and remove all elements with priority less than or equal to the given priority.
        @atomic 
        @scales O(1)
     **/
    virtual List<Value> 
    extractAllLessThanOrEqualTo ( Priority p );
};

template <class T, class P> *AtomicHeap<T> emptyAtomicHeap();





class AtomicHeapImpl
{
    virtual void  insert ( void* p, void* v )
    {                     
        while(true)
        {
            rootAndHandled = location->fetchWithHandled();

            // discard handled elements
            // insert new elements

            if (location->store(root))
                return;
        }
    }

    virtual void* extractAllLessThanOrEqualTo ( void* priority )
    {
        root = location->fetchAndUpdateHandled(priority)
        root = root->filter(predicates->lessThan(2));
        return root->map(getElements);
    }

    virtual AtomicHeapImpl();    
    virtual ~AtomicHeapImpl();    
};
