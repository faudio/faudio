
Concepts
========


### Range <*T*>

    T::iterator begin()
    T::iterator end()

### Default <*T*>

    T()

### Copyable <*T*>

    T(const T&)

### Assignable <*T*>

    void operator=(constT&)

### Equality <*T*>

    T == U

### Ordered <*T*>, Equality<*T*>
    
    T < U
    T > U

### Semigroup <*T*>
    
    T + T, + is associative

### Monoid <*T*> : Semigroup<*T*>, Default<*T*>
    
### Writeable <*T*>
                     
    std::ostream << T

### Readable <*T*>

    std::istream >> T
    