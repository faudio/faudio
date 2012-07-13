/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_UTIL_SYMBOL
#define _SCLAUDIOX_UTIL_SYMBOL

#include "sclaudiox/core.h"
#include "sclaudiox/util/string.h"

namespace doremir {
namespace scl {

class SymbolData;

/**
    A unique value represented by a String.
    
    Symbols are created by interning a String, that is allocating a symbol for it if
    one does not exist, and store that symbol in the internal symbol table.
            
    \invariant
        `Symbol.intern(x) == Symbol.intern(y)` iff `x == y`
 */
class SCLAUDIO_API Symbol : public NonCopyable, public Resource
{
public:   
    
    /**
        Destroys the given symbol.
     */
    ~Symbol();
    
    bool operator ==(const Symbol& other);
    bool operator !=(const Symbol& other);
    
    /**
        Returns an interned version of the given string.
     */
    static Symbol * intern(String name);
    
    /**
        Returns an locally interned version of the given string.
        For use in single-threaded applications only.
     */
    static Symbol * unsafeIntern(String name);

    /**
        Returns the interned string.
     */
    String string(); 

private:    
    Symbol(String name);    
    SymbolData* mData;    
};


/**
    Copyable reference to a symbol.
 */
class SCLAUDIO_API SymbolRef : public Copyable
{   
public:
    explicit SymbolRef(String string)
        : mSymbol(acquire(Symbol::intern(string))) {}
    
    explicit SymbolRef(Symbol * symbol)
        : mSymbol(acquire(symbol)) {}
    
    SymbolRef(const SymbolRef& other)
        : mSymbol(acquire(other.mSymbol)) {}

    ~SymbolRef() { release(mSymbol); }

private:
    Symbol * mSymbol;
};


} // namespace doremir
} // namespace scl

#endif
