/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#include <map>

#include "sclaudiox/util/symbol.h"
#include "sclaudiox/util/concurrency.h"

namespace doremir {
namespace scl {

namespace
{
    Mutex                     kSymbolTableMutex;
    std::map<String, Symbol*> kSymbolTable;
}   

struct SymbolData
{
    String name;
};
    
Symbol* Symbol::intern(String name)
{
    Lock lock (kSymbolTableMutex);
    return unsafeIntern(name);
}
    
Symbol* Symbol::unsafeIntern(String name)
{
    if (!kSymbolTable[name])
        kSymbolTable[name] = new Symbol(name);
    return kSymbolTable[name];
}

String Symbol::string()
{
    return mData->name;
}

Symbol::Symbol(String name)
    : mData(new SymbolData) 
{                    
    mData->name = name;
    kSymbolTable[mData->name] = this;
} 

Symbol::~Symbol()
{
    kSymbolTable[mData->name] = NULL;
    delete mData;
}

} // namespace scl
} // namespace doremir

