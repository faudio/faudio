/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_STREAM_FILE
#define _SCLAUDIOX_STREAM_FILE

#include "sndfile.h"

namespace doremir {
namespace scl {

/**
    Options passed to openDeviceStream().
 */
struct FileStreamOptions : public Options
{
    FileStreamOptions(){}
};


/**
    Implementation of a file stream.
 */
class SCLAUDIO_API FileStream : public Stream
{       
public:
    FileStream(FilePath          audioInput,
               FilePath          audioOutput,
               AudioProcessor*   processor,
               FileStreamOptions options = FileStreamOptions() ) 
    {}
        
    ~FileStream()
    {
    }    
};

} // namespace
} // namespace

#endif
