/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_STREAM_BUFFER
#define _SCLAUDIOX_STREAM_BUFFER

namespace doremir {
namespace scl {

/**
    Options passed to openDeviceStream().
 */
struct BufferStreamOptions : public Options
{
    BufferStreamOptions(){}
};


/**
    Implementation of a file stream.
 */
class SCLAUDIO_API BufferStream : public Stream
{       
public:
    BufferStream(AudioBuffer*        audioInput,
                 AudioBuffer*        audioOutput,
                 AudioProcessor*     processor,
                 BufferStreamOptions options) 
    {}
        
    ~BufferStream()
    {
    }  
};

} // namespace
} // namespace

#endif
