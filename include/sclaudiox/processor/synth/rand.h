/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_SYNTH_RAND
#define _SCLAUDIOX_SYNTH_RAND

#include "sclaudiox/device/audio.h"
#include "sclaudiox/processor.h"

namespace doremir {
namespace scl {

// =============================================================================

class SCLAUDIO_API RandDescription : public AudioProcessorDescription
{
public:
    String name() { return "Random"; }
    int numberOfInputs() { return 0; }     
    int numberOfOutputs() { return 2; } 
    int numberOfBuses() { return 0; }            
    
    bool isAtomic() { return true; }                   
    bool isStateful() { return true; }                   
    bool isPlugin() { return false; }
}; 

#define SCL_PROCESSOR_RAND_TABLE_SIZE 529200

// =============================================================================

class SCLAUDIO_API Rand : public AudioProcessor 
{
public:    
    Rand()
        : mDescr(NULL)
		, position(0)
	{
		fillTable();
	}

    ~Rand() 
    {
        if (mDescr)
            delete mDescr;
    }

    AudioProcessorDescription* description()
    {                         
        if (!mDescr)
            mDescr = new RandDescription();
        return mDescr;
    }

    void accept(Message message) {}

    void prepare(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
    {
        srand((unsigned) time(0));
    }

    void process(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
    {                             
		int length = signal.numberOfChannels * signal.numberOfFrames;
        for (int i = 0; i < length; ++i)
            signal.data[i] = table[(++position) % SCL_PROCESSOR_RAND_TABLE_SIZE];
    }
    void cleanup(AudioProcessingInformation& info, AudioProcessingBuffer &signal) {}
private:                                                                        
	void fillTable()
	{
		for (int i = 0; i < SCL_PROCESSOR_RAND_TABLE_SIZE; ++i)
			table[i] = ((float)rand()/((float)RAND_MAX/2) - 1) * 0.1;
	}
	
	Sample table[SCL_PROCESSOR_RAND_TABLE_SIZE];
	int position;
    RandDescription* mDescr;
};

} // namespace
} // namespace

#endif
