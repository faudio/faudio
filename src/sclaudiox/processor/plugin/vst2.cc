/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
/**
    @file   sclaudiox/processor/plugin/vst2.cc
    @author Hans Hoglund
 */

#include <stdio.h>
#include "sclaudiox/defines.h"

#ifdef SCL_WIN
    #include <windows.h>
#endif
#ifdef SCL_OSX
    #include <CoreFoundation/CoreFoundation.h>
#endif
#include <pluginterfaces/vst2.x/aeffectx.h>

#include "sclaudiox/processor/plugin/vst2.h"
#include "sclaudiox/util/misc.h"



namespace doremir {
namespace scl {

struct Vst2PluginDescriptionData
{                          
    Vst2Plugin* plugin;
    // info
};
struct Vst2PluginProcessorData
{
    Vst2Plugin* plugin;
    Vst2PluginDescription* description;
    // instance
};
struct Vst2PluginData
{
    Vst2PluginDescription* description;
    // plugin
};


// =======================================================================================

Vst2PluginDescription::Vst2PluginDescription(Vst2PluginDescriptionData* data)
    : mData(data) {}
    
Vst2PluginDescription::~Vst2PluginDescription()
{
    delete mData;
}

String Vst2PluginDescription::name()
{         
    // FIXME   
    return "";
}                  
bool Vst2PluginDescription::isAtomic()
{
    // FIXME  
    return false;
}
bool Vst2PluginDescription::isStateful()
{
    // FIXME     
    return false;
}
bool Vst2PluginDescription::isPlugin()
{
    // FIXME     
    return false;
}

int Vst2PluginDescription::numberOfInputs()
{
    // FIXME     
    return 0;
}

int Vst2PluginDescription::numberOfOutputs()
{
    // FIXME 
    return 0;
}

int Vst2PluginDescription::numberOfBuses()
{
    // FIXME 
    return 0;
}


AudioPlugin* Vst2PluginDescription::plugin()
{
    // FIXME 
    return 0;
}              


// =======================================================================================

Vst2PluginProcessor::Vst2PluginProcessor(Vst2PluginProcessorData* data)
    : mData(data) {}
    
Vst2PluginProcessor::~Vst2PluginProcessor()
{
    delete mData;
}


AudioProcessorDescription* Vst2PluginProcessor::description()
{
    // FIXME
    return NULL;
}

void Vst2PluginProcessor::accept(Message message)
{
    // FIXME
}

void Vst2PluginProcessor::prepare(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
{
    // FIXME    
}

void Vst2PluginProcessor::process(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
{
    // FIXME
}

void Vst2PluginProcessor::cleanup(AudioProcessingInformation& info, AudioProcessingBuffer &signal)
{
    // FIXME
}

AudioPlugin* Vst2PluginProcessor::plugin()
{
    // FIXME
    return NULL;
}
    
void* Vst2PluginProcessor::nativePluginInstance()
{
    // FIXME
    return NULL;
}
                                      

// =======================================================================================

Vst2Plugin::Vst2Plugin(Vst2PluginData* data)
    : mData(data) {}

Vst2Plugin::~Vst2Plugin()
{
    delete mData;
}

AudioPluginProcessorDescription* Vst2Plugin::description()
{
    return vst2PluginDescription();
}

AudioPluginProcessor* Vst2Plugin::createProcessor()
{
    return createVst2PluginProcessor();
}               

void* Vst2Plugin::nativePlugin()
{
    // FIXME
    return NULL;
}

Vst2PluginDescription* Vst2Plugin::vst2PluginDescription()
{                       
    if(!mData->description)
    {                                                   
        // create and init description
    }   
    return mData->description;
}

Vst2PluginProcessor* Vst2Plugin::createVst2PluginProcessor()
{
    // FIXME
    return NULL;
} 

std::list<Vst2Plugin*> Vst2Plugin::vst2Plugins(FilePath path)
{                                                
    // FIXME
    return list::create<Vst2Plugin*>();
}







//-------------------------------------------------------------------------------------------------------
static const VstInt32 kBlockSize = 512;
static const float kSampleRate = 48000.f;
static const VstInt32 kNumProcessCycles = 5;

//-------------------------------------------------------------------------------------------------------
typedef AEffect* (*PluginEntryProc) (audioMasterCallback audioMaster);
static VstIntPtr VSTCALLBACK HostCallback (AEffect* effect, VstInt32 opcode, VstInt32 index, VstIntPtr value, void* ptr, float opt);

//-------------------------------------------------------------------------------------------------------
// PluginLoader
//-------------------------------------------------------------------------------------------------------
struct PluginLoader
{
	void* module;

	PluginLoader ()
        : module (0) {}

	~PluginLoader ()
	{
		if (module)
		{
		#if _WIN32
			FreeLibrary ((HMODULE)module);
		#elif TARGET_API_MAC_CARBON
			CFBundleUnloadExecutable ((CFBundleRef)module);
			CFRelease ((CFBundleRef)module);
		#endif
		}
	}

    bool loadLibrary(String fileName)
    {
        // FIXME unicode names etc
        return loadLibrary(toSimpleString(fileName));
    }

	bool loadLibrary(const char* fileName)
	{
	#if _WIN32
		module = LoadLibrary (fileName);
	#elif TARGET_API_MAC_CARBON
		CFStringRef fileNameString = CFStringCreateWithCString (NULL, fileName, kCFStringEncodingUTF8);
		if (fileNameString == 0)
			return false;
		CFURLRef url = CFURLCreateWithFileSystemPath (NULL, fileNameString, kCFURLPOSIXPathStyle, false);
		CFRelease (fileNameString);
		if (url == 0)
			return false;
		module = CFBundleCreate (NULL, url);
		CFRelease (url);
		if (module && CFBundleLoadExecutable ((CFBundleRef)module) == false)
			return false;
	#endif
		return module != 0;
	}

	PluginEntryProc getMainEntry()
	{
		PluginEntryProc mainProc = 0;
	#if _WIN32
		mainProc = (PluginEntryProc)GetProcAddress ((HMODULE)module, "VSTPluginMain");
		if (!mainProc)
			mainProc = (PluginEntryProc)GetProcAddress ((HMODULE)module, "main");
	#elif TARGET_API_MAC_CARBON
		mainProc = (PluginEntryProc)CFBundleGetFunctionPointerForName ((CFBundleRef)module, CFSTR("VSTPluginMain"));
		if (!mainProc)
			mainProc = (PluginEntryProc)CFBundleGetFunctionPointerForName ((CFBundleRef)module, CFSTR("main_macho"));
	#endif
		return mainProc;
	}
};

void checkEffectProperties (AEffect* effect)
{
	printf ("HOST> Gathering properties...\n");

	char effectName[256] = {0};
	char vendorString[256] = {0};
	char productString[256] = {0};

	effect->dispatcher (effect, effGetEffectName, 0, 0, effectName, 0);
	effect->dispatcher (effect, effGetVendorString, 0, 0, vendorString, 0);
	effect->dispatcher (effect, effGetProductString, 0, 0, productString, 0);

	printf ("Name = %s\nVendor = %s\nProduct = %s\n\n", effectName, vendorString, productString);

	printf ("numPrograms = %d\nnumParams = %d\nnumInputs = %d\nnumOutputs = %d\n\n", 
			effect->numPrograms, effect->numParams, effect->numInputs, effect->numOutputs);

	// Iterate programs...
	for (VstInt32 progIndex = 0; progIndex < effect->numPrograms; progIndex++)
	{
		char progName[256] = {0};
		if (!effect->dispatcher (effect, effGetProgramNameIndexed, progIndex, 0, progName, 0))
		{
			effect->dispatcher (effect, effSetProgram, 0, progIndex, 0, 0); // Note: old program not restored here!
			effect->dispatcher (effect, effGetProgramName, 0, 0, progName, 0);
		}
		printf ("Program %03d: %s\n", progIndex, progName);
	}

	printf ("\n");

	// Iterate parameters...
	for (VstInt32 paramIndex = 0; paramIndex < effect->numParams; paramIndex++)
	{
		char paramName[256] = {0};
		char paramLabel[256] = {0};
		char paramDisplay[256] = {0};

		effect->dispatcher (effect, effGetParamName, paramIndex, 0, paramName, 0);
		effect->dispatcher (effect, effGetParamLabel, paramIndex, 0, paramLabel, 0);
		effect->dispatcher (effect, effGetParamDisplay, paramIndex, 0, paramDisplay, 0);
		float value = effect->getParameter (effect, paramIndex);

		printf ("Param %03d: %s [%s %s] (normalized = %f)\n", paramIndex, paramName, paramDisplay, paramLabel, value);
	}

	printf ("\n");

	// Can-do nonsense...
	static const char* canDos[] =
	{
		"receiveVstEvents",
		"receiveVstMidiEvent",
		"midiProgramNames"
	};

	for (VstInt32 canDoIndex = 0; canDoIndex < sizeof (canDos) / sizeof (canDos[0]); canDoIndex++)
	{
		printf ("Can do %s... ", canDos[canDoIndex]);
		VstInt32 result = (VstInt32)effect->dispatcher (effect, effCanDo, 0, 0, (void*)canDos[canDoIndex], 0);
		switch (result)
		{
			case 0  : printf ("don't know"); break;
			case 1  : printf ("yes"); break;
			case -1 : printf ("definitely not!"); break;
			default : printf ("?????");
		}
		printf ("\n");
	}

	printf ("\n");
}

//-------------------------------------------------------------------------------------------------------
void checkEffectProcessing (AEffect* effect)
{
	float** inputs = 0;
	float** outputs = 0;
	VstInt32 numInputs = effect->numInputs;
	VstInt32 numOutputs = effect->numOutputs;
	
	if (numInputs > 0)
	{
		inputs = new float*[numInputs];
		for (VstInt32 i = 0; i < numInputs; i++)
		{
			inputs[i] = new float[kBlockSize];
			memset (inputs[i], 0, kBlockSize * sizeof (float));
		}
	}

	if (numOutputs > 0)
	{
		outputs = new float*[numOutputs];
		for (VstInt32 i = 0; i < numOutputs; i++)
		{
			outputs[i] = new float[kBlockSize];
			memset (outputs[i], 0, kBlockSize * sizeof (float));
		}
	}

	printf ("HOST> Resume effect...\n");
	effect->dispatcher (effect, effMainsChanged, 0, 1, 0, 0);

	for (VstInt32 processCount = 0; processCount < kNumProcessCycles; processCount++)
	{
		printf ("HOST> Process Replacing...\n");
		effect->processReplacing (effect, inputs, outputs, kBlockSize);
	}

	printf ("HOST> Suspend effect...\n");
	effect->dispatcher (effect, effMainsChanged, 0, 0, 0, 0);

	if (numInputs > 0)
	{
		for (VstInt32 i = 0; i < numInputs; i++)
			delete [] inputs[i];
		delete [] inputs;
	}

	if (numOutputs > 0)
	{
		for (VstInt32 i = 0; i < numOutputs; i++)
			delete [] outputs[i];
		delete [] outputs;
	}
}

//-------------------------------------------------------------------------------------------------------
VstIntPtr VSTCALLBACK HostCallback (AEffect* effect, VstInt32 opcode, VstInt32 index, VstIntPtr value, void* ptr, float opt)
{
	VstIntPtr result = 0;

	// Filter idle calls...
	bool filtered = false;
	if (opcode == audioMasterIdle)
	{
		static bool wasIdle = false;
		if (wasIdle)
			filtered = true;
		else
		{
			printf ("(Future idle calls will not be displayed!)\n");
			wasIdle = true;
		}
	}

	if (!filtered)
		printf ("PLUG> HostCallback (opcode %d)\n index = %d, value = %p, ptr = %p, opt = %f\n", opcode, index, FromVstPtr<void> (value), ptr, opt);

	switch (opcode)
	{
		case audioMasterVersion :
			result = kVstVersion;
			break;
	}

	return result;
}



int main (int argc, char* argv[])
{
	const char* fileName = "again.dll";
	//const char* fileName = "adelay.dll";
	//const char* fileName = "surrounddelay.dll";
	//const char* fileName = "vstxsynth.dll";
	//const char* fileName = "drawtest.dll";

	if (argc > 1)
		fileName = argv[1];

	printf ("HOST> Load library...\n");
	PluginLoader loader;
	if (!loader.loadLibrary (fileName))
	{
		printf ("Failed to load VST Plugin library!\n");
		return -1;
	}

	PluginEntryProc mainEntry = loader.getMainEntry ();
	if (!mainEntry)
	{
		printf ("VST Plugin main entry not found!\n");
		return -1;
	}

	printf ("HOST> Create effect...\n");
	AEffect* effect = mainEntry (HostCallback);
	if (!effect)
	{
		printf ("Failed to create effect instance!\n");
		return -1;
	}

	printf ("HOST> Init sequence...\n");
	effect->dispatcher (effect, effOpen, 0, 0, 0, 0);
	effect->dispatcher (effect, effSetSampleRate, 0, 0, 0, kSampleRate);
	effect->dispatcher (effect, effSetBlockSize, 0, kBlockSize, 0, 0);

	checkEffectProperties (effect);
	checkEffectProcessing (effect);
//	checkEffectEditor (effect);

	printf ("HOST> Close effect...\n");
	effect->dispatcher (effect, effClose, 0, 0, 0, 0);
	return 0;
}


Vst2Plugin* Vst2Plugin::loadVst2Plugin(FilePath path)
{
	PluginLoader loader;
	if (!loader.loadLibrary(path))
	{
	    // FIXME throw
	}       
	PluginEntryProc mainEntry = loader.getMainEntry();
	if (!mainEntry)
	{
	    // FIXME throw
	}       
	AEffect* effect = mainEntry(HostCallback);
	if (!effect)
	{
	    // FIXME throw
	}

    // FIXME
    return NULL;
}


} // namespace
} // namespace      







/////////////////////








              
