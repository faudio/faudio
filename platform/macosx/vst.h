
#ifdef __cplusplus
#include "../../external/vst/pluginterfaces/vst2.x/aeffectx.h"
#else
typedef struct _AEffect AEffect;
typedef struct _VstEvent VstEvent;
typedef struct _VstEvents VstEvents;

struct _AEffect
{
	int32_t magic;			///< must be #kEffectMagic ('VstP')
	ptr_t   dispatcher;
	ptr_t   process;
	ptr_t   setParameter;
	ptr_t   getParameter;
	int32_t numPrograms;    ///< number of programs
	int32_t numParams;		///< all programs are assumed to have numParams parameters
	int32_t numInputs;		///< number of audio inputs
	int32_t numOutputs;	    ///< number of audio outputs
};

#endif

#ifdef __cplusplus
extern "C" {
#endif

AEffect* loadPlugin(char* pluginPath);
int initPlugin(AEffect *plugin);
void resumePlugin(AEffect *plugin);
void suspendPlugin(AEffect *plugin);
bool canPluginDo(AEffect *plugin, char *canDoString);
void processAudio(AEffect *plugin, float **inputs, float **outputs, long numFrames);
void silenceChannel(float **channelData, int numChannels, long numFrames);
bool openPlugin(AEffect *plugin, void*);
void processMidi(AEffect *plugin, VstEvents *events);
// void processMidi(AEffect *plugin, VstEvents *events);

#ifdef __cplusplus
}
#endif
