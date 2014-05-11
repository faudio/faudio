
#ifdef __cplusplus
#include "../../external/vst/pluginterfaces/vst2.x/aeffectx.h"
#else
typedef struct _AEffect * AEffect;
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
// void processMidi(AEffect *plugin, VstEvents *events);

#ifdef __cplusplus
}
#endif
