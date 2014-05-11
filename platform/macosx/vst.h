
AEffect* loadPlugin(char* pluginPath);
int initPlugin(AEffect *plugin);
void resumePlugin(AEffect *plugin);
void suspendPlugin(AEffect *plugin);
bool canPluginDo(AEffect *plugin, char *canDoString);
void processAudio(AEffect *plugin, float **inputs, float **outputs, long numFrames);
void silenceChannel(float **channelData, int numChannels, long numFrames);
void processMidi(AEffect *plugin, VstEvents *events);
