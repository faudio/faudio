
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

typedef int32_t VstInt32;
typedef intptr_t VstIntPtr;
#define DECLARE_VST_DEPRECATED(T) T

struct _VstEvent
{
	VstInt32 type;			///< @see VstEventTypes
	VstInt32 byteSize;		///< size of this event, excl. type and byteSize
	VstInt32 deltaFrames;	///< sample frames related to the current block start sample position
	VstInt32 flags;			///< generic flags, none defined yet

	char data[16];			///< data size may vary, depending on event type
};

struct _VstMidiEvent
{
	VstInt32 type;			///< #kVstMidiType
	VstInt32 byteSize;		///< sizeof (VstMidiEvent)
	VstInt32 deltaFrames;	///< sample frames related to the current block start sample position
	VstInt32 flags;			///< @see VstMidiEventFlags
	VstInt32 noteLength;	///< (in sample frames) of entire note, if available, else 0
	VstInt32 noteOffset;	///< offset (in sample frames) into note from note start if available, else 0
	char midiData[4];		///< 1 to 3 MIDI bytes; midiData[3] is reserved (zero)
	char detune;			///< -64 to +63 cents; for scales other than 'well-tempered' ('microtuning')
	char noteOffVelocity;	///< Note Off Velocity [0, 127]
	char reserved1;			///< zero (Reserved for future use)
	char reserved2;			///< zero (Reserved for future use)
};
typedef struct _VstMidiEvent VstMidiEvent;


enum VstEventTypes
{
	kVstMidiType = 1,		///< MIDI event  @see VstMidiEvent
	DECLARE_VST_DEPRECATED (kVstAudioType),		///< \deprecated unused event type
	DECLARE_VST_DEPRECATED (kVstVideoType),		///< \deprecated unused event type
	DECLARE_VST_DEPRECATED (kVstParameterType),	///< \deprecated unused event type
	DECLARE_VST_DEPRECATED (kVstTriggerType),	///< \deprecated unused event type
	kVstSysExType			///< MIDI system exclusive  @see VstMidiSysexEvent
};

struct _VstEvents
{
	VstInt32 numEvents;		///< number of Events in array
	VstIntPtr reserved;		///< zero (Reserved for future use)
	VstEvent* events[2];	///< event pointer array, variable size
};
#endif

#ifdef __cplusplus
extern "C" {
#endif

AEffect* loadPlugin(char* pluginPath);
int initPlugin(AEffect *plugin);
int setPluginParams(AEffect *plugin, double sr, int vs);
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
