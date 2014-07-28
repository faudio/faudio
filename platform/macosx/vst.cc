
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

/*
    Based on:
        http://teragonaudio.com/article/How-to-make-your-own-VST-host.html
*/

#include <CoreFoundation/CoreFoundation.h>
#include "../../external/vst/pluginterfaces/vst2.x/aeffectx.h"

// TODO use proper error reporting etc



#include "vst.h"
extern "C" {

void vst_log(const char* msg);
void vst_log_i(const char* msg, long n);


// Main host callback
VstIntPtr VSTCALLBACK hostCallback(
    AEffect *effect, 
    VstInt32 opcode,
    VstInt32 index, 
    VstInt32 value, 
    void *ptr, 
    float opt
    );
}

// Plugin's entry point

typedef AEffect *(*vstPluginFuncPtr)(audioMasterCallback host);

// Plugin's dispatcher function
typedef VstIntPtr (*dispatcherFuncPtr)(AEffect *effect, VstInt32 opCode,
  VstInt32 index, VstInt32 value, void *ptr, float opt);

// Plugin's getParameter() method
typedef float (*getParameterFuncPtr)(AEffect *effect, VstInt32 index);

// Plugin's setParameter() method
typedef void (*setParameterFuncPtr)(AEffect *effect, VstInt32 index, float value);

// Plugin's processEvents() method
typedef VstInt32 (*processEventsFuncPtr)(VstEvents *events);

// Plugin's process() method
typedef void (*processFuncPtr)(AEffect *effect, float **inputs,
  float **outputs, VstInt32 sampleFrames);





// XXX Callback used by plug-ins to interact with the host
VstIntPtr VSTCALLBACK hostCallback(
    AEffect *effect, 
    VstInt32 opcode,
    VstInt32 index, 
    VstInt32 value, 
    void *ptr, 
    float opt
    )
{
    // printf("-----------> Plug-in called host, opcode: %d\n", opcode);

    switch(opcode) {
      case audioMasterVersion:
        return 2400; // TODO get from vst API or sanity check

      // case audioMasterIdle:
        // effect->dispatcher(effect, effEditIdle, 0, 0, 0, 0);
      // TODO Handle other opcodes...

      // case audioMasterGetVendorString:
      //   return (VstIntPtr) "com.doremir";
      // case audioMasterGetProductString:
      //   return (VstIntPtr) "Faudio";
      // case audioMasterGetVendorVersion:
      //   return 0;
      // case audioMasterCanDo:
      //   return 0;
      
      case audioMasterUpdateDisplay: {
          vst_log("Plugin requested update display\n");
          return 0;          
      }

      // case audioMasterPinConnected:
        // return 0;
        
      // case audioMasterGetLanguage:
        // effect->dispatcher(effect, kVstLangEnglish, 0, 0, 0, 0);

      default:
        // TODO count and limit
        // vst_log("                Plugin requested unknown operation with opcode %d\n", opcode);
      vst_log_i("                Plugin requested unknown operation %d\n", opcode);
        break;
    }


    // TODO
    return NULL;
}





AEffect* loadPlugin(char* pluginPath) {
  AEffect *plugin = NULL;
  // audioMasterCallback hostCallbackFuncPtr = hostCallback;
  // char *pluginPath = (char*)"/wherever/the/plugin/is/located.vst";

  // Create a path to the bundle
  CFStringRef pluginPathStringRef = CFStringCreateWithCString(NULL,
    pluginPath, kCFStringEncodingASCII);
  CFURLRef bundleUrl = CFURLCreateWithFileSystemPath(kCFAllocatorDefault,
    pluginPathStringRef, kCFURLPOSIXPathStyle, true);
  if(bundleUrl == NULL) {
    vst_log("Couldn't make URL reference for plugin\n");
    return NULL;
  }

  // Open the bundle
  CFBundleRef bundle;
  bundle = CFBundleCreate(kCFAllocatorDefault, bundleUrl);
  if(bundle == NULL) {
    vst_log("Couldn't create bundle reference\n");
    CFRelease(pluginPathStringRef);
    CFRelease(bundleUrl);
    return NULL;
  }

  vstPluginFuncPtr mainEntryPoint = NULL;
  mainEntryPoint = (vstPluginFuncPtr)CFBundleGetFunctionPointerForName(bundle,
    CFSTR("VSTPluginMain"));
  // VST plugins previous to the 2.4 SDK used main_macho for the entry point name
  if(mainEntryPoint == NULL) {
    mainEntryPoint = (vstPluginFuncPtr)CFBundleGetFunctionPointerForName(bundle,
      CFSTR("main_macho"));
  }

  if(mainEntryPoint == NULL) {
    vst_log("Couldn't get a pointer to plugin's main()\n");
    CFBundleUnloadExecutable(bundle);
    CFRelease(bundle);
    return NULL;
  }

  plugin = mainEntryPoint(hostCallback);
  if(plugin == NULL) {
    vst_log("Plugin's main() returns null\n");
    CFBundleUnloadExecutable(bundle);
    CFRelease(bundle);
    return NULL;
  }

  // Clean up
  CFRelease(pluginPathStringRef);
  CFRelease(bundleUrl);

  return plugin;
}



int initPlugin(AEffect *plugin) {
  // Check plugin's magic number
  // If incorrect, then the file either was not loaded properly, is not a
  // real VST plugin, or is otherwise corrupt.
  if(plugin->magic != kEffectMagic) {
    vst_log("Plugin's magic number is bad\n");
    return -1;
  }

  // Create dispatcher handle
  dispatcherFuncPtr dispatcher = (dispatcherFuncPtr)(plugin->dispatcher);

  // Set up plugin callback functions
  // plugin->getParameter = (getParameterFuncPtr)plugin->getParameter;
  // plugin->processReplacing = (processFuncPtr)plugin->processReplacing;
  // plugin->setParameter = (setParameterFuncPtr)plugin->setParameter;

  dispatcher(plugin, effOpen, 0, 0, NULL, 0.0f);

  // ?
  // resume();
  return 0;
}

int setPluginParams(AEffect *plugin, double sr, int vs) {
    dispatcherFuncPtr dispatcher = (dispatcherFuncPtr)(plugin->dispatcher);

    float sampleRate = sr;
    dispatcher(plugin, effSetSampleRate, 0, 0, NULL, sampleRate);
    int blocksize = vs;
    dispatcher(plugin, effSetBlockSize, 0, blocksize, NULL, 0.0f);
    return 0;
}

void resumePlugin(AEffect *plugin) {
  dispatcherFuncPtr dispatcher = (dispatcherFuncPtr)(plugin->dispatcher);
  dispatcher(plugin, effMainsChanged, 0, 1, NULL, 0.0f);
}

void suspendPlugin(AEffect *plugin) {
  dispatcherFuncPtr dispatcher = (dispatcherFuncPtr)(plugin->dispatcher);
  dispatcher(plugin, effMainsChanged, 0, 0, NULL, 0.0f);
}

bool canPluginDo(AEffect *plugin, char *canDoString) {
  dispatcherFuncPtr dispatcher = (dispatcherFuncPtr)(plugin->dispatcher);
  return (dispatcher(plugin, effCanDo, 0, 0, (void*)canDoString, 0.0f) > 0);
}

// TODO these are for GUI/editor... rename to make more clear
/*
    According to
        http://www.juce.com/forum/topic/mac-64-bit
        
    effEditOpen:
        Handle is HWND (Windows) or WindowRef (Carbon?)/NSView (Cocoa?)
*/

bool openPlugin(AEffect *plugin, void* handle) {
  dispatcherFuncPtr dispatcher = (dispatcherFuncPtr)(plugin->dispatcher);

  bool plugin_has_cocoa = (dispatcher(plugin,effCanDo,0,0,(void*)"hasCockosViewAsConfig",0.0f) & 0xffff0000) == 0xbeef0000;
  if (!plugin_has_cocoa) {
      vst_log("This plugin does not support Cocoa, ignoring open command\n");
      return 0;
  } else {
      vst_log("OK, opening Window\n");
      return (dispatcher(plugin, effEditOpen, 0, 0, handle, 0.0f) > 0);
  }
}
bool closePlugin(AEffect *plugin) {
  dispatcherFuncPtr dispatcher = (dispatcherFuncPtr)(plugin->dispatcher);
  return (dispatcher(plugin, effEditClose, 0, 0, NULL, 0.0f) > 0);
}

void processAudio(AEffect *plugin, float **inputs, float **outputs,
  long numFrames) {
  // Note: If you are processing an instrument, you should probably zero
  // out the input channels first to avoid any accidental noise. If you
  // are processing an effect, you should probably zero the values in the
  // output channels. See the silenceChannel() function below.
  plugin->processReplacing(plugin, inputs, outputs, numFrames);
}

void silenceChannel(float **channelData, int numChannels, long numFrames) {
  for(int channel = 0; channel < numChannels; ++channel) {
    for(long frame = 0; frame < numFrames; ++frame) {
      channelData[channel][frame] = 0.0f;
    }
  }
}

void processMidi(AEffect *plugin, VstEvents *events) {
  dispatcherFuncPtr dispatcher = (dispatcherFuncPtr)(plugin->dispatcher);
  dispatcher(plugin, effProcessEvents, 0, 0, events, 0.0f);
}





