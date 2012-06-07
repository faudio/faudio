/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef  _SCLAUDIOX_PROCESSOR_PLUGIN
#define  _SCLAUDIOX_PROCESSOR_PLUGIN

#include "sclaudiox/processor.h"

namespace doremir {
namespace scl {

class AudioPlugin;
    

// =============================================================================

/**
    Description of an audio plugin or processor.     
 */
class PluginAudioProcessorDescription : public AudioProcessorDescription
{
public:    
    /** 
        Returns the plug-in object 
      */
    virtual AudioPlugin* plugin() = 0;
};


// =============================================================================

/**
    An AudioProcessor delegating to an audio plugin.     
 */
class PluginAudioProcessor : public AudioProcessor
{
public:   
    /** 
        Returns the plug-in object 
     */
    virtual AudioPlugin* plugin() = 0;
        
    /**
        Returns a pointer to the native plug-in instance.
     */
    virtual void* nativePluginInstance() = 0;
};


// =============================================================================

/**
    Provides a uniform interface to various audio plug-in architectures such as AU, VST, Ladspa, LV2 etc.  
    
    Each instance of this class corresponds to a named plug-in such as a synth, delay or reverb effect.
    Plug-ins can typically be loaded several times, each instance corresponding to an AudioProcessor. The
    createProcessor() method provides new instances.
 */
class AudioPlugin : public Resource
{
public:                    
    /** 
        Creates an description of this plugin. 
        
        This method can be used to retreive information about the plugin without necessarily loading a
        full instance. Some architectures will load instances at on demand to provide descriptions and
        then cache the information.
    */
    virtual PluginAudioProcessorDescription* description() = 0;
    
    /** 
        Creates an instance of this plugin.
      */
    virtual PluginAudioProcessor* createProcessor() = 0;
                      
    /** 
        Returns a pointer to the native plug-in object. 
      */
    virtual void* nativePlugin() = 0;
};



} // namespace
} // namespace

#endif
