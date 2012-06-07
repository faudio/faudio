/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
/**
    @file   sclaudiox/stream.cc
    @author Hans Hoglund
 */

#include "sclaudiox/stream.h"

#include "sclaudiox/stream/device/audio_only.h"
#include "sclaudiox/stream/device/midi_only.h"
#include "sclaudiox/stream/device/audio_midi.h"

namespace doremir {
namespace scl {

Stream* DeviceStream::open(MidiDevice* midiInput,
                         MidiDevice* midiOutput,
                         AudioDevice* audioInput,
                         AudioDevice* audioOutput,
                         AudioProcessor* audioProcessor,
                         DeviceStreamOptions options)
{
    bool hasMidi = midiInput != NULL || midiOutput != NULL;

    bool hasAudio = (audioInput != NULL || audioOutput != NULL) 
                        && 
                    audioProcessor != NULL;

    if (hasMidi && !hasAudio)
    {
        return new MidiOnlyDeviceStream(midiInput, midiOutput, options);
    }
    else if (hasAudio && !hasMidi)
    {
        return new AudioOnlyDeviceStream(audioInput, audioOutput, 
            audioProcessor, options);
    }
    else if (hasAudio && hasMidi)
    {
        return new AudioMidiDeviceStream(midiInput, midiOutput, 
            audioInput, audioOutput, audioProcessor, options);
    }
    else
    {
        return NULL;
    }
}


// =======================================================================================
    
} // namespace
} // namespace
