/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef  _SCLAUDIOX
#define  _SCLAUDIOX

/**
    \defgroup sclaudiox C++ API
 */

#include "sclaudiox/core.h"

#include "sclaudiox/device/audio.h"
#include "sclaudiox/device/midi.h"

#include "sclaudiox/scheduling/future.h"
#include "sclaudiox/scheduling/action.h"
#include "sclaudiox/scheduling/message.h"
#include "sclaudiox/scheduling/dispatching.h"
#include "sclaudiox/scheduling/realtime/action.h"
#include "sclaudiox/scheduling/realtime/audio.h"
#include "sclaudiox/scheduling/realtime/midi.h"

#include "sclaudiox/stream.h"
#include "sclaudiox/stream/device/audio_only.h"
#include "sclaudiox/stream/device/midi_only.h"
#include "sclaudiox/stream/device/audio_midi.h"

#include "sclaudiox/processor/plugin/au.h"
#include "sclaudiox/processor/plugin/vst2.h"
#include "sclaudiox/processor/synth/fluid.h"

#endif