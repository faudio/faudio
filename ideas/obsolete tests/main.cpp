/*
 *  main.cc
 *  ScoreCleanerAudio
 *
 *  Created by Hans Höglund on 2011-09-29.
 *  Copyright 2011 DoReMIR http://www.doremir.com/. All rights reserved.
 *
 */

#include <iostream>
#include <boost/thread.hpp>

//#include <mach-o/dyld.h>

#include "sclaudiox.h"
#include "sclaudio.h"

using namespace std;
using namespace scorecleaner;


#define SCL_TEST_SOUNDFONT_PATH \
  "/Users/hans/Documents/Kod/doremir/modus/app/resources/soundfonts/gs_fluidsynth_1.43.sf2"

//#include "tcore.h"
//#include "taudio.h"
//#include "tmidi.h"
//#include "tsignal.h"
//#include "tcontrol.h"
//#include "tprocessors.h"
//#include "tfuture.h"
#include "tstream.h"
//#include "tsclaudio.h"
//#include "tutilities.h"


/*
void runBasicTests()
{

//    testErrors();

    testString();
    testList();

    testAudioSystem();
    
    testMidiSystem();
    testConvertMidi();
    testPortmidi();
    
//    testControl();
//    testControlStringInt();
//    testControlAccessors();
//    testControlSubrange();
//    testControlThreads();
    
    testSignal();
    testSignalOperations();
    testSignalMap();
    testSignalPerformance();
    
//    testDeclareControls();

};

void runStreamTests()
{
    testBasicScheduling();
    testActionScheduler();
    testRepetition();
    
    testInterrupts();
    testGroupInterrupts();
    
    testMidiOutputCompleteness();
//    testMidiInterruption();
//    testMidiInterruption2();

    testMidiOutputChannels();

    testMidiInput(10);
    testMidiOutput(50);
}

void runApiTests()
{
    testListReturn();
    testRecursiveListReturn();
    testListPass();
    testRecursiveListPass();
}

*/

//void checkPath()
//{
//    char path[1024];
//    uint32_t size = sizeof(path);
//    if (_NSGetExecutablePath(path, &size) == 0)
//        printf("executable path is %s\n", path);
//    else
//        printf("buffer too small; need size %u\n", size);
//}

void testDeviceNameEncoding()
{
    Pm_Initialize();
    Pa_Initialize();

    int numMidiDevices = Pm_CountDevices();
    int numAudioDevices = Pa_GetDeviceCount();
    
    cout << "\n--------\n";

    for (int i = 0; i < numMidiDevices; ++i)
    {
        const char* name = Pm_GetDeviceInfo(i)->name;
        cout << name << "\n    ";
        
        int j = 0;
        char c;
        int c2;
        do
        {
            c = name[j++];
            c2 = c;
            cout << c2 << " ";
        } while (c != 0);
        cout << "\n";
    }

    cout << "--------\n";
    
    for (int i = 0; i < numAudioDevices; ++i)
    {
        const char* name = Pa_GetDeviceInfo(i)->name;
        cout << name << "\n    ";
        
        int j = 0;
        char c;
        int c2;
        do
        {
            c = name[j++];
            c2 = c;
            cout << c2 << " ";
        } while (c != 0);
        cout << "\n";
    }
    
    cout << "--------\n";

    {
        const char* name = "Hans Höglund";
        cout << name << "\n    ";
        
        int j = 0;
        char c;
        int c2;
        do
        {
            c = name[j++];
            c2 = c;
            cout << c2 << " ";
        } while (c != 0);
        cout << "\n";
    
    }

    cout << "--------\n";

    
    Pm_Terminate();
    Pa_Terminate();
}




int main(int argc, int* argv) 
{
//    testDeviceNameEncoding();

//    testDefaultMidiDevices();

//    testConnectDisconnect();
//    runBasicTests();
    
//    openStream();
//    runStreamTests();
//    freeStream();

//    openAudioStream();
//    testStartStopAudio();
    testErrorHandler();
//    freeAudioStream();

//    runApiTests();




    cout << "Finished tests\n";    
    return 0;
}

